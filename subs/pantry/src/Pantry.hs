{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Pantry
  ( -- * Congiruation
    PantryConfig
  , HackageSecurityConfig (..)
  , defaultHackageSecurityConfig
  , HasPantryConfig (..)
  , withPantryConfig

    -- * Types
  , StaticSHA256
  , CabalFileInfo (..)
  , Revision (..)
  , FileSize (..)
  , PackageLocation (..)
  , Archive (..)
  , ArchiveLocation (..)
  , Repo (..)
  , RepoType (..)
  , RelFilePath (..)
  , PackageLocationOrPath (..)
  , ResolvedDir (..)
  , resolvedAbsolute
  , PackageIdentifierRevision (..)
  , PackageName
  , Version
  , PackageIdentifier (..)
  , FlagName
  , TreeKey (..)
  , BlobKey (..)

    -- ** Raw package locations
  , RawPackageLocation
  , RawPackageLocationOrPath (..)
  , unRawPackageLocation
  , mkRawPackageLocation
  , unRawPackageLocationOrPath
  , completePackageLocation
  , resolveDirWithRel

    -- ** Cabal helpers
  , parsePackageIdentifier
  , parsePackageName
  , parseFlagName
  , parseVersion
  , displayC
  , CabalString (..)
  , toCabalStringMap
  , unCabalStringMap

    -- ** Parsers
  , parsePackageIdentifierRevision

    -- * Package location
  , parseCabalFile
  , parseCabalFileOrPath
  , getPackageLocationIdent
  , getPackageLocationTreeKey

    -- * Hackage index
  , updateHackageIndex
  , hackageIndexTarballL
  , getLatestHackageVersion

    -- * FIXME legacy from Stack, to be updated
  , loadFromIndex
  , getPackageVersions
  , fetchPackages
  , unpackPackageLocation
  ) where

import RIO
import RIO.FilePath (takeDirectory)
import qualified RIO.Map as Map
import qualified RIO.Text as T
import qualified Data.Map.Strict as Map (mapKeysMonotonic)
import Pantry.StaticSHA256
import Pantry.Storage
import Pantry.Tree
import Pantry.Types
import Pantry.Hackage
import Path (Path, Abs, File, parent, toFilePath, Dir, mkRelFile, (</>))
import Path.IO (resolveDir)
import Distribution.PackageDescription (GenericPackageDescription, FlagName)
import Distribution.PackageDescription.Parsec

withPantryConfig
  :: HasLogFunc env
  => Path Abs Dir -- ^ pantry root
  -> HackageSecurityConfig
  -> (PantryConfig -> RIO env a)
  -> RIO env a
withPantryConfig root hsc inner = do
  env <- ask
  -- Silence persistent's logging output, which is really noisy
  runRIO (mempty :: LogFunc) $ initStorage (root </> $(mkRelFile "pantry.sqlite3")) $ \storage -> runRIO env $ do
    ur <- newMVar True
    inner PantryConfig
      { pcHackageSecurity = hsc
      , pcRootDir = root
      , pcStorage = storage
      , pcUpdateRef = ur
      }

defaultHackageSecurityConfig :: HackageSecurityConfig
defaultHackageSecurityConfig = HackageSecurityConfig
  { hscKeyIds =
      [ "0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d"
      , "1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42"
      , "280b10153a522681163658cb49f632cde3f38d768b736ddbc901d99a1a772833"
      , "2a96b1889dc221c17296fcc2bb34b908ca9734376f0f361660200935916ef201"
      , "2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3"
      , "51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921"
      , "772e9f4c7db33d251d5c6e357199c819e569d130857dc225549b40845ff0890d"
      , "aa315286e6ad281ad61182235533c41e806e5a787e0b6d1e7eef3f09d137d2e9"
      , "fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0"
      ]
  , hscKeyThreshold = 3
  , hscDownloadPrefix = "https://hackage.haskell.org/"
  }

lookupPackageIdentifierExact
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> CabalFileInfo
  -> RIO env (Maybe ByteString)
lookupPackageIdentifierExact name version cfi =
  withStorage $ loadHackageCabalFile name version cfi

loadFromIndex
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> CabalFileInfo
  -> RIO env (Either () ByteString)
loadFromIndex name version cfi = do
  mres <- lookupPackageIdentifierExact name version cfi
  case mres of
    Just bs -> return $ Right bs
    -- Update the cache and try again
    Nothing -> do
      updated <- updateHackageIndex $ Just $
                "Didn't see " <>
                display (PackageIdentifierRevision name version cfi) <>
                " in your package indices.\n" <>
                "Updating and trying again."
      if updated
        then loadFromIndex name version cfi
        else do
            pure $ Left ()
            {- FIXME
            fuzzy <- fuzzyLookupCandidates name version cfi
            let suggestions = case fuzzy of
                    FRNameNotFound Nothing -> ""
                    FRNameNotFound (Just cs) ->
                          "Perhaps you meant " <> orSeparated cs <> "?"
                    FRVersionNotFound cs -> "Possible candidates: " <>
                      commaSeparated (NE.map packageIdentifierText cs)
                      <> "."
                    FRRevisionNotFound cs ->
                      "The specified revision was not found.\nPossible candidates: " <>
                      commaSeparated (NE.map (T.pack . packageIdentifierRevisionString) cs)
                      <> "."
            pure (False, Left $ UnknownPackageIdentifiers
                                  (Set.singleton (name, version, cfi))
                                  suggestions)

orSeparated :: NonEmpty Text -> Text
orSeparated xs
  | NE.length xs == 1 = NE.head xs
  | NE.length xs == 2 = NE.head xs <> " or " <> NE.last xs
  | otherwise = T.intercalate ", " (NE.init xs) <> ", or " <> NE.last xs

commaSeparated :: NonEmpty Text -> Text
commaSeparated = fold . NE.intersperse ", "

data FuzzyResults
  = FRNameNotFound !(Maybe (NonEmpty Text))
  | FRVersionNotFound !(NonEmpty (PackageName, Version))
  | FRRevisionNotFound !(NonEmpty (PackageName, Version, CabalFileInfo))

-- | Given package identifier and package caches, return list of packages
-- with the same name and the same two first version number components found
-- in the caches.
fuzzyLookupCandidates
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> CabalFileInfo
  -> RIO env FuzzyResults
fuzzyLookupCandidates name ver _rev =
  case Map.lookup name caches of
    Nothing -> FRNameNotFound $ typoCorrectionCandidates name (PackageCache caches)
    Just m ->
      case Map.lookup ver m of
        Nothing ->
          case NE.nonEmpty $ filter sameMajor $ Map.keys m of
            Just vers -> FRVersionNotFound $ NE.map (PackageIdentifier name) vers
            Nothing ->
              case NE.nonEmpty $ Map.keys m of
                Nothing -> error "fuzzyLookupCandidates: no versions"
                Just vers -> FRVersionNotFound $ NE.map (PackageIdentifier name) vers
        Just (_index, _mpd, revisions) ->
          let hashes = concatMap fst $ NE.toList revisions
              pirs = map (PackageIdentifierRevision (PackageIdentifier name ver) . CFIHash Nothing) hashes
           in case NE.nonEmpty pirs of
                Nothing -> error "fuzzyLookupCandidates: no revisions"
                Just pirs' -> FRRevisionNotFound pirs'
  where
    sameMajor v = toMajorVersion v == toMajorVersion ver

-- | Try to come up with typo corrections for given package identifier using
-- package caches. This should be called before giving up, i.e. when
-- 'fuzzyLookupCandidates' cannot return anything.
typoCorrectionCandidates
  :: PackageName
  -> Maybe (NonEmpty Text)
typoCorrectionCandidates name' =
  let name = packageNameText name'
  in  NE.nonEmpty
    . take 10
    . map snd
    . filter (\(distance, _) -> distance < 4)
    . map (\k -> (damerauLevenshtein name (packageNameText k), packageNameText k))
    . Map.keys
    $ cache
-}

-- | Returns the versions of the package available on Hackage.
getPackageVersions
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName -- ^ package name
  -> RIO env (Map Version (Map Revision BlobKey))
getPackageVersions = withStorage . loadHackagePackageVersions

-- | Returns the latest version of the given package available from
-- Hackage.
getLatestHackageVersion
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName -- ^ package name
  -> RIO env (Maybe PackageIdentifierRevision)
getLatestHackageVersion name =
  ((fmap fst . Map.maxViewWithKey) >=> go) <$> getPackageVersions name
  where
    go (version, m) = do
      (_rev, BlobKey sha size) <- fst <$> Map.maxViewWithKey m
      pure $ PackageIdentifierRevision name version $ CFIHash sha $ Just size

fetchPackages
  :: (HasPantryConfig env, HasLogFunc env, Foldable f)
  => f PackageIdentifier
  -> RIO env ()
fetchPackages _ = undefined

unpackPackageLocation
  :: (HasPantryConfig env, HasLogFunc env)
  => FilePath -- ^ unpack directory
  -> PackageLocation
  -> RIO env ()
unpackPackageLocation fp loc = do
  tree <- loadPackageLocation loc
  unpackTree fp tree

-- | Ignores all warnings
parseCabalFile
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env GenericPackageDescription
parseCabalFile loc = do
  logDebug $ "Parsing cabal file for " <> display loc
  bs <- loadCabalFile loc
  case runParseResult $ parseGenericPackageDescription bs of
    (warnings, Left (mversion, errs)) -> throwM $ InvalidCabalFile (PLRemote loc) mversion errs warnings
    (_warnings, Right gpd) -> pure gpd

-- | Same as 'parseCabalFile', but takes a 'PackageLocationOrPath'.
parseCabalFileOrPath
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocationOrPath
  -> RIO env GenericPackageDescription
parseCabalFileOrPath (PLRemote loc) = parseCabalFile loc
parseCabalFileOrPath (PLFilePath rfp) = undefined

loadCabalFile
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env ByteString
loadCabalFile (PLHackage pir mtree) = getHackageCabalFile pir
{- FIXME this is relatively inefficient
loadCabalFile loc = do
  tree <- loadPackageLocation loc
  mbs <- withStorage $ do
    (_sfp, TreeEntry key _ft) <- findCabalFile loc tree
    loadBlob key
  case mbs of
    Just bs -> pure bs
    -- FIXME what to do on Nothing? perhaps download the PackageLocation again?
-}

loadPackageLocation
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env Tree
loadPackageLocation (PLHackage pir mtree) =
  case mtree of
    Nothing -> snd <$> getHackageTarball pir

toCabalStringMap :: Map a v -> Map (CabalString a) v
toCabalStringMap = Map.mapKeysMonotonic CabalString -- FIXME why doesn't coerce work?

unCabalStringMap :: Map (CabalString a) v -> Map a v
unCabalStringMap = Map.mapKeysMonotonic unCabalString -- FIXME why doesn't coerce work?

-- | Convert a 'RawPackageLocation' into a list of 'PackageLocation's.
unRawPackageLocation :: RawPackageLocation -> [PackageLocation]
unRawPackageLocation (RPLHackage pir mtree) = [PLHackage pir mtree]

-- | Convert a 'PackageLocation' into a 'RawPackageLocation'.
mkRawPackageLocation :: PackageLocation -> RawPackageLocation
mkRawPackageLocation = undefined

-- | Convert a 'RawPackageLocationOrPath' into a list of 'PackageLocationOrPath's.
unRawPackageLocationOrPath
  :: MonadIO m
  => Path Abs File -- ^ configuration file to be used for resolving relative file paths
  -> RawPackageLocationOrPath
  -> m [PackageLocationOrPath]
unRawPackageLocationOrPath _ (RPLRemote rpl) =
  pure $ PLRemote <$> unRawPackageLocation rpl
unRawPackageLocationOrPath configFile (RPLFilePath fp) = do
  rfp <- resolveDirWithRel configFile fp
  pure [PLFilePath rfp]

resolveDirWithRel
  :: MonadIO m
  => Path Abs File -- ^ config file it was read from
  -> RelFilePath
  -> m ResolvedDir
resolveDirWithRel configFile (RelFilePath fp) = do
  absolute <- resolveDir (parent configFile) (T.unpack fp)
  pure ResolvedDir
    { resolvedRelative = fp
    , resolvedAbsoluteHack = toFilePath absolute
    }

-- | Fill in optional fields in a 'PackageLocation' for more reproducible builds.
completePackageLocation
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env PackageLocation
completePackageLocation = undefined

-- | Get the name of the package at the given location.
getPackageLocationIdent
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env PackageIdentifier
getPackageLocationIdent (PLHackage (PackageIdentifierRevision name version _) _) = pure $ PackageIdentifier name version

getPackageLocationTreeKey
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env TreeKey
getPackageLocationTreeKey = undefined
