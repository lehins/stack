{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}
-- Create a source distribution tarball
module Stack.SDist
    ( makeSDistTarball
    , checkSDistTarball
    , checkSDistTarballSink
    , SDistOpts (..)
    ) where

import qualified Data.Conduit.Tar as CTar
import qualified Conduit as C
import           Control.Applicative
import           Control.Concurrent.Execute (ActionContext(..), Concurrency(..))
import           Stack.Prelude
import           Control.Monad.Reader.Class (local)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Bits ((.&.))
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Char (toLower)
import           Data.Conduit.Zlib (WindowBits(..), compress, ungzip)
import           Data.Data (cast)
import           Data.List
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Clock.POSIX
import           Distribution.Package (Dependency (..))
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Check as Check
import qualified Distribution.PackageDescription.Parsec as Cabal
import           Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import qualified Distribution.Types.UnqualComponentName as Cabal
import           Distribution.Version (simplifyVersionRange, orLaterVersion, earlierVersion, hasUpperBound, hasLowerBound)
import           Lens.Micro (set)
import           Path
import           Path.IO hiding (getModificationTime, getPermissions, withSystemTempDir)
import           RIO.PrettyPrint
import           Stack.Build (mkBaseConfigOpts, build)
import           Stack.Build.Execute
import           Stack.Build.Installed
import           Stack.Build.Source (projectLocalPackages)
import           Stack.Package
import           Stack.SourceMap
import           Stack.Types.Build
import           Stack.Types.Config
import           Stack.Types.Package
import           Stack.Types.Runner
import           Stack.Types.SourceMap
import           Stack.Types.Version
import           System.Directory (getModificationTime, getPermissions)
import qualified System.FilePath as FP

-- | Special exception to throw when you want to fail because of bad results
-- of package check.

data SDistOpts = SDistOpts
  { sdoptsDirsToWorkWith :: [String]
  -- ^ Directories to package
  , sdoptsPvpBounds :: Maybe PvpBounds
  -- ^ PVP Bounds overrides
  , sdoptsIgnoreCheck :: Bool
  -- ^ Whether to ignore check of the package for common errors
  , sdoptsSign :: Bool
  -- ^ Whether to sign the package
  , sdoptsSignServerUrl :: String
  -- ^ The URL of the signature server
  , sdoptsBuildTarball :: Bool
  -- ^ Whether to build the tarball
  , sdoptsTarPath :: Maybe FilePath
  -- ^ Where to copy the tarball
  }

newtype CheckException
  = CheckException (NonEmpty Check.PackageCheck)
  deriving (Typeable)

instance Exception CheckException

instance Show CheckException where
  show (CheckException xs) =
    "Package check reported the following errors:\n" ++
    (intercalate "\n" . fmap show . NE.toList $ xs)

-- | Given the path to a local package, creates its source distribution tarball.
--
-- While this yields a 'FilePath', the name of the tarball, this
-- tarball is handled according to the supplied sink.
makeSDistTarball ::
       HasEnvConfig env
    => Maybe PvpBounds -- ^ Override Config value
    -> Path Abs Dir -- ^ Path to local package
    -> (Path Rel File -> ConduitM ByteString Void (ResourceT (RIO env)) a)
    -> RIO env (a, Path Rel File, Maybe (PackageIdentifier, L.ByteString))
  -- ^ Filename, tarball contents, and option cabal file revision to upload
makeSDistTarball mpvpBounds pkgDir sinkSDist = do
    config <- view configL
    let PvpBounds pvpBounds asRevision =
            fromMaybe (configPvpBounds config) mpvpBounds
        tweakCabal = pvpBounds /= PvpBoundsNone
        pkgFp = toFilePath pkgDir
    lp <- readLocalPackage pkgDir
    sourceMap <- view $ envConfigL . to envConfigSourceMap
    logInfo $ "Getting file list for " <> fromString pkgFp
    -- Use cabal to get the list of fiels to be included into the sdist tarball
    (files, cabalfp) <- getSDistFileList lp
    logInfo $ "Building sdist tarball for " <> fromString pkgFp
    -- We're going to loop below and eventually find the cabal
    -- file. When we do, we'll upload this reference, if the
    -- mpvpBounds value indicates that we should be uploading a cabal
    -- file revision.
    cabalFileRevisionRef <- liftIO (newIORef Nothing)
    let getCabalFile fp
            -- This is a cabal file, we're going to tweak it, but only
            -- tweak it as a revision.
            | tweakCabal && asRevision = do
                lbsIdent <- getCabalLbs pvpBounds (Just 1) cabalfp sourceMap
                liftIO (writeIORef cabalFileRevisionRef (Just lbsIdent))
                content <- liftIO $ L.readFile fp
                return (Nothing, content)
            -- Same, except we'll include the cabal file in the
            -- original tarball upload.
            | tweakCabal = do
                (_ident, lbs) <- getCabalLbs pvpBounds Nothing cabalfp sourceMap
                curTime <- curModTime
                return (Just curTime, lbs)
            | otherwise = do
                content <- liftIO $ L.readFile fp
                return (Nothing, content)
        isCabalFp fp = toFilePath pkgDir FP.</> fp == toFilePath cabalfp
        pkgId = packageIdentifierString (packageIdentifier (lpPackage lp))
        yieldPackageFiles pkgDir fs =
            C.yieldMany fs .| CTar.tarFiles (Just pkgDir) (Just 1) Nothing .|
            C.mapC (mapLeft resetFileInfo)
        yieldCabalFile pkgDir fp = do
            (mmodTime, cabalFileContent) <- lift $ lift $ getCabalFile fp
            fi <- resetFileInfo <$> CTar.getFileInfo fp
            C.yield $
                Left $
                fi
                    { CTar.filePath = CTar.encodeFilePath (pkgId <> "/" <> FP.normalise fp)
                    , CTar.fileSize = fromIntegral (L.length cabalFileContent)
                    , CTar.fileModTime =
                          fromMaybe (CTar.fileModTime fi) mmodTime
                    }
            C.sourceLazy cabalFileContent .| C.mapC Right
        sourcePackage =
            case break isCabalFp files of
                (_, []) -> lift $ lift $ logError "Could not find a .cabal file"
                (filesBeforeCabal, cabalFile:filesAfterCabal) -> do
                    pkgDir <- CTar.makeDirectory pkgId
                    yieldPackageFiles pkgDir (filesBeforeCabal ++ filesAfterCabal)
                    yieldCabalFile pkgDir cabalFile
        tarByteStream =
            sourcePackage .| void CTar.tar .|
            compress (-1) (WindowBits 31) -- switch to zlib's default compression.
    tarName <- parseRelFile (pkgId FP.<.> "tar.gz")
    res <- C.runConduitRes $ tarByteStream .| sinkSDist tarName
    mcabalFileRevision <- liftIO (readIORef cabalFileRevisionRef)
    return (res, tarName, mcabalFileRevision)

-- | Get the PVP bounds-enabled version of the given cabal file
getCabalLbs :: HasEnvConfig env
            => PvpBoundsType
            -> Maybe Int -- ^ optional revision
            -> Path Abs File -- ^ cabal file
            -> SourceMap
            -> RIO env (PackageIdentifier, L.ByteString)
getCabalLbs pvpBounds mrev cabalfp sourceMap = do
    (gpdio, _name, cabalfp') <- loadCabalFilePath (parent cabalfp)
    gpd <- liftIO $ gpdio NoPrintWarnings
    unless (cabalfp == cabalfp')
      $ error $ "getCabalLbs: cabalfp /= cabalfp': " ++ show (cabalfp, cabalfp')
    installMap <- toInstallMap sourceMap
    (installedMap, _, _, _) <- getInstalled GetInstalledOpts
                                { getInstalledProfiling = False
                                , getInstalledHaddock = False
                                , getInstalledSymbols = False
                                }
                                installMap
    let internalPackages = Set.fromList $
          gpdPackageName gpd :
          map (Cabal.unqualComponentNameToPackageName . fst) (Cabal.condSubLibraries gpd)
        gpd' = gtraverseT (addBounds internalPackages installMap installedMap) gpd
        gpd'' =
          case mrev of
            Nothing -> gpd'
            Just rev -> gpd'
              { Cabal.packageDescription
               = (Cabal.packageDescription gpd')
                  { Cabal.customFieldsPD
                  = (("x-revision", show rev):)
                  $ filter (\(x, _) -> map toLower x /= "x-revision")
                  $ Cabal.customFieldsPD
                  $ Cabal.packageDescription gpd'
                  }
              }
        ident = Cabal.package $ Cabal.packageDescription gpd''
    -- Sanity rendering and reparsing the input, to ensure there are no
    -- cabal bugs, since there have been bugs here before, and currently
    -- are at the time of writing:
    --
    -- https://github.com/haskell/cabal/issues/1202
    -- https://github.com/haskell/cabal/issues/2353
    -- https://github.com/haskell/cabal/issues/4863 (current issue)
    let roundtripErrs =
          [ flow "Bug detected in Cabal library. ((parse . render . parse) === id) does not hold for the cabal file at"
          <+> pretty cabalfp
          , ""
          ]
        (_warnings, eres) = Cabal.runParseResult
                          $ Cabal.parseGenericPackageDescription
                          $ T.encodeUtf8
                          $ T.pack
                          $ showGenericPackageDescription gpd
    case eres of
      Right roundtripped
        | roundtripped == gpd -> return ()
        | otherwise -> do
            prettyWarn $ vsep $ roundtripErrs ++
              [ "This seems to be fixed in development versions of Cabal, but at time of writing, the fix is not in any released versions."
              , ""
              ,  "Please see this GitHub issue for status:" <+> style Url "https://github.com/commercialhaskell/stack/issues/3549"
              , ""
              , fillSep
                [ flow "If the issue is closed as resolved, then you may be able to fix this by upgrading to a newer version of stack via"
                , style Shell "stack upgrade"
                , flow "for latest stable version or"
                , style Shell "stack upgrade --git"
                , flow "for the latest development version."
                ]
              , ""
              , fillSep
                [ flow "If the issue is fixed, but updating doesn't solve the problem, please check if there are similar open issues, and if not, report a new issue to the stack issue tracker, at"
                , style Url "https://github.com/commercialhaskell/stack/issues/new"
                ]
              , ""
              , flow "If the issue is not fixed, feel free to leave a comment on it indicating that you would like it to be fixed."
              , ""
              ]
      Left (_version, errs) -> do
        prettyWarn $ vsep $ roundtripErrs ++
          [ flow "In particular, parsing the rendered cabal file is yielding a parse error.  Please check if there are already issues tracking this, and if not, please report new issues to the stack and cabal issue trackers, via"
          , bulletedList
            [ style Url "https://github.com/commercialhaskell/stack/issues/new"
            , style Url "https://github.com/haskell/cabal/issues/new"
            ]
          , flow $ "The parse error is: " ++ unlines (map show errs)
          , ""
          ]
    return
      ( ident
      , TLE.encodeUtf8 $ TL.pack $ showGenericPackageDescription gpd''
      )
  where
    addBounds :: Set PackageName -> InstallMap -> InstalledMap -> Dependency -> Dependency
    addBounds internalPackages installMap installedMap dep@(Dependency name range) =
      if name `Set.member` internalPackages
        then dep
        else case foundVersion of
          Nothing -> dep
          Just version -> Dependency name $ simplifyVersionRange
            $ (if toAddUpper && not (hasUpperBound range) then addUpper version else id)
            $ (if toAddLower && not (hasLowerBound range) then addLower version else id)
              range
      where
        foundVersion =
          case Map.lookup name installMap of
              Just (_, version) -> Just version
              Nothing ->
                  case Map.lookup name installedMap of
                      Just (_, installed) -> Just (installedVersion installed)
                      Nothing -> Nothing

    addUpper version = intersectVersionRanges
        (earlierVersion $ nextMajorVersion version)
    addLower version = intersectVersionRanges (orLaterVersion version)

    (toAddLower, toAddUpper) =
      case pvpBounds of
        PvpBoundsNone  -> (False, False)
        PvpBoundsUpper -> (False, True)
        PvpBoundsLower -> (True,  False)
        PvpBoundsBoth  -> (True,  True)

-- | Traverse a data type.
gtraverseT :: (Data a,Typeable b) => (Typeable b => b -> b) -> a -> a
gtraverseT f =
  gmapT (\x -> case cast x of
                 Nothing -> gtraverseT f x
                 Just b  -> fromMaybe x (cast (f b)))

-- | Read in a 'LocalPackage' config.  This makes some default decisions
-- about 'LocalPackage' fields that might not be appropriate for other
-- use-cases.
readLocalPackage :: HasEnvConfig env => Path Abs Dir -> RIO env LocalPackage
readLocalPackage pkgDir = do
    config  <- getDefaultPackageConfig
    (gpdio, _, cabalfp) <- loadCabalFilePath pkgDir
    gpd <- liftIO $ gpdio YesPrintWarnings
    let package = resolvePackage config gpd
    return LocalPackage
        { lpPackage = package
        , lpWanted = False -- HACK: makes it so that sdist output goes to a log instead of a file.
        , lpCabalFile = cabalfp
        -- NOTE: these aren't the 'correct values, but aren't used in
        -- the usage of this function in this module.
        , lpTestDeps = Map.empty
        , lpBenchDeps = Map.empty
        , lpTestBench = Nothing
        , lpBuildHaddocks = False
        , lpForceDirty = False
        , lpDirtyFiles = pure Nothing
        , lpNewBuildCaches = pure Map.empty
        , lpComponentFiles = pure Map.empty
        , lpComponents = Set.empty
        , lpUnbuildable = Set.empty
        }

-- | Returns a list of normalized paths, and the absolute path to the .cabal file.
getSDistFileList :: HasEnvConfig env => LocalPackage -> RIO env ([FilePath], Path Abs File)
getSDistFileList lp =
    withSystemTempDir (stackProgName <> "-sdist") $ \tmpdir -> do
        let bopts = defaultBuildOpts
        let boptsCli = defaultBuildOptsCLI
        baseConfigOpts <- mkBaseConfigOpts boptsCli
        locals <- projectLocalPackages
        withExecuteEnv bopts boptsCli baseConfigOpts locals
            [] [] [] -- provide empty list of globals. This is a hack around custom Setup.hs files
            $ \ee ->
            withSingleContext ac ee task Nothing (Just "sdist") $ \_package cabalfp _pkgDir cabal _announce _outputType -> do
                let outFile = toFilePath tmpdir FP.</> "source-files-list"
                cabal KeepTHLoading ["sdist", "--list-sources", outFile]
                contents <- liftIO (S.readFile outFile)
                let fileList = T.lines $ T.decodeUtf8With T.lenientDecode contents
                files <- normalizeTarballPaths (map (T.unpack . stripCR) fileList)
                return (files, cabalfp)
  where
    package = lpPackage lp
    ac = ActionContext Set.empty [] ConcurrencyAllowed
    task = Task
        { taskProvides = PackageIdentifier (packageName package) (packageVersion package)
        , taskType = TTLocalMutable lp
        , taskConfigOpts = TaskConfigOpts
            { tcoMissing = Set.empty
            , tcoOpts = \_ -> ConfigureOpts [] []
            }
        , taskBuildHaddock = False
        , taskPresent = Map.empty
        , taskAllInOne = True
        , taskCachePkgSrc = CacheSrcLocal (toFilePath (parent $ lpCabalFile lp))
        , taskAnyMissing = True
        , taskBuildTypeConfig = False
        }

normalizeTarballPaths :: HasRunner env => [FilePath] -> RIO env [FilePath]
normalizeTarballPaths fps = do
    -- TODO: consider whether erroring out is better - otherwise the
    -- user might upload an incomplete tar?
    unless (null outsideDir) $
        logWarn $
            "Warning: These files are outside of the package directory, and will be omitted from the tarball: " <>
            displayShow outsideDir
    return (nubOrd files)
  where
    (outsideDir, files) = partitionEithers (map pathToEither fps)
    pathToEither fp = maybe (Left fp) Right (normalizePath fp)

normalizePath :: FilePath -> Maybe FilePath
normalizePath = fmap FP.joinPath . go . FP.splitDirectories . FP.normalise
  where
    go [] = Just []
    go ("..":_) = Nothing
    go (_:"..":xs) = go xs
    go (x:xs) = (x :) <$> go xs

dirsFromFileName :: Set FilePath -> FilePath -> Set FilePath
dirsFromFileName dirsSet fp = go Set.empty $ FP.takeDirectory fp
  where
    go s x
      | Set.member x dirsSet = s
      | otherwise = go (Set.insert x s) (FP.takeDirectory x)

-- | Check package in given tarball. This will log all warnings
-- and will throw an exception in case of critical errors.
--
-- Note that we temporarily decompress the archive to analyze it.
checkSDistTarball
  :: HasEnvConfig env
  => SDistOpts -- ^ The configuration of what to check
  -> Path Abs File -- ^ Absolute path to tarball
  -> RIO env ()
checkSDistTarball opts tarball = withTempTarGzContents tarball $ \pkgDir' -> do
    pkgDir  <- (pkgDir' </>) `liftM`
        (parseRelDir . FP.takeBaseName . FP.takeBaseName . toFilePath $ tarball)
    --               ^ drop ".tar"     ^ drop ".gz"
    checkSDistExtractedTarball opts pkgDir

-- | Check package in supplied folder, which should contain the extracted sdist tarball. This will
-- log all warnings and will throw an exception in case of critical errors.
checkSDistExtractedTarball
  :: HasEnvConfig env
  => SDistOpts -- ^ The configuration of what to check
  -> Path Abs Dir -- ^ Absolute path to tarball
  -> RIO env ()
checkSDistExtractedTarball opts pkgDir = do
    when (sdoptsBuildTarball opts) (buildExtractedTarball ResolvedPath
                                      { resolvedRelative = RelFilePath "this-is-not-used" -- ugly hack
                                      , resolvedAbsolute = pkgDir
                                      })
    unless (sdoptsIgnoreCheck opts) (checkPackageInExtractedTarball pkgDir)

checkPackageInExtractedTarball
  :: HasEnvConfig env
  => Path Abs Dir -- ^ Absolute path to tarball
  -> RIO env ()
checkPackageInExtractedTarball pkgDir = do
    (gpdio, name, _cabalfp) <- loadCabalFilePath pkgDir
    gpd <- liftIO $ gpdio YesPrintWarnings
    config  <- getDefaultPackageConfig
    let PackageDescriptionPair pkgDesc _ = resolvePackageDescription config gpd
    logInfo $
        "Checking package '" <> fromString (packageNameString name) <> "' for common mistakes"
    let pkgChecks =
          -- MSS 2017-12-12: Try out a few different variants of
          -- pkgDesc to try and provoke an error or warning. I don't
          -- know why, but when using `Just pkgDesc`, it appears that
          -- Cabal does not detect that `^>=` is used with
          -- `cabal-version: 1.24` or earlier. It seems like pkgDesc
          -- (the one we create) does not populate the `buildDepends`
          -- field, whereas flattenPackageDescription from Cabal
          -- does. In any event, using `Nothing` seems more logical
          -- for this check anyway, and the fallback to `Just pkgDesc`
          -- is just a crazy sanity check.
          case Check.checkPackage gpd Nothing of
            [] -> Check.checkPackage gpd (Just pkgDesc)
            x -> x
    fileChecks <- liftIO $ Check.checkPackageFiles minBound pkgDesc (toFilePath pkgDir)
    let checks = pkgChecks ++ fileChecks
        (errors, warnings) =
          let criticalIssue (Check.PackageBuildImpossible _) = True
              criticalIssue (Check.PackageDistInexcusable _) = True
              criticalIssue _ = False
          in partition criticalIssue checks
    unless (null warnings) $
        logWarn $ "Package check reported the following warnings:\n" <>
                   mconcat (intersperse "\n" . fmap displayShow $ warnings)
    case NE.nonEmpty errors of
        Nothing -> return ()
        Just ne -> throwM $ CheckException ne

buildExtractedTarball :: HasEnvConfig env => ResolvedPath Dir -> RIO env ()
buildExtractedTarball pkgDir = do
  envConfig <- view envConfigL
  localPackageToBuild <- readLocalPackage $ resolvedAbsolute pkgDir
  -- We remove the path based on the name of the package
  let isPathToRemove path = do
        localPackage <- readLocalPackage path
        return $ packageName (lpPackage localPackage) == packageName (lpPackage localPackageToBuild)
  pathsToKeep
    <- fmap Map.fromList
     $ flip filterM (Map.toList (smwProject (bcSMWanted (envConfigBuildConfig envConfig))))
     $ fmap not . isPathToRemove . resolvedAbsolute . ppResolvedDir . snd
  pp <- mkProjectPackage YesPrintWarnings pkgDir False
  let adjustEnvForBuild env =
        let updatedEnvConfig = envConfig
              { envConfigSourceMap = updatePackagesInSourceMap (envConfigSourceMap envConfig)
              , envConfigBuildConfig = updateBuildConfig (envConfigBuildConfig envConfig)
              }
            updateBuildConfig bc = bc
              { bcConfig = (bcConfig bc)
                 { configBuild = defaultBuildOpts { boptsTests = True } }
              }
        in set envConfigL updatedEnvConfig env
      updatePackagesInSourceMap sm =
        sm {smProject = Map.insert (cpName $ ppCommon pp) pp pathsToKeep}
  local adjustEnvForBuild $ build Nothing Nothing

-- | Version of 'checkSDistTarball' that validates the tarball, which is available as a stream of
-- bytes, rather than as a file on the file system. Contents will be extracted to a temporary
-- directory for analysis, which must be supplied as an argument. Consumes GZipped tarball contents
-- as a stream of bytes and collects it all into memory as lazy ByteString
checkSDistTarballSink
  :: HasEnvConfig env
  => SDistOpts
  -> Path Abs Dir -- ^ Temporary path where tarball should be extracted to.
  -> ConduitM ByteString Void (ResourceT (RIO env)) L.ByteString
checkSDistTarballSink opts tpath = do
    lbs <- C.getZipSink (C.ZipSink (extractTarGz tpath) *> C.ZipSink C.sinkLazy)
    lift $ lift $ checkSDistExtractedTarball opts tpath
    pure lbs

withTempTarGzContents ::
       HasLogFunc env
    => Path Abs File -- ^ Location of tarball
    -> (Path Abs Dir -> RIO env a) -- ^ Perform actions given dir with tarball contents
    -> RIO env a
withTempTarGzContents tarballPath f = withSystemTempDir "stack" $ \tmpDir ->
    C.withSourceFile (toFilePath tarballPath) $ \ sourceTarball -> do
        C.runConduitRes $ sourceTarball .| extractTarGz tmpDir
        f tmpDir

extractTarGz ::
       (MonadReader env m, HasLogFunc env, C.MonadResource m, MonadThrow m, PrimMonad m)
    => Path Abs Dir -- ^ Folder where tarball should be extract to. This path must already exist.
    -> ConduitM ByteString a m ()
extractTarGz extractIntoDir = do
    fiExcs <-
        ungzip .|
        CTar.untarWithExceptions (CTar.restoreFileIntoLenient (toFilePath extractIntoDir))
    mapM_ reportTarFileError fiExcs
  where
    reportTarFileError (fi, excs) =
        forM_ (mapMaybe fromException excs) $ \case
            CTar.UnsupportedType ty ->
                logWarn $
                "Unsupported file type: " <> displayShow ty <> " with name: " <>
                displayBytesUtf8 (CTar.filePath fi)
            _ -> pure ()
--------------------------------------------------------------------------------

-- | Reset ownership to root and allow only 0o755 or 0o644 permissions.
resetFileInfo :: CTar.FileInfo -> CTar.FileInfo
resetFileInfo fi =
    fi
        { CTar.fileUserId = 0
        , CTar.fileUserName = ""
        , CTar.fileGroupId = 0
        , CTar.fileGroupName = ""
        , CTar.fileMode =
              if CTar.fileMode fi .&. 0o700 == 0
                  then 0o644
                  else 0o755
        }


curModTime :: MonadIO m => m CTar.EpochTime
curModTime = do
    currTime <- floor <$> liftIO getPOSIXTime -- Seconds from UNIX epoch
    return $ fromIntegral (currTime :: Int64)


getDefaultPackageConfig :: (MonadIO m, MonadReader env m, HasEnvConfig env)
  => m PackageConfig
getDefaultPackageConfig = do
  platform <- view platformL
  compilerVersion <- view actualCompilerVersionL
  return PackageConfig
    { packageConfigEnableTests = False
    , packageConfigEnableBenchmarks = False
    , packageConfigFlags = mempty
    , packageConfigGhcOptions = []
    , packageConfigCompilerVersion = compilerVersion
    , packageConfigPlatform = platform
    }
