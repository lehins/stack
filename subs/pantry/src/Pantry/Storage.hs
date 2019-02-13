{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Pantry.Storage
  ( SqlBackend
  , initStorage
  , withStorage
  , migrateAll
  , storeBlob
  , loadBlob
  , loadBlobById
  , loadBlobBySHA
  , getBlobKey
  , loadURLBlob
  , storeURLBlob
  , clearHackageRevisions
  , storeHackageRevision
  , loadHackagePackageVersions
  , loadHackagePackageVersion
  , loadLatestCacheUpdate
  , storeCacheUpdate
  , storeHackageTarballInfo
  , loadHackageTarballInfo
  , storeTree
  , loadTree
  , loadPackageById
  , getPackageNameById
  , getPackageNameId
  , getVersionId
  , getTreeForKey
  , storeHackageTree
  , loadHackageTree
  , loadHackageTreeKey
  , storeArchiveCache
  , loadArchiveCache
  , storeRepoCache
  , loadRepoCache
  , storePreferredVersion
  , loadPreferredVersion
  , sinkHackagePackageNames
  , countHackageCabals
  , PackageNameId
  , PackageName
  , VersionId
  , Version
  , Unique(..)
  , EntityField(..)
    -- avoid warnings
  , BlobId
  , unBlobKey
  , HackageCabalId
  , HackageCabal(..)
  , HackageTarballId
  , CacheUpdateId
  , FilePathId
  , Tree(..)
  , TreeId
  , TreeEntry(..)
  , TreeEntryId
  , ArchiveCacheId
  , RepoCacheId
  , PreferredVersionsId
  , UrlBlobId
  ) where

import RIO hiding (FilePath)
import qualified RIO.ByteString as B
import qualified Pantry.Types as P
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import RIO.Orphans ()
import qualified Pantry.SHA256 as SHA256
import qualified RIO.Map as Map
import RIO.Time (UTCTime, getCurrentTime)
import qualified RIO.Text as T
import Path (Path, Abs, File, toFilePath, parent)
import Path.IO (ensureDir)
import Data.Pool (destroyAllResources)
import Conduit
import Data.Acquire (with)
import Pantry.Types (PackageNameP (..), VersionP (..), SHA256, FileSize (..), FileType (..), HasPantryConfig, BlobKey, Repo (..), TreeKey, SafeFilePath, Revision (..), Package (..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Raw blobs
Blob
    sha SHA256
    size FileSize
    contents ByteString
    UniqueBlobSha sha
-- Previously downloaded blobs from given URLs.
-- May change over time, so we keep a time column too.
UrlBlob sql=url_blob
    url Text
    blob BlobId
    time UTCTime
    UniqueUrlTime url time

-- For normalization, and avoiding storing strings in a bunch of
-- tables.
PackageName
    name P.PackageNameP
    UniquePackageName name
Version
    version P.VersionP
    UniqueVersion version
FilePath
    path P.SafeFilePath
    UniqueSfp path

-- Secure download information for a package on Hackage. This does not
-- contain revision information, since sdist tarballs are (blessedly)
-- unmodified on Hackage.
HackageTarball
    name PackageNameId
    version VersionId
    sha SHA256
    size FileSize
    UniqueHackageTarball name version

-- An individual cabal file from Hackage, representing a specific
-- revision.
HackageCabal
    name PackageNameId
    version VersionId
    revision P.Revision
    cabal BlobId

    -- If available: the full tree containing the HackageTarball
    -- contents with the cabal file modified.
    tree TreeId Maybe
    UniqueHackage name version revision

-- Any preferred-version information from Hackage
PreferredVersions
    name PackageNameId
    preferred Text
    UniquePreferred name

-- Last time we downloaded a 01-index.tar file from Hackage and
-- updated the three previous tables.
CacheUpdate
    -- When did we do the update?
    time UTCTime

    -- How big was the file when we updated, ignoring the last two
    -- all-null 512-byte blocks.
    size FileSize

    -- SHA256 of the first 'size' bytes of the file
    sha SHA256

-- A tree containing a Haskell package. See associated TreeEntry
-- table.
Tree
    key BlobId
    cabal BlobId
    cabalType FileType
    name PackageNameId
    version VersionId
    UniqueTree key

-- An individual file within a Tree.
TreeEntry
    tree TreeId
    path FilePathId
    blob BlobId
    type FileType

-- Like UrlBlob, but stores the contents as a Tree.
ArchiveCache
    time UTCTime
    url Text
    subdir Text
    sha SHA256
    size FileSize
    tree TreeId

-- Like ArchiveCache, but for a Repo.
RepoCache
    time UTCTime
    url Text
    type P.RepoType
    commit Text
    subdir Text
    tree TreeId
|]

initStorage
  :: HasLogFunc env
  => Path Abs File -- ^ storage file
  -> (P.Storage -> RIO env a)
  -> RIO env a
initStorage fp inner = do
  ensureDir $ parent fp
  bracket
    (createSqlitePoolFromInfo sqinfo 1)
    (liftIO . destroyAllResources) $ \pool -> do

    migrates <- runSqlPool (runMigrationSilent migrateAll) pool
    forM_ migrates $ \mig -> logDebug $ "Migration output: " <> display mig
    inner (P.Storage pool)
  where
    sqinfo = set extraPragmas ["PRAGMA busy_timeout=2000;"]
           $ set fkEnabled True
           $ mkSqliteConnectionInfo (fromString $ toFilePath fp)

withStorage
  :: (HasPantryConfig env, HasLogFunc env)
  => ReaderT SqlBackend (RIO env) a
  -> RIO env a
withStorage action = do
  P.Storage pool <- view $ P.pantryConfigL.to P.pcStorage
  runSqlPool action pool


rdbmsAwareQuery
  :: MonadIO m
  => ReaderT SqlBackend m a
  -> ReaderT SqlBackend m a
  -> ReaderT SqlBackend m a
rdbmsAwareQuery postgresQuery sqliteQuery = do
  rdbms <- connRDBMS <$> ask
  case rdbms of
    "postgresql" -> postgresQuery
    "sqlite" -> sqliteQuery
    _ -> error $ "rdbmsAwareQuery: unsupported rdbms '" ++ T.unpack rdbms ++ "'"


getPackageNameById
  :: MonadIO m
  => PackageNameId
  -> ReaderT SqlBackend m (Maybe P.PackageName)
getPackageNameById = fmap (unPackageNameP . packageNameName <$>) . get


getPackageNameId
  :: MonadIO m
  => P.PackageName
  -> ReaderT SqlBackend m PackageNameId
getPackageNameId = fmap (either entityKey id) . insertBy . PackageName . PackageNameP

getVersionId
  :: MonadIO m
  => P.Version
  -> ReaderT SqlBackend m VersionId
getVersionId = fmap (either entityKey id) . insertBy . Version . VersionP

storeBlob
  :: MonadIO m
  => ByteString
  -> ReaderT SqlBackend m (BlobId, BlobKey)
storeBlob bs = do
  let sha = SHA256.hashBytes bs
      size = FileSize $ fromIntegral $ B.length bs
  keys <- selectKeysList [BlobSha ==. sha] []
  key <-
    case keys of
      [] -> rdbmsAwareQuery
            (do rawExecute
                  "INSERT INTO blob(sha, size, contents) VALUES (?, ?, ?) ON CONFLICT DO NOTHING"
                  [toPersistValue sha, toPersistValue size, toPersistValue bs]
                rawSql "SELECT blob.id FROM blob WHERE blob.sha = ?" [toPersistValue sha] >>= \case
                  [Single key] -> pure key
                  _ -> error "soreBlob: there was a critical problem storing a blob.")
            (insert Blob
                    { blobSha = sha
                    , blobSize = size
                    , blobContents = bs
                    })
      key:rest -> assert (null rest) (pure key)
  pure (key, P.BlobKey sha size)

loadBlob
  :: (HasPantryConfig env, HasLogFunc env)
  => BlobKey
  -> ReaderT SqlBackend (RIO env) (Maybe ByteString)
loadBlob (P.BlobKey sha size) = do
  ment <- getBy $ UniqueBlobSha sha
  case ment of
    Nothing -> pure Nothing
    Just (Entity _ bt)
      | blobSize bt == size -> pure $ Just $ blobContents bt
      | otherwise ->
          Nothing <$ lift (logWarn $
             "Mismatched blob size detected for SHA " <> display sha <>
             ". Expected size: " <> display size <>
             ". Actual size: " <> display (blobSize bt))

loadBlobBySHA :: MonadIO m => SHA256 -> ReaderT SqlBackend m (Maybe BlobId)
loadBlobBySHA sha = listToMaybe <$> selectKeysList [BlobSha ==. sha] []

loadBlobById :: MonadIO m => BlobId -> ReaderT SqlBackend m ByteString
loadBlobById bid = do
  mbt <- get bid
  case mbt of
    Nothing -> error "loadBlobById: ID doesn't exist in database"
    Just bt -> pure $ blobContents bt

getBlobKey :: MonadIO m => BlobId -> ReaderT SqlBackend m BlobKey
getBlobKey bid = do
  res <- rawSql "SELECT sha, size FROM blob WHERE id=?" [toPersistValue bid]
  case res of
    [] -> error $ "getBlobKey failed due to missing ID: " ++ show bid
    [(Single sha, Single size)] -> pure $ P.BlobKey sha size
    _ -> error $ "getBlobKey failed due to non-unique ID: " ++ show (bid, res)

getBlobId :: MonadIO m => BlobKey -> ReaderT SqlBackend m (Maybe BlobId)
getBlobId (P.BlobKey sha size) = do
  res <- rawSql "SELECT id FROM blob WHERE sha=? AND size=?"
           [toPersistValue sha, toPersistValue size]
  pure $ listToMaybe $ map unSingle res

loadURLBlob :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe ByteString)
loadURLBlob url = do
  ment <- rawSql
    "SELECT blob.contents\n\
    \FROM blob, url_blob\n\
    \WHERE url=?\
    \  AND url_blob.blob=blob.id\n\
    \ ORDER BY url_blob.time DESC"
    [toPersistValue url]
  case ment of
    [] -> pure Nothing
    (Single bs) : _ -> pure $ Just bs

storeURLBlob :: MonadIO m => Text -> ByteString -> ReaderT SqlBackend m ()
storeURLBlob url blob = do
  (blobId, _) <- storeBlob blob
  now <- getCurrentTime
  insert_ UrlBlob
        { urlBlobUrl = url
        , urlBlobBlob = blobId
        , urlBlobTime = now
        }

clearHackageRevisions :: MonadIO m => ReaderT SqlBackend m ()
clearHackageRevisions = deleteWhere ([] :: [Filter HackageCabal])

storeHackageRevision
  :: MonadIO m
  => P.PackageName
  -> P.Version
  -> BlobId
  -> ReaderT SqlBackend m ()
storeHackageRevision name version key = do
  nameid <- getPackageNameId name
  versionid <- getVersionId version
  rev <- count
    [ HackageCabalName ==. nameid
    , HackageCabalVersion ==. versionid
    ]
  insert_ HackageCabal
    { hackageCabalName = nameid
    , hackageCabalVersion = versionid
    , hackageCabalRevision = Revision (fromIntegral rev)
    , hackageCabalCabal = key
    , hackageCabalTree = Nothing
    }

loadHackagePackageVersions
  :: MonadIO m
  => P.PackageName
  -> ReaderT SqlBackend m (Map P.Version (Map Revision BlobKey))
loadHackagePackageVersions name = do
  nameid <- getPackageNameId name
  -- would be better with esequeleto
  (Map.fromListWith Map.union . map go) <$> rawSql
    "SELECT hackage.revision, version.version, blob.sha, blob.size\n\
    \FROM hackage_cabal as hackage, version, blob\n\
    \WHERE hackage.name=?\n\
    \AND   hackage.version=version.id\n\
    \AND   hackage.cabal=blob.id"
    [toPersistValue nameid]
  where
    go (Single revision, Single (P.VersionP version), Single key, Single size) =
      (version, Map.singleton revision (P.BlobKey key size))

loadHackagePackageVersion
  :: MonadIO m
  => P.PackageName
  -> P.Version
  -> ReaderT SqlBackend m (Map Revision (BlobId, P.BlobKey))
loadHackagePackageVersion name version = do
  nameid <- getPackageNameId name
  versionid <- getVersionId version
  -- would be better with esequeleto
  (Map.fromList . map go) <$> rawSql
    "SELECT hackage.revision, blob.sha, blob.size, blob.id\n\
    \FROM hackage_cabal as hackage, version, blob\n\
    \WHERE hackage.name=?\n\
    \AND   hackage.version=?\n\
    \AND   hackage.cabal=blob.id"
    [toPersistValue nameid, toPersistValue versionid]
  where
    go (Single revision, Single sha, Single size, Single bid) =
      (revision, (bid, P.BlobKey sha size))

loadLatestCacheUpdate
  :: MonadIO m
  => ReaderT SqlBackend m (Maybe (FileSize, SHA256))
loadLatestCacheUpdate =
    fmap go <$> selectFirst [] [Desc CacheUpdateTime]
  where
    go (Entity _ cu) = (cacheUpdateSize cu, cacheUpdateSha cu)

storeCacheUpdate
  :: MonadIO m
  => FileSize
  -> SHA256
  -> ReaderT SqlBackend m ()
storeCacheUpdate size sha = do
  now <- getCurrentTime
  insert_ CacheUpdate
    { cacheUpdateTime = now
    , cacheUpdateSize = size
    , cacheUpdateSha = sha
    }

storeHackageTarballInfo
  :: MonadIO m
  => P.PackageName
  -> P.Version
  -> SHA256
  -> FileSize
  -> ReaderT SqlBackend m ()
storeHackageTarballInfo name version sha size = do
  nameid <- getPackageNameId name
  versionid <- getVersionId version
  void $ insertBy HackageTarball
    { hackageTarballName = nameid
    , hackageTarballVersion = versionid
    , hackageTarballSha = sha
    , hackageTarballSize = size
    }

loadHackageTarballInfo
  :: MonadIO m
  => P.PackageName
  -> P.Version
  -> ReaderT SqlBackend m (Maybe (SHA256, FileSize))
loadHackageTarballInfo name version = do
  nameid <- getPackageNameId name
  versionid <- getVersionId version
  fmap go <$> getBy (UniqueHackageTarball nameid versionid)
  where
    go (Entity _ ht) = (hackageTarballSha ht, hackageTarballSize ht)

getFilePathId
  :: MonadIO m
  => SafeFilePath
  -> ReaderT SqlBackend m FilePathId
getFilePathId sfp =
  selectKeysList [FilePathPath ==. sfp] [] >>= \case
    [fpId] -> pure fpId
    []     -> rdbmsAwareQuery
              (do rawExecute
                    "INSERT INTO file_path(path) VALUES (?) ON CONFLICT DO NOTHING"
                    [toPersistValue sfp]
                  rawSql "SELECT id FROM file_path WHERE path = ?" [toPersistValue sfp] >>= \case
                    [Single key] -> pure key
                    _ -> error "getFilePathId: there was a critical problem storing a blob.")
              (insert $ FilePath sfp)
    _      -> error $ "getFilePathId: FilePath unique constraint key is violated for: " ++ fp
  where
    fp = T.unpack (P.unSafeFilePath sfp)


storeTree
  :: MonadIO m
  => P.PackageIdentifier
  -> P.Tree
  -> P.TreeEntry
  -- ^ cabal file
  -> ReaderT SqlBackend m (TreeId, P.TreeKey)
storeTree (P.PackageIdentifier name version) tree@(P.TreeMap m) (P.TreeEntry (P.BlobKey cabal _) cabalType) = do
  (bid, blobKey) <- storeBlob $ P.renderTree tree
  mcabalid <- loadBlobBySHA cabal
  cabalid <-
    case mcabalid of
      Just cabalid -> pure cabalid
      Nothing -> error $ "storeTree: cabal BlobKey not found: " ++ show (tree, cabal)
  nameid <- getPackageNameId name
  versionid <- getVersionId version
  etid <- insertBy Tree
    { treeKey = bid
    , treeCabal = cabalid
    , treeCabalType = cabalType
    , treeName = nameid
    , treeVersion = versionid
    }
  case etid of
    Left (Entity tid _) -> pure (tid, P.TreeKey blobKey) -- already in database, assume it matches
    Right tid -> do
      for_ (Map.toList m) $ \(sfp, P.TreeEntry blobKey' ft) -> do
        sfpid <- getFilePathId sfp
        mbid <- getBlobId blobKey'
        bid' <-
          case mbid of
            Nothing -> error $ "Cannot store tree, contains unknown blob: " ++ show blobKey'
            Just bid' -> pure bid'
        insert_ TreeEntry
          { treeEntryTree = tid
          , treeEntryPath = sfpid
          , treeEntryBlob = bid'
          , treeEntryType = ft
          }
      pure (tid, P.TreeKey blobKey)

loadTree :: MonadIO m => P.TreeKey -> ReaderT SqlBackend m (Maybe P.Tree)
loadTree key = do
  ment <- getTreeForKey key
  case ment of
    Nothing -> pure Nothing
    Just ent -> Just <$> loadTreeByEnt ent

getTreeForKey
  :: MonadIO m
  => TreeKey
  -> ReaderT SqlBackend m (Maybe (Entity Tree))
getTreeForKey (P.TreeKey key) = do
  mbid <- getBlobId key
  case mbid of
    Nothing -> pure Nothing
    Just bid -> getBy $ UniqueTree bid

loadPackageById :: MonadIO m => TreeId -> ReaderT SqlBackend m Package
loadPackageById tid = do
  mts <- get tid
  ts <-
    case mts of
      Nothing -> error $ "loadPackageById: invalid foreign key " ++ show tid
      Just ts -> pure ts
  tree <- loadTreeByEnt $ Entity tid ts
  key <- getBlobKey $ treeKey ts

  mname <- get $ treeName ts
  name <-
    case mname of
      Nothing -> error $ "loadPackageByid: invalid foreign key " ++ show (treeName ts)
      Just (PackageName (P.PackageNameP name)) -> pure name

  mversion <- get $ treeVersion ts
  version <-
    case mversion of
      Nothing -> error $ "loadPackageByid: invalid foreign key " ++ show (treeVersion ts)
      Just (Version (P.VersionP version)) -> pure version

  cabalKey <- getBlobKey $ treeCabal ts
  let ident = P.PackageIdentifier name version
  let cabalEntry = P.TreeEntry cabalKey (treeCabalType ts)
  pure Package
    { packageTreeKey = P.TreeKey key
    , packageTree = tree
    , packageCabalEntry = cabalEntry
    , packageIdent = ident
    }

loadTreeByEnt
  :: MonadIO m
  => Entity Tree
  -> ReaderT SqlBackend m P.Tree
loadTreeByEnt (Entity tid _t) = do
  entries <- rawSql
    "SELECT file_path.path, blob.sha, blob.size, tree_entry.type\n\
    \FROM tree_entry, blob, file_path\n\
    \WHERE tree_entry.tree=?\n\
    \AND   tree_entry.blob=blob.id\n\
    \AND   tree_entry.path=file_path.id"
    [toPersistValue tid]
  pure $ P.TreeMap $ Map.fromList $ map
    (\(Single sfp, Single sha, Single size, Single ft) ->
         (sfp, P.TreeEntry (P.BlobKey sha size) ft))
    entries

storeHackageTree
  :: MonadIO m
  => P.PackageName
  -> P.Version
  -> BlobId
  -> P.TreeKey
  -> ReaderT SqlBackend m ()
storeHackageTree name version cabal treeKey' = do
  nameid <- getPackageNameId name
  versionid <- getVersionId version
  ment <- getTreeForKey treeKey'
  for_ ment $ \ent -> updateWhere
    [ HackageCabalName ==. nameid
    , HackageCabalVersion ==. versionid
    , HackageCabalCabal ==. cabal
    ]
    [HackageCabalTree =. Just (entityKey ent)]

loadHackageTreeKey
  :: MonadIO m
  => P.PackageName
  -> P.Version
  -> SHA256
  -> ReaderT SqlBackend m (Maybe TreeKey)
loadHackageTreeKey name ver sha = do
  res <- rawSql
    "SELECT treeblob.sha, treeblob.size\n\
    \FROM blob as treeblob, blob as cabalblob, package_name, version, hackage_cabal, tree\n\
    \WHERE package_name.name=?\n\
    \AND   version.version=?\n\
    \AND   cabalblob.sha=?\n\
    \AND   hackage_cabal.name=package_name.id\n\
    \AND   hackage_cabal.version=version.id\n\
    \AND   hackage_cabal.cabal=cabalblob.id\n\
    \AND   hackage_cabal.tree=tree.id\n\
    \AND   tree.key=treeblob.id"
    [ toPersistValue $ P.PackageNameP name
    , toPersistValue $ P.VersionP ver
    , toPersistValue sha
    ]
  case res of
    [] -> pure Nothing
    (Single treesha, Single size):_ ->
      pure $ Just $ P.TreeKey $ P.BlobKey treesha size

loadHackageTree
  :: MonadIO m
  => P.PackageName
  -> P.Version
  -> BlobId
  -> ReaderT SqlBackend m (Maybe Package)
loadHackageTree name ver bid = do
  nameid <- getPackageNameId name
  versionid <- getVersionId ver
  ment <- selectFirst
    [ HackageCabalName ==. nameid
    , HackageCabalVersion ==. versionid
    , HackageCabalCabal ==. bid
    , HackageCabalTree !=. Nothing
    ]
    []
  case ment of
    Nothing -> pure Nothing
    Just (Entity _ hc) ->
      case hackageCabalTree hc of
        Nothing -> assert False $ pure Nothing
        Just tid -> Just <$> loadPackageById tid

storeArchiveCache
  :: MonadIO m
  => Text -- ^ URL
  -> Text -- ^ subdir
  -> SHA256
  -> FileSize
  -> P.TreeKey
  -> ReaderT SqlBackend m ()
storeArchiveCache url subdir sha size treeKey' = do
  now <- getCurrentTime
  ment <- getTreeForKey treeKey'
  for_ ment $ \ent -> insert_ ArchiveCache
    { archiveCacheTime = now
    , archiveCacheUrl = url
    , archiveCacheSubdir = subdir
    , archiveCacheSha = sha
    , archiveCacheSize = size
    , archiveCacheTree = entityKey ent
    }

loadArchiveCache
  :: MonadIO m
  => Text -- ^ URL
  -> Text -- ^ subdir
  -> ReaderT SqlBackend m [(SHA256, FileSize, TreeId)]
loadArchiveCache url subdir = map go <$> selectList
  [ ArchiveCacheUrl ==. url
  , ArchiveCacheSubdir ==. subdir
  ]
  [Desc ArchiveCacheTime]
  where
    go (Entity _ ac) = (archiveCacheSha ac, archiveCacheSize ac, archiveCacheTree ac)

storeRepoCache
  :: MonadIO m
  => Repo
  -> Text -- ^ subdir
  -> TreeId
  -> ReaderT SqlBackend m ()
storeRepoCache repo subdir tid = do
  now <- getCurrentTime
  insert_ RepoCache
    { repoCacheTime = now
    , repoCacheUrl = repoUrl repo
    , repoCacheType = repoType repo
    , repoCacheCommit = repoCommit repo
    , repoCacheSubdir = subdir
    , repoCacheTree = tid
    }

loadRepoCache
  :: MonadIO m
  => Repo
  -> Text -- ^ subdir
  -> ReaderT SqlBackend m (Maybe TreeId)
loadRepoCache repo subdir = fmap (repoCacheTree . entityVal) <$> selectFirst
  [ RepoCacheUrl ==. repoUrl repo
  , RepoCacheType ==. repoType repo
  , RepoCacheCommit ==. repoCommit repo
  , RepoCacheSubdir ==. subdir
  ]
  [Desc RepoCacheTime]

storePreferredVersion
  :: MonadIO m
  => P.PackageName
  -> Text
  -> ReaderT SqlBackend m ()
storePreferredVersion name p = do
  nameid <- getPackageNameId name
  ment <- getBy $ UniquePreferred nameid
  case ment of
    Nothing -> insert_ PreferredVersions
      { preferredVersionsName = nameid
      , preferredVersionsPreferred = p
      }
    Just (Entity pid _) -> update pid [PreferredVersionsPreferred =. p]

loadPreferredVersion
  :: MonadIO m
  => P.PackageName
  -> ReaderT SqlBackend m (Maybe Text)
loadPreferredVersion name = do
  nameid <- getPackageNameId name
  fmap (preferredVersionsPreferred . entityVal) <$> getBy (UniquePreferred nameid)

sinkHackagePackageNames
  :: MonadUnliftIO m
  => (P.PackageName -> Bool)
  -> ConduitT P.PackageName Void (ReaderT SqlBackend m) a
  -> ReaderT SqlBackend m a
sinkHackagePackageNames predicate sink = do
  acqSrc <- selectSourceRes [] []
  with acqSrc $ \src -> runConduit
    $ src
   .| concatMapMC go
   .| sink
  where
    go (Entity nameid (PackageName (PackageNameP name)))
      | predicate name = do
          -- Make sure it's actually on Hackage. Would be much more
          -- efficient with some raw SQL and an inner join, but we
          -- don't have a Conduit version of rawSql.
          onHackage <- checkOnHackage nameid
          pure $ if onHackage then Just name else Nothing
      | otherwise = pure Nothing

    checkOnHackage nameid = do
      cnt <- count [HackageCabalName ==. nameid]
      pure $ cnt > 0

countHackageCabals
  :: MonadIO m
  => ReaderT SqlBackend m Int
countHackageCabals = do
  res <- rawSql
    "SELECT COUNT(*)\n\
    \FROM hackage_cabal"
    []
  case res of
    [] -> pure 0
    (Single n):_ ->
      pure n
