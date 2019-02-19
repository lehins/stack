{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-|
Module      : Stack.Sig.Sign
Description : Signing Packages
Copyright   : (c) 2015-2019, Stack contributors
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Sign (sign, signPackage, signTarBytes) where

import           Stack.Prelude
import qualified Conduit as C
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.Tar as CTar
import           Data.Conduit.Zlib (ungzip)
import qualified Distribution.PackageDescription as D
import qualified Distribution.PackageDescription.Parsec as D
import qualified Distribution.Verbosity as D
import           Network.HTTP.StackClient (RequestBody (RequestBodyBS), httpLbs, parseUrlThrow, setRequestMethod, setRequestBody, getResponseStatusCode, methodPut)
import           Path
import           Stack.Sig.GPG
import           Stack.Types.Sig
import qualified System.FilePath as FP

-- | Sign a haskell package with the given url of the signature
-- service and a path to a tarball.
sign
    :: HasLogFunc env
    => String -> Path Abs File -> RIO env Signature
sign url filePath =
    withRunInIO $ \run ->
    withSystemTempDir
        "stack"
        (\tempDir ->
              do maybePath <- C.runConduitRes $ C.sourceFile (toFilePath filePath) .|
                     ungzip .| CTar.untar (extractCabalFile tempDir) .| C.asumC
                 case maybePath of
                     Nothing -> throwM SigInvalidSDistTarBall
                     Just cabalFullPath -> do
                         pkg <- cabalFilePackageId cabalFullPath
                         run (signPackage url pkg filePath))
  where
    extractCabalFile tempDir fi =
        case CTar.fileType fi of
            CTar.FTNormal ->
                case FP.splitFileName (CTar.getFileInfoPath fi) of
                    (folder,file)
                      | length (FP.splitDirectories folder) == 1 &&
                            FP.takeExtension file == ".cabal" -> do
                          cabalFile <- parseRelFile file
                          let restorePath = tempDir </> cabalFile
                          C.sinkFile (toFilePath restorePath)
                          C.yield (Just restorePath)
                    (_,_) -> C.yield Nothing
            _ -> C.yield Nothing

-- | Sign a haskell package with the given url to the signature
-- service, a package tarball path (package tarball name) and a lazy
-- bytestring of bytes that represent the tarball bytestream.  The
-- function will write the bytes to the path in a temp dir and sign
-- the tarball with GPG.
signTarBytes
    :: HasLogFunc env
    => String -> Path Rel File -> L.ByteString -> RIO env Signature
signTarBytes url tarPath bs =
    withSystemTempDir
        "stack"
        (\tempDir ->
              do let tempTarBall = tempDir </> tarPath
                 liftIO (L.writeFile (toFilePath tempTarBall) bs)
                 sign url tempTarBall)

-- | Sign a haskell package given the url to the signature service, a
-- @PackageIdentifier@ and a file path to the package on disk.
signPackage
    :: HasLogFunc env
    => String -> PackageIdentifier -> Path Abs File -> RIO env Signature
signPackage url pkg filePath = do
    sig@(Signature signature) <- gpgSign filePath
    let (PackageIdentifier name version) = pkg
    fingerprint <- gpgVerify sig filePath
    let fullUrl =
            url <> "/upload/signature/" <> packageNameString name <> "/" <> versionString version <>
            "/" <>
            show fingerprint
    req <- parseUrlThrow fullUrl
    let put = setRequestMethod methodPut
            $ setRequestBody (RequestBodyBS signature) req
    res <- liftIO (httpLbs put)
    when
        (getResponseStatusCode res /= 200)
        (throwM (GPGSignException "unable to sign & upload package"))
    logInfo ("Signature uploaded to " <> fromString fullUrl)
    return sig

-- | Extract the @PackageIdentifier@ given an exploded haskell package
-- path.
cabalFilePackageId
    :: (MonadIO m, MonadThrow m)
    => Path Abs File -> m PackageIdentifier
cabalFilePackageId fp = do
    D.package . D.packageDescription <$> liftIO (D.readGenericPackageDescription D.silent $ toFilePath fp)
