{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Network.Wai.Middleware.ETag
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- WAI ETag middleware for static files.

module Network.Wai.Middleware.ETag where

import           Control.Concurrent.MVar  (MVar, newMVar, takeMVar)
import           Control.Exception        (SomeException, try)
import           Control.Monad            (liftM)
import           Crypto.Hash.MD5          as MD5
import qualified Data.ByteString          as BS (ByteString, readFile)
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString.Char8    as BS8
import qualified Data.HashMap.Strict      as M
import           Network.HTTP.Date        (HTTPDate, epochTimeToHTTPDate,
                                           formatHTTPDate, parseHTTPDate)
import           Network.HTTP.Types       (Header, ResponseHeaders, status304)
import           Network.Wai              (Middleware, requestHeaders,
                                           responseLBS)
import           Network.Wai.Internal     (Request (..), Response (..))
import           System.Posix.Types       (EpochTime)
import           System.PosixCompat.Files (fileSize, getFileStatus,
                                           modificationTime)

-- | Attaches the middleware with a provided context.
etag :: ETagContext -> MaxAge -> Middleware
etag cache age app req sendResponse =
    app req $ \response -> case response of
        rf@(ResponseFile _ _ path _) -> do
            r <- hashFileCached cache path

            case (r, lookup "if-none-match" $ requestHeaders req) of
                (Hash h, Just rh) | h == rh ->
                    sendResponse $ addCacheControl age $ responseLBS status304 [] ""

                (Hash h, _) -> do
                    zz <- respond age rf [("ETag", h)]
                    sendResponse zz

                (FileNotFound, _) ->
                    sendResponse rf

                (FileTooBig, _) -> do
                    modTime <- getModificationTimeIfExists path

                    case (fmap epochTimeToHTTPDate modTime, modifiedSince req) of
                        (Just mdate, Just lastSent) | mdate == lastSent ->
                            sendResponse $ addCacheControl age $ responseLBS status304 [] ""
                        (Just mdate, _) ->
                            respond age rf [("last-modified", formatHTTPDate mdate)] >>= sendResponse
                        (Nothing, _) ->
                            respond age rf [] >>= sendResponse
        x ->
            sendResponse x

-- | Attaches the middleware without caching the calculated ETags.
etagWithoutCache :: MaxAge -> Middleware
etagWithoutCache age app req sendResponse =
    emptyETagContext False >>= \ctx -> etag ctx age app req sendResponse

-- | Finalize the response by attaching a cache-control header based on age.
respond :: Monad m => MaxAge -> Response -> [Header] -> m Response
respond age res hs =
    case res of
        (ResponseFile st hs' path part) ->
            return $ addCacheControl age $ ResponseFile st (hs ++ hs') path part
        x ->
            return x

-- | Add cache control headers to the response object.
addCacheControl :: MaxAge -> Response -> Response
addCacheControl age res =
    case res of
        (ResponseFile st hs path part) ->
            ResponseFile st (cacheControl age hs) path part
        (ResponseBuilder st hs b) ->
            ResponseBuilder st (cacheControl age hs) b
        (ResponseStream st hs body) ->
            ResponseStream st (cacheControl age hs) body
        (ResponseRaw bs f) ->
            ResponseRaw bs f

-- | Determine if-modified-since tag from the http request if present.
modifiedSince :: Request -> Maybe HTTPDate
modifiedSince req =
    lookup "if-modified-since" (requestHeaders req) >>= parseHTTPDate

-- | Determine the hash of a provided file located at 'path'.
-- If caching is enabled, use the cached checksum, otherwise
-- always re-calculate.
hashFileCached :: ETagContext -> FilePath -> IO HashResult
hashFileCached ctx path =
    if etagCtxUseCache ctx
        then
            liftM (M.lookup path) (takeMVar $ etagCtxCache ctx) >>= \r-> case r of
                Just cachedHash ->
                    return $ Hash cachedHash
                Nothing ->
                    hashFile path
        else
            hashFile path

-- | Add cache-control to the provided response-headears.
cacheControl :: MaxAge -> ResponseHeaders -> ResponseHeaders
cacheControl maxage =
    headerCacheControl . headerExpires
    where
        headerCacheControl = case maxage of
            NoMaxAge ->
                id
            MaxAgeSeconds i ->
                (:) ("Cache-Control", BS8.append "public, max-age=" $ BS8.pack $ show i)
            MaxAgeForever ->
                cacheControl (MaxAgeSeconds (60 * 60 * 24 * 365))

        headerExpires = case maxage of
            NoMaxAge ->
                id
            MaxAgeSeconds _ ->
                id -- FIXME
            MaxAgeForever ->
                (:) ("Expires", "Thu, 31 Dec 2037 23:55:55 GMT")

-- | Hash the file with MD5 located at 'fp'.
hashFile :: FilePath -> IO HashResult
hashFile fp = do
    size <- liftM fileSize (getFileStatus fp)

    if size < 1024 * 1024
        then do
            res <- try $ liftM (B64.encode . MD5.hash) (BS.readFile fp)

            return $ case res of
                Left (_ :: SomeException) ->
                    FileNotFound
                Right x ->
                    Hash x
        else
            return FileTooBig

-- | Determine the file modification time at the location 'fp' if it exists.
getModificationTimeIfExists :: FilePath -> IO (Maybe EpochTime)
getModificationTimeIfExists fp = do
    res <- try $ liftM modificationTime (getFileStatus fp)
    return $ case res of
        Left (_ :: SomeException) -> Nothing
        Right x -> Just x

-- | Create an empty 'ETagContext'.
emptyETagContext :: Bool -> IO ETagContext
emptyETagContext useCache =
    liftM (ETagContext useCache) (newMVar M.empty)

-- | Maximum age that will be attached to all file-resources
-- processed by the middleware.
data MaxAge
    = NoMaxAge
    | MaxAgeSeconds Int
    | MaxAgeForever
    deriving (Show, Eq, Ord, Read)

-- | The result of an hash-calculation of a file.
data HashResult
    = Hash BS.ByteString
    | FileTooBig
    | FileNotFound
    deriving (Show, Eq, Ord, Read)

-- | The configuration context of the middleware.
data ETagContext = ETagContext
    { etagCtxUseCache :: !Bool
    -- ^ Set to false to disable the cache
    , etagCtxCache    :: !(MVar (M.HashMap FilePath BS.ByteString))
    -- ^ The underlying store mapping filepaths to calculated checksums
    } deriving (Eq)
