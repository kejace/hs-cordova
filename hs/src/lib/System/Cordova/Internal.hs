{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Cordova.Internal where

import GHCJS.Types (JSRef, IsJSRef)
import GHCJS.Foreign
import GHCJS.Marshal
import JavaScript.Object (getProp)
import Control.Monad (forM, guard, join)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Text as T

fromJSRef' :: (FromJSRef a) => JSRef -> IO a
fromJSRef' ref = fromJSRef ref >>= \mx -> case mx of
  Nothing -> error "fromJSRef': deserialization failed"
  Just x  -> return x

newtype JSEither e a = JSEither { fromJSEither :: Either e a }

--type JSEitherRef e a = JSRef (JSEither e a)

instance (FromJSRef e, FromJSRef a) => FromJSRef (JSEither e a) where
  fromJSRef ary = do
    --code <- indexArray 0 $  ary
    code <- ary !! 0
    let element :: (FromJSRef a) => IO (Maybe a)
        --element = indexArray 1 (ary) >>= fromJSRef
        element = (ary >>= fromJSRef) !! 1
    if code == 0
      then element >>= \res -> return $ fmap (JSEither . Right) res
      else element >>= \res -> return $ fmap (JSEither . Left) res

fromJSEitherRef
  :: (FromJSRef e, FromJSRef a) => JSRef -> IO (Either e a)
fromJSEitherRef ref = fromJSRef ref >>= maybe
  (error "fromJSEitherRef: couldn't deserialize")
  (return . fromJSEither)

js_fromEnum :: (Enum a, Bounded a, ToJSRef a) => JSRef -> IO (Maybe a)
js_fromEnum ref = fmap (listToMaybe . catMaybes) $
  forM [minBound .. maxBound] $ \x -> do
    xref <- toJSRef x
    return $ guard (ref == xref) >> Just x

fromProp :: (FromJSRef b, IsJSString b, IsJSRef a) => T.Text -> a -> IO (Maybe b)
fromProp k obj = getProp k obj >>= fromJSRef

-- instance FromJSRef a => FromJSRef (Maybe a) where
--   fromJSRef r = if (r == jsNull) || (r == jsUndefined)
--     then return $ Just Nothing
--     else do
--       mx <- fromJSRef $ r
--       return $ case mx of
--         Just x  -> Just (Just x)
--         Nothing -> Nothing

instance ToJSRef Integer where
  toJSRef i = toJSRef (fromInteger i :: Double)

fromRefMaybe :: (FromJSRef a) => JSRef -> IO (Maybe a)
fromRefMaybe = fmap join . fromJSRef

foreign import javascript unsafe
  "$1.getTime()"
  js_dateToEpochMilli :: JSRef -> IO Double

foreign import javascript unsafe
  "$1 instanceof Date"
  js_isDate :: JSRef -> Bool

foreign import javascript unsafe
  "typeof $1 === 'number'"
  js_isNumber :: JSRef-> Bool

foreign import javascript unsafe
  "new Date($1)"
  js_epochMilliToDate :: Double -> IO (JSRef)

-- | Supports both Date objects and raw numbers (epoch milliseconds)
instance FromJSRef UTCTime where
  fromJSRef r
    | js_isDate r = do
      milli <- js_dateToEpochMilli r
      return $ Just $ posixSecondsToUTCTime $ realToFrac $ milli / 1000
    | js_isNumber r = do
      milli <- fromJSRef' (r :: JSRef)
      return $ Just $ posixSecondsToUTCTime $ realToFrac $ milli / 1000
    | otherwise = return Nothing

-- | Creates a Date object
instance ToJSRef UTCTime where
  toJSRef utc = let
    milli = realToFrac (utcTimeToPOSIXSeconds utc) * 1000
    in js_epochMilliToDate milli
