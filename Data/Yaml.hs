{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides a high-level interface for processing YAML files.
--
-- This module reuses most of the infrastructure from the @aeson@ package.
-- This means that you can use all of the existing tools for JSON
-- processing for processing YAML files. As a result, much of the
-- documentation below mentions JSON; do not let that confuse you, it's
-- intentional.
--
-- For the most part, YAML content translates directly into JSON, and
-- therefore there is very little data loss. If you need to deal with YAML
-- more directly (e.g., directly deal with aliases), you should use the
-- "Text.Libyaml" module instead.
--
-- For documentation on the @aeson@ types, functions, classes, and
-- operators, please see the @Data.Aeson@ module of the @aeson@ package.
--
-- Look in the examples directory of the source repository for some initial
-- pointers on how to use this library.

#if (defined (ghcjs_HOST_OS))
module Data.Yaml {-# WARNING "GHCJS is not supported yet (will break at runtime once called)." #-}
#else
module Data.Yaml
#endif
    ( -- * Types
      Value (..)
    , Parser
    , Object
    , Array
    , ParseException(..)
    , prettyPrintParseException
    , YamlException (..)
    , YamlMark (..)
      -- * Constructors and accessors
    , object
    , array
    , (.=)
    , (.:)
    , (.:?)
    , (.!=)
      -- ** With helpers (since 0.8.23)
    , withObject
    , withText
    , withArray
    , withScientific
    , withBool
      -- * Parsing
    , parseMonad
    , parseEither
    , parseMaybe
      -- * Classes
    , ToJSON (..)
    , FromJSON (..)
      -- * Encoding/decoding
    , encode
    , encodeFile
    , decode
    , decodeFailsafe
    , decodeFile
    , decodeFileFailsafe
      -- ** Better error information
    , decodeEither
    , decodeEitherFailsafe
    , decodeEither'
    , decodeEitherFailsafe'
    , decodeFileEither
    , decodeFileEitherFailsafe
      -- ** More control over decoding
    , decodeHelper
    ) where
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative((<$>))
#endif
import Control.Exception
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson
    ( Value (..), ToJSON (..), FromJSON (..), object
    , (.=) , (.:) , (.:?) , (.!=)
    , Object, Array
    , withObject, withText, withArray, withScientific, withBool
    )
#if MIN_VERSION_aeson(1,0,0)
import Data.Aeson.Text (encodeToTextBuilder)
#else
import Data.Aeson.Encode (encodeToTextBuilder)
#endif
import Data.Aeson.Types (Pair, parseMaybe, parseEither, Parser)
import Data.ByteString (ByteString)
import Data.Conduit ((.|), ConduitM, runConduitRes)
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

import Data.Yaml.Internal hiding (decodeHelper)
import qualified Data.Yaml.Internal as YI
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile)
import qualified Text.Libyaml as Y

encode :: ToJSON a => a -> ByteString
encode obj = unsafePerformIO $ runConduitRes
    $ CL.sourceList (objToEvents $ toJSON obj)
   .| Y.encode

encodeFile :: ToJSON a => FilePath -> a -> IO ()
encodeFile fp obj = runConduitRes
    $ CL.sourceList (objToEvents $ toJSON obj)
   .| Y.encodeFile fp

objToEvents :: Value -> [Y.Event]
objToEvents o = (:) EventStreamStart
              . (:) EventDocumentStart
              $ objToEvents' o
              [ EventDocumentEnd
              , EventStreamEnd
              ]

{- FIXME
scalarToEvent :: YamlScalar -> Event
scalarToEvent (YamlScalar v t s) = EventScalar v t s Nothing
-}

objToEvents' :: Value -> [Y.Event] -> [Y.Event]
--objToEvents' (Scalar s) rest = scalarToEvent s : rest
objToEvents' (Array list) rest =
    EventSequenceStart Nothing
  : foldr objToEvents' (EventSequenceEnd : rest) (V.toList list)
objToEvents' (Object pairs) rest =
    EventMappingStart Nothing
  : foldr pairToEvents (EventMappingEnd : rest) (M.toList pairs)

-- Empty strings need special handling to ensure they get quoted. This avoids:
-- https://github.com/snoyberg/yaml/issues/24
objToEvents' (String "") rest = EventScalar "" NoTag SingleQuoted Nothing : rest

objToEvents' (String s) rest =
    event : rest
  where
    event
        -- Make sure that special strings are encoded as strings properly.
        -- See: https://github.com/snoyberg/yaml/issues/31
        | s `HashSet.member` specialStrings || isNumeric s = EventScalar (encodeUtf8 s) NoTag SingleQuoted Nothing
        | otherwise = EventScalar (encodeUtf8 s) StrTag PlainNoTag Nothing
objToEvents' Null rest = EventScalar "null" NullTag PlainNoTag Nothing : rest
objToEvents' (Bool True) rest = EventScalar "true" BoolTag PlainNoTag Nothing : rest
objToEvents' (Bool False) rest = EventScalar "false" BoolTag PlainNoTag Nothing : rest
-- Use aeson's implementation which gets rid of annoying decimal points
objToEvents' n@Number{} rest = EventScalar (TE.encodeUtf8 $ TL.toStrict $ toLazyText $ encodeToTextBuilder n) IntTag PlainNoTag Nothing : rest

pairToEvents :: Pair -> [Y.Event] -> [Y.Event]
pairToEvents (k, v) rest =
    EventScalar (encodeUtf8 k) StrTag PlainNoTag Nothing
  : objToEvents' v rest


decodeWithSchema :: FromJSON a
                 => Schema
                 -> ByteString
                 -> Maybe a
decodeWithSchema schema bs = unsafePerformIO
                    $ either (const Nothing) id
                    <$> decodeHelper_ schema (Y.decode bs)

decode, decodeFailsafe :: FromJSON a => ByteString -> Maybe a
decode = decodeWithSchema OtherSchema

-- | Same as 'decode' but treat scalar value as just string-like value instead.
--
-- > -- Just "version: 1.1\n"
-- > fmap encode (decode "version: 1.10" :: Maybe Value)
-- >
-- > -- Just "version: '1.10'\n"
-- > fmap encode (decodeFailsafe  "version: 1.10" :: Maybe Value)
decodeFailsafe = decodeWithSchema Failsafe


decodeFileWithSchema :: FromJSON a
                     => Schema
                     -> FilePath
                     -> IO (Maybe a)
decodeFileWithSchema schema fp = YI.decodeHelper schema (Y.decodeFile fp) >>= either throwIO (return . either (const Nothing) id)

decodeFile, decodeFileFailsafe :: FromJSON a => FilePath -> IO (Maybe a)
decodeFile = decodeFileWithSchema OtherSchema
decodeFileFailsafe = decodeFileWithSchema Failsafe


decodeFileEitherWithSchema
    :: FromJSON a
    => Schema
    -> FilePath
    -> IO (Either ParseException a)
decodeFileEitherWithSchema schema = decodeHelper_ schema . Y.decodeFile

-- | A version of 'decodeFile' which should not throw runtime exceptions.
--
-- Since 0.8.4
decodeFileEither, decodeFileEitherFailsafe
    :: FromJSON a
    => FilePath
    -> IO (Either ParseException a)
decodeFileEither = decodeFileEitherWithSchema OtherSchema
decodeFileEitherFailsafe = decodeFileEitherWithSchema Failsafe


decodeEitherWithSchema :: FromJSON a => Schema -> ByteString -> Either String a
decodeEitherWithSchema schema bs = unsafePerformIO
                $ either (Left . prettyPrintParseException) id
                <$> YI.decodeHelper schema (Y.decode bs)

decodeEither, decodeEitherFailsafe :: FromJSON a => ByteString -> Either String a
decodeEither = decodeEitherWithSchema OtherSchema
decodeEitherFailsafe = decodeEitherWithSchema Failsafe


decodeEitherWithSchema' :: FromJSON a => Schema -> ByteString -> Either ParseException a
decodeEitherWithSchema' schema = either Left (either (Left . AesonException) Right)
              . unsafePerformIO
              . YI.decodeHelper schema
              . Y.decode

-- | More helpful version of 'decodeEither' which returns the 'YamlException'.
--
-- Since 0.8.3
decodeEither', decodeEitherFailsafe' :: FromJSON a => ByteString -> Either ParseException a
decodeEither' = decodeEitherWithSchema' OtherSchema
decodeEitherFailsafe' = decodeEitherWithSchema' Failsafe


decodeHelper :: FromJSON a
             => ConduitM () Event (StateT (Map.Map String Value) (ResourceT IO)) ()
             -> IO (Either ParseException (Either String a))
decodeHelper = YI.decodeHelper OtherSchema

array :: [Value] -> Value
array = Array . V.fromList

parseMonad :: Monad m => (a -> Parser b) -> a -> m b
parseMonad p = either fail return . parseEither p
