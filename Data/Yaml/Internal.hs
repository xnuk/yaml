{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Yaml.Internal
    (
      ParseException(..)
    , Schema(..)
    , prettyPrintParseException
    , parse
    , decodeHelper
    , decodeHelper_
    , specialStrings
    , isNumeric
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), Applicative(..))
#endif
import Control.Exception
import Control.Monad (liftM, ap, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.State
import Data.Aeson
import Data.Aeson.Types hiding (parse)
import qualified Data.Attoparsec.Text as Atto
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Conduit ((.|), ConduitM, runConduit)
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Typeable
import qualified Data.Vector as V

import qualified Text.Libyaml as Y
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile)

data ParseException = NonScalarKey
                    | UnknownAlias { _anchorName :: Y.AnchorName }
                    | UnexpectedEvent { _received :: Maybe Event
                                      , _expected :: Maybe Event
                                      }
                    | InvalidYaml (Maybe YamlException)
                    | AesonException String
                    | OtherParseException SomeException
                    | NonStringKeyAlias Y.AnchorName Value
                    | CyclicIncludes
    deriving (Show, Typeable)

data Schema = Failsafe
--          | JSON
--          | Core
            | OtherSchema
    deriving (Show, Eq)

instance Exception ParseException where
#if MIN_VERSION_base(4, 8, 0)
  displayException = prettyPrintParseException
#endif

-- | Alternative to 'show' to display a 'ParseException' on the screen.
--   Instead of displaying the data constructors applied to their arguments,
--   a more textual output is returned. For example, instead of printing:
--
-- > InvalidYaml (Just (YamlParseException {yamlProblem = "did not find expected ',' or '}'", yamlContext = "while parsing a flow mapping", yamlProblemMark = YamlMark {yamlIndex = 42, yamlLine = 2, yamlColumn = 12}})))
--
--   It looks more pleasant to print:
--
-- > YAML parse exception at line 2, column 12,
-- > while parsing a flow mapping:
-- > did not find expected ',' or '}'
--
-- Since 0.8.11
prettyPrintParseException :: ParseException -> String
prettyPrintParseException pe = case pe of
  NonScalarKey -> "Non scalar key"
  UnknownAlias anchor -> "Unknown alias `" ++ anchor ++ "`"
  UnexpectedEvent { _expected = mbExpected, _received = mbUnexpected } -> unlines
    [ "Unexpected event: expected"
    , "  " ++ show mbExpected
    , "but received"
    , "  " ++ show mbUnexpected
    ]
  InvalidYaml mbYamlError -> case mbYamlError of
    Nothing -> "Unspecified YAML error"
    Just yamlError -> case yamlError of
      YamlException s -> "YAML exception:\n" ++ s
      YamlParseException problem context mark -> concat
        [ "YAML parse exception at line " ++ show (yamlLine mark) ++
          ", column " ++ show (yamlColumn mark)
        , case context of
            "" -> ":\n"
            -- The context seems to include a leading "while" or similar.
            _  -> ",\n" ++ context ++ ":\n"
        , problem
        ]
  AesonException s -> "Aeson exception:\n" ++ s
  OtherParseException exc -> "Generic parse exception:\n" ++ show exc
  NonStringKeyAlias anchor value -> unlines
    [ "Non-string key alias:"
    , "  Anchor name: " ++ anchor
    , "  Value: " ++ show value
    ]
  CyclicIncludes -> "Cyclic includes"

newtype PErrorT m a = PErrorT { runPErrorT :: m (Either ParseException a) }
instance Monad m => Functor (PErrorT m) where
    fmap = liftM
instance Monad m => Applicative (PErrorT m) where
    pure  = PErrorT . return . Right
    (<*>) = ap
instance Monad m => Monad (PErrorT m) where
    return = pure
    (PErrorT m) >>= f = PErrorT $ do
        e <- m
        case e of
            Left e' -> return $ Left e'
            Right a -> runPErrorT $ f a
instance MonadTrans PErrorT where
    lift = PErrorT . liftM Right
instance MonadIO m => MonadIO (PErrorT m) where
    liftIO = lift . liftIO

type Parse = StateT (Map.Map String Value) (ResourceT IO)

requireEvent :: Event -> ConduitM Event o Parse ()
requireEvent e = do
    f <- CL.head
    unless (f == Just e) $ liftIO $ throwIO $ UnexpectedEvent f $ Just e

parse :: Schema -> ConduitM Event o Parse Value
parse schema = do
    streamStart <- CL.head
    case streamStart of
        Nothing ->
            -- empty string input
            return Null
        Just EventStreamStart -> do
            documentStart <- CL.head
            case documentStart of
                Just EventStreamEnd ->
                    -- empty file input, comment only string/file input
                    return Null
                Just EventDocumentStart -> do
                    res <- parseO schema
                    requireEvent EventDocumentEnd
                    requireEvent EventStreamEnd
                    return res
                _ -> liftIO $ throwIO $ UnexpectedEvent documentStart Nothing
        _ -> liftIO $ throwIO $ UnexpectedEvent streamStart Nothing

parseScalar :: Schema -> ByteString -> Anchor -> Style -> Tag
            -> ConduitM Event o Parse Text
parseScalar schema v a style tag = do
    let res = decodeUtf8With lenientDecode v
    case a of
        Nothing -> return res
        Just an -> do
            lift $ modify (Map.insert an $ textToValue schema style tag res)
            return res

textToValue :: Schema -> Style -> Tag -> Text -> Value
textToValue Failsafe _ _ t = String t
textToValue _ SingleQuoted _ t = String t
textToValue _ DoubleQuoted _ t = String t
textToValue _ _ StrTag t = String t
textToValue _ Folded _ t = String t
textToValue OtherSchema _ _ t
    | t `elem` ["null", "Null", "NULL", "~", ""] = Null
    | any (t `isLike`) ["y", "yes", "on", "true"] = Bool True
    | any (t `isLike`) ["n", "no", "off", "false"] = Bool False
    | Right x <- textToScientific t = Number x
    | otherwise = String t
  where x `isLike` ref = x `elem` [ref, T.toUpper ref, titleCased]
          where titleCased = toUpper (T.head ref) `T.cons` T.tail ref

textToScientific :: Text -> Either String Scientific
textToScientific = Atto.parseOnly (Atto.scientific <* Atto.endOfInput)

parseO :: Schema -> ConduitM Event o Parse Value
parseO schema = do
    me <- CL.head
    case me of
        Just (EventScalar v tag style a) ->
            textToValue schema style tag <$> parseScalar schema v a style tag
        Just (EventSequenceStart a) -> parseS schema a id
        Just (EventMappingStart a) -> parseM schema a M.empty
        Just (EventAlias an) -> do
            m <- lift get
            case Map.lookup an m of
                Nothing -> liftIO $ throwIO $ UnknownAlias an
                Just v -> return v
        _ -> liftIO $ throwIO $ UnexpectedEvent me Nothing

parseS :: Schema
       -> Y.Anchor
       -> ([Value] -> [Value])
       -> ConduitM Event o Parse Value
parseS schema a front = do
    me <- CL.peek
    case me of
        Just EventSequenceEnd -> do
            CL.drop 1
            let res = Array $ V.fromList $ front []
            case a of
                Nothing -> return res
                Just an -> do
                    lift $ modify $ Map.insert an res
                    return res
        _ -> do
            o <- parseO schema
            parseS schema a $ front . (:) o

parseM :: Schema
       -> Y.Anchor
       -> M.HashMap Text Value
       -> ConduitM Event o Parse Value
parseM schema a front = do
    me <- CL.peek
    case me of
        Just EventMappingEnd -> do
            CL.drop 1
            let res = Object front
            case a of
                Nothing -> return res
                Just an -> do
                    lift $ modify $ Map.insert an res
                    return res
        _ -> do
            CL.drop 1
            s <- case me of
                    Just (EventScalar v tag style a') -> parseScalar schema v a' style tag
                    Just (EventAlias an) -> do
                        m <- lift get
                        case Map.lookup an m of
                            Nothing -> liftIO $ throwIO $ UnknownAlias an
                            Just (String t) -> return t
                            Just v -> liftIO $ throwIO $ NonStringKeyAlias an v
                    _ -> liftIO $ throwIO $ UnexpectedEvent me Nothing
            o <- parseO schema

            let al  = M.insert s o front
                al' = if s == pack "<<"
                         then case o of
                                  Object l  -> M.union front l
                                  Array l -> M.union front $ foldl merge' M.empty $ V.toList l
                                  _          -> al
                         else al
            parseM schema a al'
    where merge' al (Object om) = M.union al om
          merge' al _           = al

decodeHelper :: FromJSON a
                   => Schema
                   -> ConduitM () Y.Event Parse ()
                   -> IO (Either ParseException (Either String a))
decodeHelper schema src = do
    -- This used to be tryAny, but the fact is that catching async
    -- exceptions is fine here. We'll rethrow them immediately in the
    -- otherwise clause.
    x <- try $ runResourceT $ flip evalStateT Map.empty $ runConduit $ src .| parse schema
    case x of
        Left e
            | Just pe <- fromException e -> return $ Left pe
            | Just ye <- fromException e -> return $ Left $ InvalidYaml $ Just (ye :: YamlException)
            | otherwise -> throwIO e
        Right y -> return $ Right $ parseEither parseJSON y

decodeHelper_ :: FromJSON a
                    => Schema
                    -> ConduitM () Event Parse ()
                    -> IO (Either ParseException a)
decodeHelper_ schema src = do
    x <- try $ runResourceT $ flip evalStateT Map.empty $ runConduit $ src .| parse schema
    return $ case x of
        Left e
            | Just pe <- fromException e -> Left pe
            | Just ye <- fromException e -> Left $ InvalidYaml $ Just (ye :: YamlException)
            | otherwise -> Left $ OtherParseException e
        Right y -> either
            (Left . AesonException)
            Right
            (parseEither parseJSON y)

-- | Strings which must be escaped so as not to be treated as non-string scalars.
specialStrings :: HashSet.HashSet Text
specialStrings = HashSet.fromList $ T.words
    "y Y yes Yes YES n N no No NO true True TRUE false False FALSE on On ON off Off OFF null Null NULL ~ *"

isNumeric :: Text -> Bool
isNumeric t =
    T.all isNumeric' t && T.any isNumber t
  where
    isNumber c = '0' <= c && c <= '9'
    isNumeric' c = isNumber c
                || c == 'e'
                || c == 'E'
                || c == '.'
                || c == '-'
                || c == '+'
