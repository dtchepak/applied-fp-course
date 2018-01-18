{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Conf.File where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.Text                  (Text)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try)

import           Data.Aeson                 (FromJSON, Object, (.:))
import           Data.Aeson.Types           (parseMaybe)

import qualified Data.Aeson                 as Aeson

import           FirstApp.Types             (ConfigError(DecodeError, MissingConfigFile),
                                             PartialConf (PartialConf))
-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

-- | File Parsing

-- We're trying to avoid complications when selecting a configuration file
-- package from Hackage. We'll use an encoding that you are probably familiar
-- with, for better or worse, and write a small parser to pull out the bits we
-- need. The package we're using is the ``aeson`` package to parse some JSON and
-- we'll pick the bits off the Object.

-- Complete the helper function that will be used to retrieve values off the
-- JSON object.

-- | fromJsonObjWithKey
-- >>> let (Just obj) = ( Aeson.decode "{\"foo\":\"Susan\"}" ) :: Maybe Aeson.Object
--
-- >>> fromJsonObjWithKey "foo" (id :: Text -> Text) obj
-- Last {getLast = Just "Susan"}
--
-- >>> fromJsonObjWithKey "foo" id obj
-- Last {getLast = Nothing}
--
-- >>> fromJsonObjWithKey "missingField" (id :: Text -> Text) obj
-- Last {getLast = Nothing}
--
fromJsonObjWithKey
  :: FromJSON a
  => Text
  -> (a -> b)
  -> Object
  -> Last b
fromJsonObjWithKey key f =
    Last . parseMaybe (\o -> f <$> (o .: key))

-- |----
-- | You will need to update these tests when you've completed the following functions!
-- | The 'undefined' in these tests needs to be replaced with their respective Error constructors.
-- |----

-- | decodeObj
-- >>> decodeObj ""
-- Left (DecodeError "Error in $: not enough input")
--
-- >>> decodeObj "{\"bar\":33}"
-- Right (fromList [("bar",Number 33.0)])
--
decodeObj
  :: ByteString
  -> Either ConfigError Object
decodeObj =
  first DecodeError . Aeson.eitherDecode

-- | Update these tests when you've completed this function.
--
-- | readObject
-- >>> readObject "badFileName.no"
-- Left (MissingConfigFile "badFileName.no")
--
-- >>> readObject "test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readObject
  :: FilePath
  -> IO ( Either ConfigError ByteString )
readObject path =
    let handleError :: IOError -> ConfigError
        handleError = const (MissingConfigFile path)
    in first handleError <$> try (LBS.readFile path)

-- Construct the function that will take a ``FilePath``, read it in and attempt
-- to decode it as a valid JSON object, using the ``aeson`` package. Then pull
-- specific keys off this object and construct our ``PartialConf``. Using the
-- function we wrote above to assist in pulling items off the object.
parseJSONConfigFile
  :: FilePath
  -> IO ( Either ConfigError PartialConf )
parseJSONConfigFile path =
  error "parseJSONConfigFile not implemented"
