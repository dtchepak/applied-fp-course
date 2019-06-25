{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.Bifunctor             (bimap)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (IOException, try)
import           Control.Monad.IO.Class     (liftIO)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Attoparsec      as D
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

import           Level06.AppM               (AppM, liftEither, hoistError)
import           Level06.Types              (ConfigError (..),
                                             PartialConf (PartialConf),
                                             partialConfDecoder)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> readConfFile "badFileName.no"
-- Left (undefined "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile =
  hoistError ConfIOError . liftIO . try . BS.readFile

  -- (>>= liftEither . first ConfIOError) . liftIO . try . BS.readFile

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile f =
  let
    decode = D.decodeAttoparsecByteString partialConfDecoder
  in readConfFile f >>=
    hoistError (BadConfFile . fst) . decode

-- Go to 'src/Level06/Conf.hs' next.
