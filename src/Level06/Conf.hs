{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level06.Conf
    ( parseOptions
    ) where

import           GHC.Word                 (Word16)

import           Data.Bifunctor           (first)
import           Data.Functor             (($>))
import           Data.Monoid              ((<>), Last(..))

import           Control.Monad.IO.Class   (liftIO)

import           Level06.AppM             (AppM, liftEither)
import           Level06.Types            (Conf(..), ConfigError(..),
                                           DBFilePath (DBFilePath), PartialConf,
                                           Port (Port),
                                           PartialConf(..))

import           Level06.Conf.CommandLine (commandLineParser)
import           Level06.Conf.File        (parseJSONConfigFile)

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf =
  PartialConf (mempty $> Port 3000) (mempty $> DBFilePath "app.db")

-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig (PartialConf p f) =
    Conf
        <$> p `lastOr` MissingPort
        <*> f `lastOr` MissingDbFilePath
{-
makeConfig (PartialConf (Last Nothing) _) = Left MissingPort
makeConfig (PartialConf _ (Last Nothing)) = Left MissingDbFilePath
makeConfig (PartialConf (Last (Just p)) (Last (Just d))) = Right (Conf p d)
-}

lastOr :: Last a -> b -> Either b a
lastOr (Last (Just x)) _ = Right x
lastOr _ b = Left b

-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> AppM ConfigError Conf
parseOptions f = do
  -- Parse the options from the config file: "files/appconfig.json"
  file <- parseJSONConfigFile f
  -- Parse the options from the commandline using 'commandLineParser'
  commandLine <- liftIO commandLineParser
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value
  liftEither (makeConfig (defaultConf <> file <> commandLine))

