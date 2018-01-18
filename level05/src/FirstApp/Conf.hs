{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.Conf
    ( parseOptions
    ) where

import           GHC.Word                  (Word16)

import           Control.Applicative       (liftA2)

import           Data.Bifunctor            (first)
import           Data.Monoid               ((<>), Last(Last))

import           FirstApp.Types            (Conf(Conf), ConfigError(MissingPort, MissingDBFilePath),
                                            DBFilePath (DBFilePath),
                                            PartialConf(PartialConf), Port (Port))

import           FirstApp.Conf.CommandLine (commandLineParser)
import           FirstApp.Conf.File        (parseJSONConfigFile)

-- For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf =
  PartialConf
    (Last . pure . Port . fromInteger $ 3005)
    (Last . pure . DBFilePath $ "app.db")

-- We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig (PartialConf port path) =
    case (port, path) of
        (Last Nothing, _) -> Left MissingPort
        (_, Last Nothing) -> Left MissingDBFilePath
        (Last (Just port'), Last (Just path')) -> pure (Conf port' path')

-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.

-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions path =
  -- Parse the options from the config file: "appconfig.json"
  -- Parse the options from the commandline using 'commandLineParser'
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value
  let conf defaults file cmd = makeConfig $ defaults <> file <> cmd
  in parseJSONConfigFile path
            >>= either
                  (pure . Left)
                  (\f -> conf defaultConf f <$> commandLineParser)
