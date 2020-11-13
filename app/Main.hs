module Main (main) where

import Colog (LoggerT (..), richMessageAction, usingLoggerT)
import Control.Monad.Catch
import qualified Data.HDF5 as H5
import Data.Version (showVersion)
import Options.Applicative
import SpinED

instance MonadThrow m => MonadThrow (LoggerT msg m) where
  throwM e = lift $ throwM e

-- See https://github.com/pcapriotti/optparse-applicative/issues/389
versioner :: Parser (a -> a)
versioner = infoOption (showVersion version) (short 'V' <> long "version" <> help "Show version" <> hidden)

runApp :: Text -> IO ()
runApp filename = do
  let messageAction = richMessageAction
  userConfig <- usingLoggerT messageAction $ readConfig filename >>= toConfig
  H5.withFile (cOutput userConfig) ReadWriteMode $ \file -> do
    let env = Environment userConfig messageAction file
    flip runEnvT env $ do
      buildRepresentatives
      isReal <- isOperatorReal =<< asks (cHamiltonian . eConfig)
      dtype <- asks (cDatatype . eConfig)
      withDatatype isReal dtype $ \proxy -> do
        (_, evecs, _) <- diagonalize proxy
        computeExpectations evecs

main :: IO ()
main = runApp =<< execParser options
  where
    settings = strArgument (metavar "input_file" <> help "Input yaml file")
    options =
      info
        (settings <**> helper <**> versioner)
        ( fullDesc
            <> progDesc "This is my program description"
            <> header "This is my header"
        )
