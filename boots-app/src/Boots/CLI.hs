{-# LANGUAGE CPP #-}
module Boots.CLI(
    CLI(..)
  , interruptCli
  , runCLI
  ) where

import           Control.Exception   (Exception, catch)
import           Control.Monad.Catch (MonadThrow (..))
import           Data.Text           (Text)
import           Options.Applicative
#if __GLASGOW_HASKELL__ < 804
import           Control.Applicative
import           Data.Semigroup
#endif

-- | Options parsed from arguments.
data CLI = CLI
  { iHelp    :: Bool
  , iVersion :: Bool
  , options  :: [(Text, Text)]
  }

data CliInterrupt = CliInterrupt deriving Show

instance Exception CliInterrupt

-- | Normal interrupt cli.
interruptCli :: MonadThrow m => m a
interruptCli = throwM CliInterrupt

cli :: Parser CLI
cli = CLI
  <$> switch ( long "help"    <> short 'h' <> help "Show this help text")
  <*> switch ( long "version" <> short 'V' <> help "Print version information")
  <*> pure []

-- | Run cli.
runCLI :: (CLI -> IO ()) -> IO ()
runCLI f = (execParser go >>= f) `catch` ge
  where
    go = info (cli <**> helper)
      (fullDesc
      <> progDesc "Print a greeting for TARGET"
      <> header "hello - a test for optparse-applicative")
    ge CliInterrupt = return ()
