{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Boots.CLI(
    CLI(..)
  , interruptCli
  , runCLI
  ) where

import           Boots.Prelude
import           Control.Exception    (Exception, catch)
import           Control.Monad.Catch  (MonadThrow (..))
import           Data.List            (intercalate)
import           Data.Text            (Text)
import           Data.Version         (Version, showVersion)
import           Data.Void
import           Options.Applicative
import           Salak
import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Char as M
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif

-- | Options parsed from arguments.
data CLI = CLI
  { cliV :: Bool
  , cOpt :: [(Text, Text)]
  }

data CliInterrupt = CliInterrupt deriving Show

instance Exception CliInterrupt

-- | Normal interrupt cli.
interruptCli :: MonadThrow m => m a
interruptCli = throwM CliInterrupt

cli :: Parser CLI
cli = CLI
  <$> switch (long "version" <> short 'V' <> help "Print version information")
  <*> many (argument (eitherReader go) (metavar "KEY=VAL..."))
  where
    go = mapLeft M.errorBundlePretty . M.parse kv "" . fromString
    kv :: P (Text, Text)
    kv = do
      k <- key
      _ <- M.char '='
      v <- val
      return (fromString $ intercalate "." k, fromString v)
    key = ((:) <$> M.lowerChar <*> M.many (M.choice [ M.lowerChar, M.digitChar, M.char '-'])) `M.sepBy` M.char '.'
    val = M.some M.printChar

type P = M.Parsec Void Text

-- | Run cli.
runCLI :: Version -> (ParseCommandLine -> IO ()) -> IO ()
runCLI v f = (execParser go >>= g2) `catch` ge
  where
    go = info (cli <**> helper) fullDesc
    ge CliInterrupt   = return ()
    g2 CLI{..} = do
      if cliV
        then putStrLn $ showVersion v
        else f $ \_ -> return cOpt
