# boots

[![Hackage](https://img.shields.io/hackage/v/boots.svg)](https://hackage.haskell.org/package/boots)
[![stackage LTS package](http://stackage.org/package/boots/badge/lts)](http://stackage.org/lts/package/boots)
[![stackage Nightly package](http://stackage.org/package/boots/badge/nightly)](http://stackage.org/nightly/package/boots)
[![Build Status](https://travis-ci.org/leptonyu/boots.svg?branch=master)](https://travis-ci.org/leptonyu/boots)

Boot applications by using plugins.

```Haskell
main :: IO ()
main = booting (pluginSimple "application") go
  where
    go = forever $ do
      user <- require "user"
      logInfo $ "Hello, " <> user <> "!"
      liftIO $ threadDelay 1000000
```