module Main where

import           Control.Concurrent.MVar
import           Control.Exception       (Exception)
import           Control.Monad.Factory
import           Control.Monad.Identity
import           Test.Hspec


main = hspec $ do
  describe "Control.Monad.Factory" specProperty


data TestExp = Failure deriving Show

instance Exception TestExp

specProperty :: SpecWith ()
specProperty = do
  context "Definition" $ do
    it "running" $ do
      running () (return ()) return      `shouldBe` Just ()
    it "running - Error" $ do
      running () (throwM Failure) return              `shouldBe` Nothing
      running () (return ()) (const $ throwM Failure) `shouldBe` Nothing
    it "boot" $ do
      boot (return $ return ()) `shouldBe` Just ()
    it "boot - Error " $ do
      boot (return $ throwM Failure) `shouldBe` Nothing
    it "within" $ do
      running () (within 'A' $ getEnv >>= \a -> liftFT (a `shouldBe` 'A')) return
  context "natTrans" $ do
    it "natTrans" $ do
      running () (natTrans runIdentityT IdentityT $ return ()) return `shouldBe` Just ()
  context "Resource" $ do
    it "produce" $ do
      ref <- newMVar (0 :: Int)
      let
        open = do
          a <- swapMVar ref 1
          a `shouldBe` 0
          return a
      let
        close a = do
          a `shouldBe` 0
          b <- swapMVar ref 3
          b `shouldBe` 2
          return ()
      boot $ do
        a <- produce open close
        liftFT $ do
          a `shouldBe` 0
          b <- swapMVar ref 2
          b `shouldBe` 1
        return (return ())
    it "bracket - error" $ do
      ref <- newMVar (0 :: Int)
      (`shouldThrow` anyException) $ boot $ do
        a <- produce (readMVar ref) (\_ -> throwM Failure)
        _ <- liftFT $ do
          a `shouldBe` 0
          swapMVar ref 1
        return (return ())
      v  <- readMVar ref
      v `shouldBe` 1

