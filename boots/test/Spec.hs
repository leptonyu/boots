module Main where

import           Boots.Factory
import           Control.Concurrent.MVar
import           Control.Exception       (Exception)
import           Control.Monad.Identity
import           Test.Hspec


main = hspec $ do
  describe "Boots.Factory" specProperty


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
  context "With" $ do
    it "withFactory" $ do
      running ('A', True) (withFactory fst $ get >>= \a -> offer (a `shouldBe` 'A'))  return
      running ('A', True) (withFactory snd $ get >>= \a -> offer (a `shouldBe` True)) return
    it "within" $ do
      running () (within 'A' $ get >>= \a -> offer (a `shouldBe` 'A')) return
  context "Polish" $ do
    it "polish" $ do
      running () (polish (0 :: Int) (replicate 10 $ withFactory (+1) get)) (shouldBe 10)
  context "natTrans" $ do
    it "natTrans" $ do
      running () (natTrans runIdentityT IdentityT $ return ()) return `shouldBe` Just ()
  context "Resource" $ do
    it "bracket" $ do
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
        a <- bracket open close
        offer $ do
          a `shouldBe` 0
          b <- swapMVar ref 2
          b `shouldBe` 1
        return (return ())
    it "bracket - error" $ do
      ref <- newMVar (0 :: Int)
      (`shouldThrow` anyException) $ boot $ do
        a <- bracket (readMVar ref) (\_ -> throwM Failure)
        _ <- offer $ do
          a `shouldBe` 0
          swapMVar ref 1
        return (return ())
      v  <- readMVar ref
      v `shouldBe` 1

