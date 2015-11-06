import           Control.Concurrent
import           Control.Concurrent.Lifted    (fork)
import           Control.Exception            (Exception, MaskingState (MaskedInterruptible),
                                               getMaskingState, throwIO, try)
import           Control.Exception            (SomeException, handle)
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.IORef
import           Data.Typeable                (Typeable)
import           Test.Hspec
import           Data.Acquire

main :: IO ()
main = hspec $ do
    describe "general" $ do
        it "survives releasing bottom" $ do
            x <- newIORef 0
            handle (\(_ :: SomeException) -> return ()) $ runResourceT $ do
                _ <- register $ writeIORef x 1
                release undefined
            x' <- readIORef x
            x' `shouldBe` 1
