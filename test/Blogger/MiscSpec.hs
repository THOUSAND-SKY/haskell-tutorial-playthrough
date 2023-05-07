module Blogger.MiscSpec where

import Control.Applicative (liftA2)
import Test.Hspec

newtype MyReaderT r m a = MyReaderT (r -> m a)

instance Functor (MyReaderT a b)

instance Applicative (MyReaderT a b)

instance Monad (MyReaderT a b)

-- myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 :: (a -> b -> c) -> MyReaderT Int IO a -> MyReaderT Int IO b -> MyReaderT Int IO c
myLiftA2 f fa fb = do
  -- ap (fmap f fa) fb -- needs "Monad"
  -- f <$> fa <*> fb -- doesn't need "Monad"
  fa >>= \a -> fb >>= \b -> return $ f a b -- needs monad again

-- Returns a function, so this is called like myLiftA2' `x = f a b`, and then `x f`
myLiftA2' :: (a -> b -> c) -> (Int -> IO a) -> (Int -> IO b) -> (Int -> IO c)
-- alternative
-- myLiftA2' f fa fb readerCallbackValue = do
--   a <- fa readerCallbackValue
--   b <- fb readerCallbackValue
--   return (f a b)
myLiftA2' f fa fb env =
  liftA2 f (fa env) (fb env)

spec :: Spec
spec = do
  describe "7. reader" $ do
    it "should work" $ do
      let s a b = a + b
      let f i = pure i :: IO Int
      myLiftA2' s f f 3 `shouldReturn` 6
