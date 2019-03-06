{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
module Hui.Auto where

import Control.Arrow
import Control.Category
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe
import Prelude hiding ((.), id)

-- newtype Auto i o = Auto (i -> (o, Auto i o))
newtype AutoM m i o = AutoM { runAutoM :: i -> m (o, AutoM m i o) }

type Auto = AutoM Identity

runAuto :: Auto i o -> i -> (o, Auto i o)
runAuto auto i = runIdentity (runAutoM auto i)

instance Monad m => Functor (AutoM m i) where
  fmap f a2b = AutoM $ \a -> do
    (b, a2b2) <- runAutoM a2b a
    return (f b, fmap f a2b2)

instance Monad m => Category (AutoM m) where
  id        = AutoM $ \a -> return (a, id)
  b2c . a2b = AutoM $ \a -> do (b, a2b2) <- runAutoM a2b a
                               (c, b2c2) <- runAutoM b2c b
                               return (c, b2c2 . a2b2)
--
--            = Auto $ \a -> let (b, a2b2) = runAuto a2b a
--                               (c, b2c2) = runAuto b2c b in
--                               (c, b2c2 . a2c2)

instance Monad m => Arrow (AutoM m) where
  arr a2b   = AutoM $ \a      -> return (a2b a, arr a2b)
  first a2b = AutoM $ \(a, x) -> do (b, a2b2) <- runAutoM a2b a
                                    return ((b, x), first a2b2)

instance Monad m => ArrowChoice (AutoM m) where
  left a2b = AutoM $ \case (Left  b) -> do (c, next) <- runAutoM a2b b
                                           return (Left c, left next)
                           (Right d) -> return (Right d, left a2b)

instance MonadFix m => ArrowLoop (AutoM m) where
  loop a = AutoM $ \x -> do
    rec ((y, d), a') <- runAutoM a (x, d)
    return (y, loop a')

accumA :: (o -> i -> o) -> o -> Auto i o
accumA f a = AutoM $ \b -> return (f a b, accumA f $ f a b)

delayedAccumA :: (o -> i -> o) -> o -> Auto i o
delayedAccumA f a = AutoM $ \b -> return (a, delayedAccumA f $ f a b)

remember :: a -> Auto (Maybe a) a
remember = accumA remember'
  where remember' a Nothing  = a
        remember' _ (Just b) = b

convertM :: Monad m => Auto i o -> AutoM m i o
convertM a = AutoM $ \i -> let (o, nexta) = runIdentity $ runAutoM a i in return (o, convertM nexta)

convertState :: s -> AutoM (State s) i o -> Auto i o
convertState s0 am = AutoM $ \i -> let ((o1, nextsam), s1) = runState (runAutoM am i) s0
                                       in return (o1, convertState s1 nextsam)

getA :: AutoM (State o) i o
getA = AutoM $ \_ -> do
  s <- get
  return (s, getA)
