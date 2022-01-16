{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module Moi where

import Data.Tuple (swap)


newtype Moi s a = Moi { runMoi :: s -> (a, s) }

-- to have an equivalent of ``state``
moi :: (s -> (a, s)) -> Moi s a
moi = Moi


instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> mapFst f (g s)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)


instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure = Moi . (,)
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi $ \s ->
            let (fab, s') = f s
            in mapFst fab (g s')


-- Moi . (,) = \a -> Moi (a,) = \a -> Moi $ \s -> (a, s)
--        )
--       (   <-- cat tail?

{-
f :: s -> (a->b, s)
f = undefined

g :: s -> (a, s)
g = undefined
-}


instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g =
    Moi $ \s ->
      let (a, s') = f s
       in (runMoi $ g a) s'

{-
f :: s -> (a, s)
f = undefined

g :: a -> Moi s b
g = undefined
-}

-- Get the state within
get :: Moi s s
get = moi $ \s -> (s, s)
--get = moi $ (,) <$> id <*> id
  
-- update the state
put :: s -> Moi s ()
--put s = moi $ const ((), s)
put = moi . const . ((), )


-- evaluate the computation and return the final state
exec :: Moi s a -> s -> s
exec sa = snd . runMoi sa


-- evaluate the computation adn return the final value
eval :: Moi s a -> s -> a
eval sa = fst . runMoi sa

-- apply an action within the State
modify :: (s -> s) -> Moi s ()
modify f = do
  s <- get
  put (f s)
