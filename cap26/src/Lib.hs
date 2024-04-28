module Lib where

import Control.Monad ((<=<), (>=>))

newtype MyMaybeT m a = MyMaybeT { runMyMaybeT :: m (Maybe a) }


instance Functor m => Functor (MyMaybeT m) where
  fmap f (MyMaybeT mma) = MyMaybeT $ (fmap . fmap) f mma

instance Applicative m => Applicative (MyMaybeT m) where
  pure = MyMaybeT . (pure . pure)

  MyMaybeT mab <*> MyMaybeT ma = MyMaybeT $ (<*>) <$> mab <*> ma

instance Monad m => Monad (MyMaybeT m) where
  return = pure

  MyMaybeT mma >>= f = MyMaybeT $ do -- : MyMaybe m b
    ma <- mma
    case ma of Nothing -> return Nothing
               Just a  -> (runMyMaybeT . f) a


newtype MyEitherT e m a = MyEitherT { runMyEitherT :: m (Either e a) }

instance Functor m => Functor (MyEitherT e m) where
  fmap f = MyEitherT . (fmap . fmap) f . runMyEitherT

instance Applicative m => Applicative (MyEitherT e m) where
  pure = MyEitherT . pure . pure
  --f <*> a = undefined
  -- m( eab <*>) <*> m( ea ) :: m (eb)
  -- m (ea -> eb) <*> m (ea)
  --MyEitherT meab <*> MyEitherT mea = MyEitherT $ (<*>) <$> meab <*> mea
  f <*> a = MyEitherT $ (<*>) <$> runMyEitherT f <*> runMyEitherT a

instance Monad m => Monad (MyEitherT e m) where
  return = pure
  v >>= f = MyEitherT $
    runMyEitherT v >>= (\ea -> case ea of Left e -> return (Left e)
                                          Right a -> runMyEitherT (f a)
                       )

-- transformer version of swapEither
swapEitherT :: (Functor m) => MyEitherT e m a -> MyEitherT a m e
swapEitherT u = MyEitherT $ swapEither <$> runMyEitherT u

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

myEitherT :: Monad m => (a -> m c) -> (b -> m c) -> MyEitherT a m b -> m c
myEitherT f g = myEither <=< runMyEitherT
  where myEither (Left e) = f e
        myEither (Right a) = g a


newtype MyReader r a = MyReader { runMyReader :: r -> a }
newtype MyReaderT r m a = MyReaderT { runMyReaderT :: r -> m a }


instance Functor (MyReader r) where
  fmap f r = MyReader $ f . runMyReader r


instance Functor m => Functor (MyReaderT r m) where
  -- f:: a->b
  -- r -> m a
  fmap f r = MyReaderT $ (f <$>) <$> runMyReaderT r


instance Applicative (MyReader r) where
  pure = MyReader . const
  (MyReader rab) <*> (MyReader ra) = MyReader $ \r -> rab r (ra r)


instance Applicative m => Applicative (MyReaderT r m) where
  pure = MyReaderT . const . pure

  (MyReaderT rmab) <*> (MyReaderT rma) = MyReaderT $ \r -> rmab r <*> rma r
  -- MyReaderT $ (<*>) <$> rmab <*> rma


instance Monad (MyReader r) where
  return = pure
  (MyReader ra) >>= faMrb = MyReader $ \r -> runMyReader (faMrb (ra r)) r


instance (Monad m) => Monad (MyReaderT r m) where
  return = pure

  MyReaderT rma >>= faMrmb = MyReaderT $ \r -> rma r >>= ($ r) . runMyReaderT . faMrmb
-- = MyReaderT $ \r -> do
--     a <- rma r
--     runMyReaderT (f a) r


newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a,s) }

fmapFst :: (a -> b) -> (a, c) -> (b, c)
fmapFst f = (,) <$> f . fst <*> snd

instance (Functor m)=> Functor (MyStateT s m) where
  fmap f msmas = MyStateT $ fmap (fmapFst f) <$> runMyStateT msmas

instance (Monad m) => Applicative (MyStateT s m) where
  pure a = MyStateT $ \s -> pure (a, s)

  MyStateT smabs <*> MyStateT smas = MyStateT $
    -- \s -> smabs s >>= \t -> let (f, s') = t in fmapFst f <$> smas s'
    smabs >=> \t -> let (f, s') = t in fmapFst f <$> smas s'

instance (Monad m) => Monad (MyStateT s m) where
  return = pure
  MyStateT smas >>= f = MyStateT $ \s -> smas s >>= \(a, s') -> (runMyStateT (f a) s')
