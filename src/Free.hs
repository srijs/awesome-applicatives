{-# LANGUAGE GADTs #-}

module Free where


import Control.Applicative


data Ap f a where
  Pure :: a -> Ap f a
  Ap :: f a -> Ap f (a -> b) -> Ap f b


instance Functor (Ap f) where
  fmap f x =
    case x of
      Pure a ->
        Pure (f a)

      Ap y z ->
        Ap y ((\g -> f . g) <$> z)


instance Applicative (Ap f) where
  pure =
    Pure

  l <*> r =
    case l of
      Pure f ->
        fmap f r

      Ap x y ->
        Ap x (flip <$> y <*> r)


liftAp :: f a -> Ap f a
liftAp x =
  Ap x (Pure id)


retractAp :: Applicative f => Ap f a -> f a
retractAp x =
  case x of
    Pure a ->
      pure a

    Ap y z ->
      y <**> retractAp z
