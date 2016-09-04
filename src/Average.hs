{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}


module Average where


import Prelude hiding (sum)
import Control.Applicative
import Data.Ratio


data Aggregation a b where
  Agg :: x -> (x -> a -> x) -> (x -> b) -> Aggregation a b


instance Functor (Aggregation a) where
  fmap f (Agg nil cons fin) =
    Agg nil cons (f . fin)


instance Applicative (Aggregation a) where
  pure a =
    Agg () (const (const ())) (const a)

  Agg lnil lcons lfin <*> Agg rnil rcons rfin =
    Agg (lnil, rnil) (\(lx, rx) a -> (lcons lx a, rcons rx a)) (\(lx, rx) -> lfin lx (rfin rx))


------


sum :: Integral a => Aggregation a a
sum =
  Agg 0 (+) id


count :: Integral b => Aggregation a b
count =
  Agg 0 (const . succ) id


average :: Integral a => Aggregation a (Ratio a)
average =
  (%) <$> sum <*> count


------


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


-----


newtype ParallelAggregation a b = PAgg (Ap (Aggregation a) b)
  deriving (Functor, Applicative)


toParallel :: Aggregation a b -> ParallelAggregation a b
toParallel agg =
  PAgg (liftAp agg)


fromParallel :: ParallelAggregation a b -> Aggregation a b
fromParallel (PAgg ap) =
  retractAp ap
