{-# LANGUAGE GADTs #-}


module Average where


import Prelude hiding (sum)
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
