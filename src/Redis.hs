{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module Redis where


import Free


type Key = String


data Op a where
  Get :: Key -> Op String
  Set :: Key -> String -> Op ()
  Del :: Key -> Op ()


newtype Tx a = Tx (Ap Op a)
  deriving (Functor, Applicative)


get :: Key -> Tx String
get key =
  Tx (liftAp (Get key))


set :: Key -> String -> Tx ()
set key val =
  Tx (liftAp (Set key val))


del :: Key -> Tx ()
del key =
  Tx (liftAp (Del key))
