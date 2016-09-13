module Result where

import Prelude hiding (Either(..))


-- Either


data Either a b =
    Left a
  | Right b


instance Functor (Either a) where
  fmap f x =
    case x of
      Left a ->
        Left a
      Right b ->
        Right (f b)


instance Applicative (Either a) where
  pure =
    Right

  xf <*> x =
    case (xf, x) of
      (Right f, Right b) ->
        Right (f b)
      (Left a, _) ->
        Left a
      (_, Left a) ->
        Left a


-- Result


data Result a b =
    Err (a, [a])
  | Ok b


instance Functor (Result a) where
  fmap f r =
    case r of
      Err (a, as) ->
        Err (a, as)
      Ok b ->
        Ok (f b)


instance Applicative (Result a) where
  pure =
    Ok

  xf <*> x =
    case (xf, x) of
      (Ok f, Ok b) ->
        Ok (f b)
      (Err (a, as), Err (a', as')) ->
        Err (a, as ++ [a'] ++ as')
      (Err (a, as), _) ->
        Err (a, as)
      (_, Err (a, as)) ->
        Err (a, as)
