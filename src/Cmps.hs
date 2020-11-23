{-# LANGUAGE TypeOperators #-}

module Cmps where

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps x) = Cmps $ fmap (fmap h) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
  (Cmps f) <*> (Cmps v) =
    error "<*> for Applicative (f |.| g) is not defined yet"

-- Prog 1.5.3 - Напишите универсальные функции, позволяющие избавляться от синтаксического шума для композиции нескольких функторов:

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = fmap getCmps . getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
-- unCmps4 = fmap (fmap getCmps) . fmap getCmps . getCmps
unCmps4 = fmap (fmap getCmps . getCmps) . getCmps