{-# LANGUAGE UnicodeSyntax #-}

module Arrn where

-- Сделайте типы данных Arr2 e1 e2 и Arr3 e1 e2 e3 представителями класса типов Functor:

newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

-- Эти типы инкапсулируют вычисление с двумя и тремя независимыми окружениями соответственно:

instance Functor (Arr2 e1 e2) where
  -- fmap f (Arr2 a) = Arr2 $ \ e1 -> (\ e2 -> f $ a e1 e2)
  fmap f (Arr2 a) = Arr2 $ \ e1 → f . a e1

instance Functor (Arr3 e1 e2 e3) where
  -- fmap f (Arr3 a) = Arr3 $ \ e1 -> (\ e2 -> (\ e3 -> f $ a e1 e2 e3))
  fmap f (Arr3 a) = Arr3 $ \ e1 e2 → f . a e1 e2