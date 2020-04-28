{-# LANGUAGE UnicodeSyntax #-}

module Arrn where

-- Сделайте типы данных Arr2 e1 e2 и Arr3 e1 e2 e3 представителями класса типов Functor:

newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

-- Эти типы инкапсулируют вычисление с двумя и тремя независимыми окружениями соответственно:

instance Functor (Arr2 e1 e2) where
  -- fmap f (Arr2 a) = Arr2 $ \ e1 -> (\ e2 -> f $ a e1 e2)
  -- fmap f (Arr2 a) = Arr2 $ \ e1 → f . a e1
  fmap f = Arr2 . (fmap . fmap $ f) . getArr2

instance Functor (Arr3 e1 e2 e3) where
  -- fmap f (Arr3 a) = Arr3 $ \ e1 -> (\ e2 -> (\ e3 -> f $ a e1 e2 e3))
  -- fmap f (Arr3 a) = Arr3 $ \ e1 e2 → f . a e1 e2
  fmap f = Arr3 . (fmap . fmap . fmap $ f) . getArr3
  
-- Prog 1.2.3  
-- Сделайте типы данных Arr2 e1 e2 и Arr3 e1 e2 e3 представителями класса типов Applicative с естественной семантикой двух и трех окружений:

instance Applicative (Arr2 e1 e2) where
  -- pure a = Arr2 $ \ e1 e2 -> a
  pure = Arr2 . const . const
  (Arr2 f) <*> (Arr2 v) = Arr2 $ \ e1 e2 -> f e1 e2 (v e1 e2) 

instance Applicative (Arr3 e1 e2 e3) where
  pure = Arr3 . const . const . const
  (Arr3 f) <*> (Arr3 v) = Arr3 $ \ e1 e2 e3 -> f e1 e2 e3 (v e1 e2 e3) 