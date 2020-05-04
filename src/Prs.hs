module Prs where

import Control.Applicative (Alternative, empty, (<|>))
import Data.Maybe (isJust)

-- Предположим, тип парсера определен следующим образом:
newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

-- Prg 1.4.1
-- Сделайте этот парсер представителем класса типов Functor.
instance Functor Prs where
  fmap f p = Prs go where
    go s =
      let res = runPrs p s
      in  case res of
        Nothing -> Nothing
        Just (a, s') -> Just (f a, s')

-- Реализуйте также парсер anyChr :: Prs Char, удачно разбирающий и возвращающий любой первый символ любой непустой входной строки.
anyChr :: Prs Char
anyChr = Prs go where
  go []     = Nothing
  go (x:xs) = Just (x, xs)

-- Prg 1.4.2
-- Сделайте парсер Prs a из предыдущей задачи аппликативным функтором с естественной для парсера семантикой:

instance Applicative Prs where
  pure a = Prs go where
    go s = Just (a, s)
  fa <*> va = Prs go where
    go s = case runPrs fa s of
      Nothing -> Nothing
      Just (f, s') -> case runPrs va s' of
        Nothing -> Nothing
        Just (v, s'') -> Just (f v, s'')

-- Another solution using monad comprehension
--{-# LANGUAGE MonadComprehensions #-}
--
--instance Applicative Prs where
--  pure v  = Prs fun where
--    fun xs = Just (v, xs)
--  fp <*> vp = Prs fun where
--    fun s = [ (f v, s'')
--            | (f, s')    <- runPrs fp s
--            , (v, s'')   <- runPrs vp s'
--            ]

-- Prog 1.4.5
-- Сделайте парсер Prs представителем класса типов Alternative с естественной для парсера семантикой:
 --Представители для классов типов Functor и Applicative уже реализованы. Функцию char :: Char -> Prs Char включать в решение не нужно, но полезно реализовать для локального тестирования.
 
instance Alternative Prs where
  empty = Prs (const Nothing)
  pa <|> pb = Prs go where
    go s = 
      let ra = runPrs pa s
      in if isJust ra then ra else runPrs pb s

satisfy :: (Char -> Bool) -> Prs Char
satisfy p = Prs go where
  go []     = Nothing
  go (x:xs) = if p x then Just (x, xs) else Nothing

char :: Char -> Prs Char
char = satisfy . (==)

-- Prog 1.4.6
-- Реализуйте для парсера Prs парсер-комбинатор many1 :: Prs a -> Prs [a], который отличается от many только тем, что он терпит неудачу в случае, когда парсер-аргумент неудачен на начале входной строки. Функцию char :: Char -> Prs Char включать в решение не нужно, но полезно реализовать для локального тестирования.

many :: Prs a -> Prs [a]
many p = (:) <$> p <*> many p <|> pure []

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> (many1 p <|> pure [])