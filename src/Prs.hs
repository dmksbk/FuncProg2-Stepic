module Prs where

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