module Prs where

-- Предположим, тип парсера определен следующим образом:
newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

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

