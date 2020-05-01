module PrsE where

--Рассмотрим более продвинутый парсер, позволяющий возвращать пользователю причину неудачи при синтаксическом разборе:
newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

-- Prog 1.4.3 - Реализуйте функцию satisfyE :: (Char -> Bool) -> PrsE Char таким образом, чтобы функция
charE :: Char -> PrsE Char
charE c = satisfyE (== c)

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE go where
  go []     = Left "unexpected end of input"
  go (x:xs) = if p x then Right (x, xs) else Left ("unexpected " ++ [x])

--обладала бы следующим поведением:
--  GHCi> runPrsE (charE 'A') "ABC"
--  Right ('A',"BC")
--  GHCi> runPrsE (charE 'A') "BCD"
--  Left "unexpected B"
--  GHCi> runPrsE (charE 'A') ""
--  Left "unexpected end of input"

-- Prog 1.4.4 - Applicative PrsE
-- Сделайте парсер PrsE из предыдущей задачи функтором и аппликативным функтором:
instance Functor PrsE where
  fmap f prs = PrsE go where
    go s = case runPrsE prs s of
      Left e -> Left e
      Right (v, xs) -> Right (f v, xs)

instance Applicative PrsE where
  pure v = PrsE go where
    go s = Right (v, s)
  fp <*> vp = PrsE go where
    go s = case runPrsE fp s of
      Left e -> Left e
      Right (f, s') -> case runPrsE vp s' of
        Left e -> Left e
        Right (v, s'') -> Right (f v, s'')