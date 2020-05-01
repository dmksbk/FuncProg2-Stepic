module PrsE where

--Рассмотрим более продвинутый парсер, позволяющий возвращать пользователю причину неудачи при синтаксическом разборе:
newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

--Реализуйте функцию satisfyE :: (Char -> Bool) -> PrsE Char таким образом, чтобы функция
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

