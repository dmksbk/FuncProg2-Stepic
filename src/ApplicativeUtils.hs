{-# LANGUAGE RankNTypes #-}
module ApplicativeUtils where

import Control.Applicative (ZipList(..), liftA2)
import Data.Char
import Data.List
import Text.Layout.Table

--Двойственный оператор аппликации (<**>) из модуля Control.Applicative изменяет направление вычислений, не меняя порядок эффектов:
infixl 4 <**>
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))

--Определим оператор (<*?>) с той же сигнатурой, что и у (<**>), но другой реализацией:
infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)

--Для каких стандартных представителей класса типов Applicative можно привести цепочку аппликативных вычислений, дающую разный результат в зависимости от того, какой из этих операторов использовался?

--В следующих шести примерах вашей задачей будет привести такие контрпримеры для стандартных типов данных, для которых они существуют. Следует заменить аппликативное выражение в предложении in на выражение того же типа, однако дающее разные результаты при вызовах с (<??>) = (<**>) и (<??>) = (<*?>). Проверки имеют вид exprXXX (<**>) == exprXXX (<*?>) для различных имеющихся XXX. Если вы считаете, что контрпримера не существует, то менять ничего не надо.

exprMaybe :: (forall a b . Maybe a -> Maybe (a -> b) -> Maybe b) -> Maybe Int
exprMaybe op =
  let (<??>) = op
      infixl 4 <??>
  in Just 5 <??> Just (+2) -- NO
  -- in Just 5 <??> Just (+2) -- place for counterexample

exprList :: (forall a b . [a] -> [a -> b] -> [b]) -> [Int]
exprList op =
  let (<??>) = op
      infixl 4 <??>
  in [1,2] <??> [(+3),(+4),(+5)] -- YES
  -- in [1,2] <??> [(+3),(+4)] -- place for counterexample

exprZipList :: (forall a b . ZipList a -> ZipList (a -> b) -> ZipList b) -> ZipList Int
exprZipList op =
  let (<??>) = op
      infixl 4 <??>
  in ZipList [1,2] <??> ZipList [(+3),(+4)]  -- NO
  -- in ZipList [1,2] <??> ZipList [(+3),(+4)]  -- place for counterexample

exprEither :: (forall a b . Either String a -> Either String (a -> b) -> Either String b) -> Either String Int
exprEither op =
  let (<??>) = op
      infixl 4 <??>
  in Left "AA" <??> Left "bb"  -- YES
  -- in Left "AA" <??> Right (+1)  -- place for counterexample

exprPair :: (forall a b . (String,a) -> (String,a -> b) -> (String,b)) -> (String,Int)
exprPair op =
  let (<??>) = op
      infixl 4 <??>
  in ("AA", 3) <??> ("bb",(+1))  -- YES
  -- in ("AA", 3) <??> ("",(+1))  -- place for counterexample

exprEnv :: (forall a b . (String -> a) -> (String -> (a -> b)) -> (String -> b)) -> (String -> Int)
exprEnv op =
  let (<??>) = op
      infixl 4 <??>
  in (ord . head) <??> ((+) . length)  -- NO
  -- in length <??> (\_ -> (+5))  -- place for counterexample

-- Достаточно подумать где эффекты некоммутативны:
--  - для списков эффект это прямое произведение, оно меняет порядок элементов в упорядоченных парах
--  - для Monoid a => (,) a это mappend, зависит от коммутативности отдельно взятого mappend'а, но для строк это конкатенация и она некоммутативна если один из элементов непустая строка
--  - и наконец для Either e эффект это пропагация ошибки e, некоммутативна, если ошибки различны,
--  - отсюда кстати ясно почему Maybe коммутативен -- у него ошибки (отсутствующее значение) неразличимы.
--  - Эффект ZipList -- длина списка становится минимальной среди всех учавствующих в эффекте списков, коммутативен за счёт коммутативности минимума.
--  - А эффект стрелки с окружением это просто протаскивание стрелки с окружением всем элементам, понять коммутативность тут сложнее, но она интуитивно ощущается за счёт того что всюду окружение передается одно и то же, т.е. без изменений, а значит этот эффект можно применять в любом порядке.

f :: Int -> String -> String
f n str  =
  let l  = length str
      b  = max 0 $ div (n - l) 2 -- spaces before
      a  = max 0 $ n - l - b     -- spaces after
      sp = flip replicate ' '
  in  sp b ++ str ++ sp a

cw = 20 -- Cell Width
cc = 3  -- Cell Count
tw = cw * cc + cc - 1 -- Table Width

--f' :: Show s => s -> String
f' = f cw

csi :: [Int] -> String -> String
csi args code = "\ESC[" ++ intercalate ";" (map show args) ++ code
clearScreen = putStr $ csi [2] "J"

res :: String
res = tableString
       [def, def, def]
       asciiRoundS
       (titlesH ["expr", "<**>", "<*?>"])
       [ rowG ["exprMaybe", show $ exprMaybe (<**>), show $ exprMaybe (<*?>)]
       , rowG ["exprList", show $ exprList (<**>), show $ exprList (<*?>)]
       , rowG ["exprZipList", show . getZipList $ exprZipList (<**>), show . getZipList $ exprZipList (<*?>)]
       , rowG ["exprEither", show $ exprEither (<**>), show $ exprEither (<*?>)]
       , rowG ["exprPair", show $ exprPair (<**>), show $ exprPair (<*?>)]
       , rowG ["exprEnv \"Hello\"", show $ exprEnv (<**>) "Hello", show $ exprEnv (<*?>) "Hello"]
       ]

check :: IO()
check = do
  clearScreen
  putStrLn ""
  putStrLn $ f tw "Prog 1.2.4 - results for ApplicativeUtils"
  putStrLn $ replicate tw '='
  putStrLn $ f' "expr" ++ "|" ++ f' "<**>" ++ "|" ++ f' "<*?>"
  putStrLn $ replicate tw '-'
  putStrLn $ f' "exprMaybe" ++ "|"
          ++ f' (show $ exprMaybe (<**>)) ++ "|"
          ++ f' (show $ exprMaybe (<*?>))
  putStrLn $ f' "exprList" ++ "|"
          ++ f' (show $ exprList (<**>)) ++ "|"
          ++ f' (show $ exprList (<*?>))
  putStrLn $ f' "exprZipList" ++ "|"
          ++ f' (show . getZipList $ exprZipList (<**>)) ++ "|"
          ++ f' (show . getZipList $ exprZipList (<*?>))
  putStrLn $ f' "exprEither" ++ "|"
          ++ f' (show $ exprEither (<**>)) ++ "|"
          ++ f' (show $ exprEither (<*?>))
  putStrLn $ f' "exprPair" ++ "|"
          ++ f' (show $ exprPair (<**>)) ++ "|"
          ++ f' (show $ exprPair (<*?>))
  putStrLn $ f' "exprEnv \"Hello\" " ++ "|"
          ++ f' (show $ exprEnv (<**>) "Hello") ++ "|"
          ++ f' (show $ exprEnv (<*?>) "Hello")
  putStrLn $ replicate tw '-'
  putStrLn ""
  putStrLn res
