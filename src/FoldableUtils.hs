module FoldableUtils where

import Data.Foldable ()
import Data.Monoid
    ( Any(Any, getAny), All(All, getAll), Last(Last, getLast) )
import Data.Char ( isDigit )

-- Question 2.1.7
-- Предположим, что определены следующие функции:
f :: [Integer] -> Maybe Bool
f = Just . getAny . foldMap Any . fmap even

g :: [Maybe a] -> Maybe a
g = getLast . foldMap Last

h :: [Char] -> Maybe Bool
h = Just . getAll . foldMap All . map isDigit

-- Сопоставьте их вызовы и результаты этих вызовов. Предполагается, что загружены все модули, требующиеся для доступа к использованным функциям и конструкторам данных.
main = do
    print $ f [3,5,6]
    print $ g [Just True,Just False,Nothing]
    print $ h ['3', '5', '6']
