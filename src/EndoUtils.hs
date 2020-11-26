module EndoUtils where

import Data.Monoid ( Endo(..) )

-- Prog 2.1.12
-- Реализуйте функцию, принимающую контейнер функций и последовательно сцепляющую элементы этого контейнера с помощью композиции, порождая в итоге эндоморфизм.

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo

e1 :: Endo Integer
e1 = mkEndo [(+5),(*3),(^2)]

e2 :: Endo Integer
e2 = mkEndo (42,(*3))

main :: IO ()
main = do
    print $ appEndo e1 2    -- should be 17
    print $ appEndo e2 2    -- should be 6

{- GHCi> e1 = mkEndo [(+5),(*3),(^2)]
GHCi> appEndo e1 2
17
GHCi> e2 = mkEndo (42,(*3))
GHCi> appEndo e2 2
6 -}