module ZipListUtils where

import Control.Applicative (ZipList(ZipList), getZipList)

--В модуле Data.List имеется семейство функций zipWith, zipWith3, zipWith4,..:
  --GHCi> let x1s = [1,2,3]
  --GHCi> let x2s = [4,5,6]
  --GHCi> let x3s = [7,8,9]
  --GHCi> let x4s = [10,11,12]
  --GHCi> zipWith (\a b -> 2*a+3*b) x1s x2s
  --[14,19,24]
  --GHCi> zipWith3 (\a b c -> 2*a+3*b+5*c) x1s x2s x3s
  --[49,59,69]
  --GHCi> zipWith4 (\a b c d -> 2*a+3*b+5*c-4*d) x1s x2s x3s x4s
  --[9,15,21]
--Аппликативные функторы могут заменить всё это семейство

  --GHCi> getZipList $ (\a b -> 2*a+3*b) <$> ZipList x1s <*> ZipList x2s
  --[14,19,24]
  --GHCi> getZipList $ (\a b c -> 2*a+3*b+5*c) <$> ZipList x1s <*> ZipList x2s <*> ZipList x3s
  --[49,59,69]
  --GHCi> getZipList $ (\a b c d -> 2*a+3*b+5*c-4*d) <$> ZipList x1s <*> ZipList x2s <*>ZipList x3s <*> ZipList x4s
  --[9,15,21]
--Реализуйте операторы (>*<) и (>$<), позволяющие спрятать упаковку ZipList и распаковку getZipList:

(>$<)    :: (a -> b) -> [a] -> [b]
-- (>$<) f = getZipList . fmap f . ZipList
f >$< as = getZipList $ pure f <*> ZipList as

(>*<)    :: [a -> b] -> [a] -> [b]
fs >*< as = getZipList $ ZipList fs <*> ZipList as