module Cmps3 where

-- Сделайте тип
newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) }
  deriving (Eq,Show)
-- представителем класса типов Functor при условии, что первые его три параметра являются функторами:

instance (Functor f1, Functor f2, Functor f3) => Functor (Cmps3 f1 f2 f3) where
  -- fmap h (Cmps3 x) = Cmps3 $ fmap (fmap (fmap h)) x
  fmap h = Cmps3 . fmap (fmap (fmap h)) . getCmps3
