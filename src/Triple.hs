module Triple where

-- Следующий тип данных задает гомогенную тройку элементов, которую можно рассматривать как трехмерный вектор:

data Triple a = Tr a a a  deriving (Eq,Show)

-- Сделайте этот тип функтором и аппликативным функтором с естественной для векторов семантикой покоординатного применения:

instance Functor Triple where
    fmap f (Tr a b c) = Tr (f a) (f b) (f c)
    
instance Applicative Triple where
    pure a = Tr a a a
    (Tr fa fb fc) <*> (Tr a b c) = Tr (fa a) (fb b) (fc c)

-- Prog 2.1.3
-- Сделайте тип Triple представителем класса типов Foldable:

instance Foldable Triple where
    foldr f ini (Tr a b c) = a `f` (b `f` (c `f` ini))
    foldl f ini (Tr a b c) = ((ini `f` a) `f` b) `f` c
