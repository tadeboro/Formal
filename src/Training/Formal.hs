{- |
Module      : Training.Formal
Description : Simple module for working with formal power series
Copyright   : (c) Tadej Borovšak, 2014
License     : GPL-3
Maintainer  : tadeboro@gmail.com
Stability   : experimental
Portability : POSIX

A library of basic functionality needed to work with formal power series.
The aim of this library is to provide essential building blocks for power
series arithmetics.
-}
module Training.Formal where

infixr 5 :+:
-- | Data type that represents power series
data Formal a = a :+: Formal a

-- | Get first coeficient in series
head :: Formal a -> a
head (x :+: _) = x

-- | Get all but first coefficient in series
tail :: Formal a -> Formal a
tail (_ :+: xs) = xs

-- | Zero
zero :: Num a => Formal a
zero = 0 :+: zero

-- | Construct formal power series from coefficient list
fromList :: Num a => [a] -> Formal a
fromList [] = zero
fromList (x : xs) = x :+: fromList xs

-- | Convert formal power series to coefficient list
toList :: Formal a -> [a]
toList (x :+: xs) = x : toList xs

infixl 7 .*
-- | Multiply series by scalar value
(.*) :: Num a => a -> Formal a -> Formal a
a .* (x :+: xs) = a * x :+: a .* xs

-- | Compose 2 power series
compose :: Num a => Formal a -> Formal a -> Formal a
compose (x :+: xs) (y :+: ys) = x :+: ys * (compose xs (0 :+: ys))

-- | Calculate derivative
deriv :: Num a => Formal a -> Formal a
deriv (x :+: xs) = (deriv' xs 1)
    where deriv' (x :+: xs) n = n * x :+: (deriv' xs (n + 1))

-- | Integrate series
integral :: Fractional a => Formal a -> Formal a
integral xs = 0 :+: (int' xs 1)
    where int' (x :+: xs) n = x / n :+: (int' xs (n + 1))

-- | Series representation of 'exp' function
exps = 1 + (integral exps)

-- | Series representation of 'sin' and 'cos' functions
sins = integral coss
coss = 1 - (integral sins)

-- | Evaluate series until n-th element
evals a n xs = evals' a n xs 0 1
    where evals' _ 0 _ r _ = r
          evals' a n (x :+: xs) r an =
              evals' a (n - 1) xs (r + an * x) (a * an)

instance Functor Formal where
    fmap f (x :+: xs) = f x :+: fmap f xs

instance Show a => Show (Formal a) where
    show = show . take 20 . toList

instance Num a => Num (Formal a) where
    (x :+: xs) + (y :+: ys) = x + y :+: xs + ys
    (x :+: xs) * cy@(y :+: ys) = x * y :+: x .* ys + xs * cy
    abs = undefined
    signum = undefined
    negate (x :+: xs) = negate x :+: negate xs
    fromInteger x = fromInteger x :+: zero

instance (Eq a, Fractional a) => Fractional (Formal a) where
    fromRational x = fromRational x :+: zero
    (0 :+: xs) / (0 :+: ys) = xs / ys
    (x :+: xs) / cy@(y :+: ys) = q :+: (xs - q .* ys) / cy
        where q = x / y

instance (Eq a, Fractional a) => Floating (Formal a) where
    sqrt (0 :+: 0 :+: xs) = 0 :+: (sqrt xs)
    sqrt (1 :+: xs) = qs
        where qs = 1 + integral ((deriv (1 :+: xs)) / (2 .* qs))
    pi = undefined
    exp = undefined
    log = undefined
    sin = undefined
    cos = undefined
    asin = undefined
    atan = undefined
    acos = undefined
    sinh = undefined
    cosh = undefined
    asinh = undefined
    atanh = undefined
    acosh = undefined
