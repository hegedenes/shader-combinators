{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds #-}

import Control.Applicative
import Control.Arrow

f1 :: (Num a) => (a,b,c) -> a -> a
f1 (x,_,_) y = y + x

g1 :: (Num a) => (a,b,c) -> a -> a
g1 (x,_,_) y = y - x

-- Composition w/ arrows (Control.Arrow)
h1 :: (Num a) => (a,b,c) -> a -> a
h1 = (f1 &&& g1) >>> (uncurry (>>>))

h2 :: (Num a) => (a,b,c) -> a -> a
h2 = (f1 &&& g1) >>> (uncurry (&&&)) >>> (uncurry (+) .)

-- Composition w/ applicative functors (Control.Applicative)
i1 :: (Num a) => (a,b,c) -> a -> a
i1 x = (g1 x) <$> (f1 x)

i2 :: (Num a) => (a,b,c) -> a -> a
i2 x = (+) <$> (f1 x) <*> (g1 x)


----------------------------------------------------


f :: (Num a) => a -> a -> a
f a b = b + a

g :: (Num a) => a -> a -> a
g a b = b - a

-- h = (f &&& g) >>> uncurry (>>>)
h = (f &&& g) ==< (>>>)

h3 a b = g a (f a b)

k :: (Num a) => (a -> (a,a)) -> a -> a
k f = uncurry (+) . f

infixl 3 ==<, >==
(==<) f g = f >>> uncurry g
(>==) f g = f >>> ((uncurry g) .)

j :: (Num a) => a -> a -> a
j = (f &&& g) ==< (&&&) >== (+)

j2 :: (Num a) => a -> a -> a
j2 = (f &&& g) >>> uncurry (&&&) >>> ((uncurry (+)) .)
