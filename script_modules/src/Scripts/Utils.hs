module Scripts.Utils where

import Data.Functor (void)

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

for_  :: Functor f => f a -> (a -> b) -> f ()
for_ x f = void $ fmap f x
