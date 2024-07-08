module Set (Set (..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where

import Data.List (sort)
import Prelude hiding (null)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _ = False

member :: (Eq a) => a -> Set a -> Bool
member _ Empty = False
member a (Singleton b) = a == b
member a (Union b c) = member a b || member a c

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList = foldr (Union . singleton) Empty

toList :: Set a -> [a]
toList Empty = []
toList (Singleton a) = [a]
toList (Union a b) = toList a ++ toList b

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x : y : xs)
    | x == y = removeDuplicates (x : xs)
    | otherwise = x : removeDuplicates (y : xs)

toAscList :: (Ord a) => Set a -> [a]
toAscList = removeDuplicates . sort . toList

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union = Union

insert :: a -> Set a -> Set a
insert = union . singleton

instance (Ord a) => Eq (Set a) where
    (==) a b = toAscList a == toAscList b

instance Semigroup (Set a) where
    (<>) = union

instance Monoid (Set a) where
    mempty = empty

instance (Show a) => Show (Set a) where
    show = show . toList

instance Functor Set where
    fmap _ Empty = Empty
    fmap f (Singleton a) = Singleton (f a)
    fmap f (Union a b) = Union (fmap f a) (fmap f b)
