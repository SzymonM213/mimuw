{-# LANGUAGE TypeApplications #-}

module Graph where

import Data.List (sort)
import Set (Set)
import qualified Set as Set

class Graph g where
    empty :: g a
    vertex :: a -> g a
    union :: g a -> g a -> g a
    connect :: g a -> g a -> g a

data Relation a = Relation {domain :: Set a, relation :: Set (a, a)}
    deriving (Eq, Show)

data Basic a
    = Empty
    | Vertex a
    | Union (Basic a) (Basic a)
    | Connect (Basic a) (Basic a)

connectRelation :: Relation a -> Relation a -> Set (a, a)
connectRelation a b =
    Set.fromList
        [(x, y) | x <- Set.toList (domain a), y <- Set.toList (domain b)]

instance Graph Relation where
    empty = Relation{domain = Set.empty, relation = Set.empty}
    vertex a = Relation{domain = Set.singleton a, relation = Set.empty}
    union a b =
        Relation
            { domain = Set.union (domain a) (domain b)
            , relation = Set.union (relation a) (relation b)
            }
    connect a b =
        Relation
            { domain = Set.union (domain a) (domain b)
            , relation =
                Set.union
                    (Set.union (relation a) (relation b))
                    (connectRelation a b)
            }

removeGraphDuplicates :: (Ord a) => Relation a -> Relation a
removeGraphDuplicates (Relation d r) =
    Relation
        { domain = Set.fromList (Set.toAscList d)
        , relation = Set.fromList (Set.toAscList r)
        }

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+) a b = removeGraphDuplicates (union a b)
    (*) a b = removeGraphDuplicates (connect a b)
    signum = const empty
    abs = id
    negate = id

instance Graph Basic where
    empty = Empty
    vertex = Vertex
    union = Union
    connect = Connect

instance (Ord a) => Eq (Basic a) where
    a == b = fromBasic @Relation a == fromBasic @Relation b

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+) = union
    (*) = connect
    signum = const empty
    abs = id
    negate = id

instance Semigroup (Basic a) where
    (<>) = union

instance Monoid (Basic a) where
    mempty = Empty

fromBasic :: (Graph g) => Basic a -> g a
fromBasic Empty = Graph.empty
fromBasic (Vertex a) = vertex a
fromBasic (Union a b) = union (fromBasic a) (fromBasic b)
fromBasic (Connect a b) = connect (fromBasic a) (fromBasic b)

ascListsDiff :: (Ord a) => [a] -> [a] -> [a]
ascListsDiff [] _ = []
ascListsDiff xs [] = xs
ascListsDiff (x : xs) (y : ys)
    | x == y = ascListsDiff xs ys
    | x > y = ascListsDiff (x : xs) ys
    | otherwise = x : ascListsDiff xs (y : ys)

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x : y : xs)
    | x == y = removeDuplicates (x : xs)
    | otherwise = x : removeDuplicates (y : xs)

isolatedVertices :: (Ord a) => Relation a -> [a]
isolatedVertices (Relation d r) =
    ascListsDiff
        (Set.toAscList d)
        (removeDuplicates $ sort $ concat [[x, y] | (x, y) <- Set.toList r])

instance (Ord a, Show a) => Show (Basic a) where
    show g =
        "edges "
            ++ show (Set.toList (relation relationG))
            ++ " + vertices "
            ++ show (isolatedVertices relationG)
      where
        relationG = removeGraphDuplicates (fromBasic @Relation g)

--  | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1 * 2 + 2 * (3 + 4) + (3 + 4) * 5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot g =
    "digraph {\n"
        ++ concatMap
            (\(x, y) -> show x ++ " -> " ++ show y ++ ";\n")
            (Set.toList (relation (fromBasic @Relation g)))
        ++ concatMap (\x -> show x ++ ";\n") (isolatedVertices (fromBasic @Relation g))
        ++ "}"

instance Functor Basic where
    fmap _ Empty = Empty
    fmap f (Vertex a) = Vertex (f a)
    fmap f (Union a b) = Union (fmap f a) (fmap f b)
    fmap f (Connect a b) = Connect (fmap f a) (fmap f b)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: (Eq a) => a -> a -> a -> Basic a -> Basic a
mergeV a b c = (>>= \x -> (if (x == a) || (x == b) then Vertex c else Vertex x))

instance Applicative Basic where
    pure = Vertex
    (<*>) Empty _ = Empty
    (<*>) (Vertex f) x = fmap f x
    (<*>) (Union f g) x = Union (f <*> x) (g <*> x)
    (<*>) (Connect f g) x = Connect (f <*> x) (g <*> x)

instance Monad Basic where
    (>>=) Empty _ = Empty
    (>>=) (Vertex a) f = f a
    (>>=) (Union a b) f = Union (a >>= f) (b >>= f)
    (>>=) (Connect a b) f = Connect (a >>= f) (b >>= f)

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: (Eq a) => a -> a -> a -> Basic a -> Basic a
splitV a b c = (>>= \x -> if x == a then Union (Vertex b) (Vertex c) else Vertex x)
