module Tree where

import Data.List as L

import qualified Data.Maybe as Maybe
import qualified Data.Foldable as F


data BTree a = Empty | Branch (BTree a) a (BTree a) 
    deriving (Show)


indent :: Int -> String
indent = flip L.replicate '.'


str :: (Show a) => BTree a -> String
str = aux 0
    where
        aux k Empty = indent k ++ "@\n"
        aux k (Branch lt x rt) = indent k ++ show x ++ "\n" ++ aux (k + 1) lt ++ aux (k + 1) rt



-- Q1
-- a
exampleBT :: BTree Int
exampleBT = bt10
    where
        bt3 = Branch Empty 3 Empty
        bt5 = Branch Empty 5 Empty
        bt8 = Branch bt5 8 Empty
        bt4 = Branch bt3 4 bt8
        bt15 = Branch Empty 15 Empty
        bt20 = Branch bt15 20 Empty
        bt10 = Branch bt4 10 bt20

-- b
emptyBT :: BTree Int
emptyBT = Empty

-- c
size :: Num b => BTree a -> b
size Empty = 0
size (Branch left _ right) = 1 + size left + size right

-- d
maxBT :: (Ord a, Bounded a) => BTree a -> a
maxBT Empty  = minBound
-- Pattern matching on each part
maxBT (Branch left a right) = maximum [maxBT left, a, maxBT right]


-- e
minBT :: (Ord a, Bounded a) => BTree a -> a
minBT Empty  = minBound

-- f
-- on peut factoriser le code commun en passant en argument de formes currifiés des focntions min et max
minBT (Branch left a right) = minimum [minBT left, a, minBT right]
app :: ([a] -> a) -> a -> BTree a -> a
app _ funcB Empty =  funcB  
app funcA funcB (Branch l x r) = funcA [app funcA funcB l, x, app funcA funcB r]

minBT' :: (Ord a, Bounded a) => BTree a -> a
minBT' = app minimum maxBound
maxBT' :: (Ord a, Bounded a) => BTree a -> a
maxBT' = app maximum minBound


--- g

--- Nothing always lower than Just so it's works for maxBT''
--- minimum [1, Just, Nothing]
--- >> Nothing
maxBT'' :: (Ord a) => BTree a -> Maybe a
maxBT'' Empty = Nothing
maxBT'' (Branch l x r) = maximum [maxBT'' l, Just x, maxBT'' r]


--- Important de faire tous les cas car ça permet d'éviter de filtrer les 
--- nothing déja trouvé, car Nothing  < Just  
minBT'' :: (Ord a) => BTree a -> Maybe a
minBT'' Empty = Nothing 
--- if tree x
minBT'' (Branch Empty x Empty) = Just x
minBT'' (Branch l x Empty) = minimum [minBT'' l, Just x]
minBT'' (Branch Empty x r) = minimum [Just x, minBT'' r]
minBT'' (Branch l x r) = minimum [minBT'' l, Just x, minBT'' r]

--- h 
height :: (Ord b, Num b) => BTree a -> b
height Empty = 0
height (Branch l _ r) = 1 + max (height l) (height r)


--- i 
searchBT :: Eq a => BTree a -> a -> Bool
searchBT Empty  _ = False
searchBT (Branch l x r) myInt
    | x == myInt = True
    | otherwise = searchBT l myInt || searchBT r myInt


--- j
--toList :: BTree a -> [a]
--toList Empty  = []
--- on concaténe et on ajoute la valeur courante
--toList (Branch l currrentValue r) = currrentValue : (toList l ++ toList r)


--- k 
-- Même chose que toList
preVisit :: BTree a -> [a]
preVisit Empty = []
preVisit (Branch l x r) = x : preVisit l ++ preVisit r

toList :: BTree a -> [a]
toList = preVisit

inVisit :: BTree a -> [a]
inVisit (Branch l x r) = inVisit l ++ [x] ++ inVisit r

postVisit :: BTree a -> [a]
postVisit Empty = []
postVisit (Branch l x r) = postVisit l ++ postVisit r ++ [x]


--- l 
filterBT :: (a -> Bool) -> BTree a -> [a]
filterBT myFunc tree = filter myFunc(toList tree) 

--- m 
--- il faut recosntruire l'arbre binaire
mapBT :: (a -> b) -> BTree a -> BTree b
mapBT _ Empty = Empty
mapBT f (Branch l x r) = Branch (mapBT f l) (f x) (mapBT f r)


--- Q2

--- a
insertBST :: (Ord a) => BTree a -> a -> BTree a
insertBST Empty v = Branch Empty v Empty
insertBST (Branch l x r) v
    | v <= x = Branch (insertBST l v) x r
    | otherwise = Branch l x (insertBST r v)


--- b 
searchBST :: (Ord a) => BTree a -> a -> Bool
searchBST Empty _ = False;
searchBST (Branch left valueNode right) elt 
    | valueNode == elt = True
    | valueNode > elt  =  searchBST left elt -- naviguer a gauche si data > elt
    | otherwise = searchBST right elt -- naviguer à droit sinon si data < elt


--- c 
deleteLargestBST :: BTree a -> Maybe (a, BTree a)
deleteLargestBST Empty = Nothing 
deleteLargestBST (Branch left x Empty) = Just(x, left)
deleteLargestBST (Branch left x right) = case deleteLargestBST right of 
    Nothing -> Nothing
    Just(elt, right') -> Just(elt, Branch left x right')


--- d
--- Si on a trouve l’element que l’on doit supprimer, on peut
--- aller chercher l’element maximum du sous-arbre gauche pour le mettre a la place
deleteBST :: (Ord a) => BTree a -> a -> BTree a
deleteBST Empty _ = Empty 
deleteBST bst@(Branch Empty x right) y 
    | x == y    = right
    | y > x     = Branch Empty x (deleteBST right y)
    | otherwise = bst 
deleteBST (Branch left x right) y
    | x == y    = Branch left' x' right
    | y < x     = Branch (deleteBST left y) x right
    | otherwise = Branch left x (deleteBST right y)
    where 
        Just (x', left') = deleteLargestBST left


--- e 
deleteBST' :: (Ord a) => BTree a -> a -> Maybe (BTree a)
deleteBST' Empty _ = Just Empty 
deleteBST' (Branch Empty x right) y 
    | x == y    = Just right
    | y > x     = case deleteBST' right y of
        Nothing -> Nothing
        Just right' -> Just (Branch Empty x right')
    | otherwise = Nothing

deleteBST' (Branch left x right) y
    | x == y    = Just (Branch left' x' right)
    | y < x     = case deleteBST' left y of
        Nothing -> Nothing
        Just left' -> Just (Branch left' x right)
    | otherwise = case deleteBST' right y of 
        Nothing -> Nothing 
        Just right' -> Just (Branch left x right')
    where 
        Just (x', left') = deleteLargestBST left



---- Q3
--- Foldable left example 
mkBST :: (Ord a) => [a] -> BTree a
mkBST = F.foldl insertBST emptyBT


