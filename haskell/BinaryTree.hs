module BinaryTree (
    empty,
    singleton,
    push,
    pop,
    null',
    contains,
    find,
    preOrder,
    inOrder,
    posOrder,
    size,
    height,
    fromList,
    toList,
) where

import Data.Maybe (isJust)

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving Eq

empty :: BinaryTree a
empty = Empty

singleton :: Ord a => a -> BinaryTree a
singleton x = Node x Empty Empty

push :: Ord a => a -> BinaryTree a -> BinaryTree a
push x Empty = singleton x
push x (Node n l r)
    | x < n = Node n (push x l) r
    | otherwise = Node n l (push x r)

pop :: Ord a => a -> BinaryTree a -> BinaryTree a
pop _ Empty = Empty
pop x (Node n l r)
    | x == n = go (Node n l r)
    | x < n = Node n (pop x l) r
    | otherwise = Node n l (pop x r)
    where
        go (Node n l Empty) = l
        go (Node n Empty r) = r
        go (Node n l r) =
            let maxLeft = go' l
            in Node maxLeft (pop maxLeft l) r
        go' (Node n _ Empty) = n
        go' (Node _ _ r) = go' r

null' :: BinaryTree a -> Bool
null' Empty = True
null' _ = False

contains :: Ord a => a -> BinaryTree a -> Bool
contains x = isJust . find x

find :: Ord a => a -> BinaryTree a -> Maybe a
find _ Empty = Nothing
find x (Node n l r)
    | x == n = Just n
    | x < n = find x l
    | otherwise = find x r

preOrder :: (a -> b) -> BinaryTree a -> [b]
preOrder _ Empty = []
preOrder f (Node x l r) = [f x] ++ preOrder f l ++ preOrder f r

inOrder :: (a -> b) -> BinaryTree a -> [b]
inOrder _ Empty = []
inOrder f (Node x l r) = inOrder f l ++ [f x] ++ inOrder f r

posOrder :: (a -> b) -> BinaryTree a -> [b]
posOrder _ Empty = []
posOrder f (Node x l r) = posOrder f l ++ posOrder f r ++ [f x]

size :: Ord a => BinaryTree a -> Int
size = go 0
    where
        go acc Empty = acc
        go acc (Node x l r) = 1 + go acc l + go acc r

height :: Ord a => BinaryTree a -> Int
height = go 0
    where
        go acc Empty = acc
        go acc (Node x l r) =
            let h1 = go acc l
                h2 = go acc r
            in 1 + max h1 h2

fromList :: Ord a => [a] -> BinaryTree a
fromList = go Empty
    where
        go acc [] = acc
        go acc (x:xs) = go (push x acc) xs

toList :: Ord a => BinaryTree a -> [a]
toList = inOrder id

instance Show a => Show (BinaryTree a) where
    show Empty = "null"
    show (Node n l r) = "(" ++ show l ++ ") <- " ++ show n ++ " -> (" ++ show r ++ ")"

instance Functor BinaryTree where
    fmap _ Empty = Empty
    fmap f (Node n l r) = Node (f n) (fmap f l) (fmap f r)
