module Stack (
    empty,
    singleton,
    fromList,
    toList,
    push,
    pop,
    peek,
    find,
    contains,
    isEmpty,
    size,
    fmap,
    filter',
    foldl'
) where

{- 
TODO
impl Monad
-}

data Stack a = Empty | Node a (Stack a) deriving Eq

empty :: Stack a
empty = Empty

singleton :: a -> Stack a
singleton x = Node x Empty

fromList :: [a] -> Stack a
fromList xs = go xs Empty
    where
        go [] acc = acc
        go (x:xs) acc = go xs (push x acc)

toList :: Stack a -> [a]
toList = go []
    where
        go acc Empty = acc
        go acc (Node x xs) = acc ++ [x] ++ go acc xs

push :: a -> Stack a -> Stack a
push x Empty = Node x Empty
push x xs = Node x xs

pop :: Stack a -> (Stack a, Maybe a)
pop Empty = (Empty, Nothing)
pop (Node x xs) = (xs, Just x)

peek :: Stack a -> Maybe a
peek Empty = Nothing
peek (Node x xs) = Just x

find :: Eq a => a -> Stack a -> Int
find n = go 0
    where
        go acc Empty = -1
        go acc (Node x xs)
            | x == n = acc
            | otherwise = go (acc+1) xs

contains :: Eq a => a -> Stack a -> Bool
contains n xs = find n xs /= -1

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False

size :: Stack a -> Int
size = go 0
    where
        go acc Empty = acc
        go acc (Node x xs) = 1 + go acc xs

filter' :: (a -> Bool) -> Stack a -> Stack a
filter' f = go Empty
    where
        go acc Empty = acc
        go acc (Node x xs)
            | f x = push x (go acc xs)
            | otherwise = go acc xs

foldl' :: (b -> a -> b) -> b -> Stack a -> b
foldl' f acc = go acc
    where
        go acc Empty = acc
        go acc (Node x xs) = go (f acc x) xs

instance Functor Stack where
    fmap _ Empty = Empty
    fmap f (Node x xs) = Node (f x) (fmap f xs)

instance Show a => Show (Stack a) where
    show Empty = "null"
    show (Node x xs) = show x ++ " -> " ++ show xs

instance Applicative Stack where
    pure x = Node x Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    fs <*> xs = go fs xs xs
        where
            go (Node f Empty) (Node x Empty) _ = Node (f x) Empty
            go (Node f fs) (Node x Empty) xs' = Node (f x) (go fs xs' xs')
            go gs@(Node f fs) (Node x xs) xs' = Node (f x) (go gs xs xs')

main :: IO()
main = do
    let stack = fromList [0..5]
    print stack
    
    let stack' = push 8 $ push 7 $ push 6 stack
    print stack'

    let (stack'', elem) = pop stack'
    print stack''
    print elem

    print $ peek stack''
    print $ size stack''

    print $ toList stack''
    print $ (\i -> i*i) <$> stack''
    print $ filter' even stack''

    print $ contains 5 stack
    print $ find 3 stack
    
    print $ foldl' (+) 0 stack
    print $ 'a' <$ stack''
