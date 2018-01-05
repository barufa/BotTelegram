module BST(
 BST,
 member,
 fromlistBy,
 searchBy
) where

import Data.List (sort,sortBy)

data BST a = Node a (BST a) (BST a) | Leaf deriving Show

fromlist :: [a] -> BST a
fromlist []  = Leaf
fromlist xs  = Node (head rs) (fromlist ls) (fromlist (tail rs))
               where l  = length xs`div`2
                     rs = drop l xs
                     ls = take l xs

fromlistBy :: (a->a->Ordering) -> [a] -> BST a
fromlistBy cmp xs = fromlist (sortBy cmp xs)

searchBy :: (a->b->Ordering) -> BST b -> a -> Maybe b
searchBy cmp Leaf y = Nothing
searchBy cmp (Node x l r) y = case cmp y x of
                                LT -> searchBy cmp l y
                                GT -> searchBy cmp r y
                                EQ -> Just x

member ::(a->a->Ordering) -> BST a -> a -> Bool
member cmp Leaf y = False
member cmp (Node x l r) y = case cmp y x of
                              LT -> member cmp l y
                              GT -> member cmp r y
                              EQ -> True
