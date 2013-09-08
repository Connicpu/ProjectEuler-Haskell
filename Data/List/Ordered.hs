-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Ordered
-- Copyright   :  (c) 2009-2010 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  leon@melding-monads.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements bag and set operations on ordered lists.
-- Except for variations of the  'sort' and 'isSorted' functions,
-- every function assumes that any list arguments are sorted lists.
-- Assuming this precondition is met,  every resulting list is also
-- sorted.
--
-- Note that these functions handle multisets, and are left-biased.
-- Thus, even assuming the arguments are sorted,  'isect' does not always
-- return the same results as Data.List.intersection,  due to multiplicity.
--
-----------------------------------------------------------------------------

module  Data.List.Ordered
     (
        -- * Predicates
        member, memberBy, has, hasBy
     ,  subset, subsetBy
     ,  isSorted, isSortedBy

        -- * Insertion Functions
     ,  insertBag, insertBagBy
     ,  insertSet, insertSetBy

        -- * Set-like operations
     ,  isect, isectBy
     ,  union, unionBy
     ,  minus, minusBy
     ,  xunion, xunionBy
     ,  merge, mergeBy
     ,  mergeAll  , mergeAllBy
     ,  unionAll  , unionAllBy

        -- * Lists to Ordered Lists
     ,  nub, nubBy
     ,  sort, sortBy
     ,  sortOn, sortOn'
     ,  nubSort, nubSortBy
     ,  nubSortOn, nubSortOn'

     )  where

import Data.List(sort,sortBy)

-- |  The 'isSorted' predicate returns 'True' if the elements of a list occur in non-descending order,  equivalent to @'isSortedBy' ('<=')@.
isSorted :: Ord a => [a] -> Bool
isSorted = isSortedBy (<=)

-- |  The 'isSortedBy' function returns 'True' iff the predicate returns true
-- for all adjacent pairs of elements in the list.
isSortedBy :: (a -> a -> Bool) -> [a] -> Bool
isSortedBy lte = loop
  where
    loop []       = True
    loop [_]      = True
    loop (x:y:zs) = (x `lte` y) && loop (y:zs)

-- |  The 'member' function returns 'True' if the element appears in the
-- ordered list.
member :: Ord a => a -> [a] -> Bool
member = memberBy compare

-- |  The 'memberBy' function is the non-overloaded version of 'member'.
memberBy :: (a -> a -> Ordering) -> a -> [a] -> Bool
memberBy cmp x = loop
  where
    loop []     = False
    loop (y:ys) = case cmp x y of
                    LT -> False
                    EQ -> True
                    GT -> loop ys

-- |  The 'has' function returns 'True' if the element appears in the list;
-- it is equivalent to 'member' except the order of the arguments is reversed,
-- making it a function from an ordered list to its characteristic function.
has :: Ord a => [a] -> a -> Bool
has xs y = memberBy compare y xs

-- |  The 'hasBy' function is the non-overloaded version of 'has'.
hasBy :: (a -> a -> Ordering) -> [a] -> a -> Bool
hasBy cmp xs y = memberBy cmp y xs

-- |  The 'insertBag' function inserts an element into a list.  If the element
-- is already there,  then another copy of the element is inserted.
insertBag :: Ord a => a -> [a] -> [a]
insertBag = insertBagBy compare

-- |  The 'insertBagBy' function is the non-overloaded version of 'insertBag'.
insertBagBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBagBy cmp = loop
  where
    loop x [] = [x]
    loop x (y:ys)
      = case cmp x y of
         GT -> y:loop x ys
         _  -> x:y:ys

-- |  The 'insertSet' function inserts an element into an ordered list.
-- If the element is already there,  then the element replaces the existing
-- element.
insertSet :: Ord a => a -> [a] -> [a]
insertSet = insertSetBy compare

-- |  The 'insertSetBy' function is the non-overloaded version of 'insertSet'.
insertSetBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertSetBy cmp = loop
  where
    loop x [] = [x]
    loop x (y:ys) = case cmp x y of
            LT -> x:y:ys
            EQ -> x:ys
            GT -> y:loop x ys

{-
-- This function is moderately interesting,  as it encompasses all the
-- "Venn diagram" functions on two sets. (though not merge;  which isn't
-- a set function)

-- However, it doesn't seem that useful,  considering that of the 8 possible
-- functions,  there are only 4 interesting variations:  isect, union, minus,
-- and xunion.  Due to interactions with GHC's optimizer,  coded separately,
-- these have a smaller combined object code size than the object code size
-- for genSectBy.  (Or,  turn off certain optimizations and lose speed.)

-- Each individual object code can be recovered from genSectBy via GHC's
-- inliner and constant propagation;  but this doesn't save much in terms
-- of source code size and reduces portability.

-- Note that the Static Argument Transformation is necessary for this to work
-- correctly;  inlining genSectBy allows for cmp and p to be inlined as well,
-- or at least eliminate some indirect jumps.  All of the *By functions in
-- this module follow this idiom for this reason.

genSectBy :: (a -> a -> Ordering)
          -> (Bool -> Bool -> Bool)
          -> [a] -> [a] -> [a]
genSectBy cmp p = loop
  where
    loop [] ys | p False True = ys
               | otherwise    = []
    loop xs [] | p True False = xs
               | otherwise    = []
    loop (x:xs) (y:ys)
      = case cmp x y of
          LT | p True False -> x : loop xs (y:ys)
             | otherwise    ->     loop xs (y:ys)
          EQ | p True True  -> x : loop xs ys
             | otherwise    ->     loop xs ys
          GT | p False True -> y : loop (x:xs) ys
             | otherwise    ->     loop (x:xs) ys

-- Here's another variation that was suggested to me.  It is more general
-- than genSectBy, as it can implement a merge; but it cannot implement
-- a left-biased merge

foldrMergeBy :: (a -> b -> Ordering)
             -> (a -> c -> c) -> (b -> c -> c) -> (a -> b -> c -> c) -> c
             -> [a] -> [b] -> c
foldrMergeBy cmp addA addB unify z = loop
  where
    loop xs [] = foldr addA z xs
    loop [] ys = foldr addB z ys
    loop (x:xs) (y:ys)
      = case cmp x y of
          LT -> x `addA` loop  xs (y:ys)
          EQ -> unify x y (loop xs ys)
          GT -> y `addB` loop (x:xs) ys
-}

-- |  The 'isect' function computes the intersection of two ordered lists.
-- An element occurs in the output as many times as the minimum number of
-- occurrences in either input.  If either input is a set,  then the output
-- is a set.
--
-- > isect [ 1,2, 3,4 ] [ 3,4, 5,6 ]   == [ 3,4 ]
-- > isect [ 1, 2,2,2 ] [ 1,1,1, 2,2 ] == [ 1, 2,2 ]
isect :: Ord a => [a] -> [a] -> [a]
isect = isectBy compare

-- |  The 'isectBy' function is the non-overloaded version of 'isect'.
isectBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
isectBy cmp = loop
  where
     loop [] _ys  = []
     loop _xs []  = []
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT ->     loop xs (y:ys)
          EQ -> x : loop xs ys
          GT ->     loop (x:xs) ys

-- |  The 'union' function computes the union of two ordered lists.
-- An element occurs in the output as many times as the maximum number
-- of occurrences in either input.  If both inputs are sets,  then the
-- output is a set.
--
-- > union [ 1,2, 3,4 ] [ 3,4, 5,6 ]   == [ 1,2, 3,4, 5,6 ]
-- > union [ 1, 2,2,2 ] [ 1,1,1, 2,2 ] == [ 1,1,1, 2,2,2 ]
union :: Ord a => [a] -> [a] -> [a]
union = unionBy compare

-- |  The 'unionBy' function is the non-overloaded version of 'union'.
unionBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
unionBy cmp = loop
  where
     loop [] ys = ys
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT -> x : loop xs (y:ys)
          EQ -> x : loop xs ys
          GT -> y : loop (x:xs) ys

-- |  The 'minus' function computes the difference of two ordered lists.
-- An element occurs in the output as many times as it occurs in
-- the first input, minus the number of occurrences in the second input.
-- If the first input is a set,  then the output is a set.
--
-- > minus [ 1,2, 3,4 ] [ 3,4, 5,6 ]   == [ 1,2 ]
-- > minus [ 1, 2,2,2 ] [ 1,1,1, 2,2 ] == [ 2 ]
minus :: Ord a => [a] -> [a] -> [a]
minus = minusBy compare

-- |  The 'minusBy' function is the non-overloaded version of 'minus'.
minusBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
minusBy cmp = loop
  where
     loop [] _ys = []
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT -> x : loop xs (y:ys)
          EQ ->     loop xs ys
          GT ->     loop (x:xs) ys

-- |  The 'xunion' function computes the exclusive union of two ordered lists.
-- An element occurs in the output as many times as the absolute difference
-- between the number of occurrences in the inputs.  If both inputs
-- are sets,  then the output is a set.
--
-- > xunion [ 1,2, 3,4 ] [ 3,4, 5,6 ]   == [ 1,2, 5,6 ]
-- > xunion [ 1, 2,2,2 ] [ 1,1,1, 2,2 ] == [ 1,1, 2 ]
xunion :: Ord a => [a] -> [a] -> [a]
xunion = xunionBy compare

-- |  The 'xunionBy' function is the non-overloaded version of 'xunion'.
xunionBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
xunionBy cmp = loop
  where
     loop [] ys = ys
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT -> x : loop xs (y:ys)
          EQ ->     loop xs ys
          GT -> y : loop (x:xs) ys

-- |  The 'merge' function combines all elements of two ordered lists.
-- An element occurs in the output as many times as the sum of the
-- occurrences in the lists.
--
-- > merge [ 1,2, 3,4 ] [ 3,4, 5,6 ]   == [ 1,2,  3,3,4,4,  5,6 ]
-- > merge [ 1, 2,2,2 ] [ 1,1,1, 2,2 ] == [ 1,1,1,1,  2,2,2,2,2 ]
merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

-- |  The 'mergeBy' function is the non-overloaded version of 'merge'.
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp = loop
  where
    loop [] ys  = ys
    loop xs []  = xs
    loop (x:xs) (y:ys)
      = case cmp x y of
         GT -> y : loop (x:xs) ys
         _  -> x : loop xs (y:ys)

-- |  The 'subset' function returns true if the first list is a sub-list
-- of the second.
subset :: Ord a => [a] -> [a] -> Bool
subset = subsetBy compare

-- |  The 'subsetBy' function is the non-overloaded version of 'subset'.
subsetBy :: (a -> a -> Ordering) -> [a] -> [a] -> Bool
subsetBy cmp = loop
  where
    loop [] _ys = True
    loop _xs [] = False
    loop (x:xs) (y:ys)
      = case cmp x y of
         LT -> False
         EQ -> loop xs ys
         GT -> loop (x:xs) ys

{-
-- This is Ian Lynagh's mergesort implementation,  which appeared as
-- Data.List.sort, with the static argument transformation applied.
-- It's not clear whether this modification is truly worthwhile or not.

sort :: Ord a => [a] -> [a]
sort = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = foldTree (mergeBy cmp) [] . map (\x -> [x])
-}

-- |  The 'sortOn' function provides the decorate-sort-undecorate idiom,
-- also known as the \"Schwartzian transform\".
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f  = map snd . sortOn' fst .  map (\x -> (f x, x))

-- |  This variant of 'sortOn' recomputes the sorting key every comparison.
-- This can be better for functions that are cheap to compute.
-- This is definitely better for projections,  as the decorate-sort-undecorate
-- saves nothing and adds two traversals of the list and extra memory
-- allocation.
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f = sortBy (\x y -> compare (f x) (f y))

-- |  The 'nubSort' function is equivalent to @'nub' '.' 'sort'@,  except
-- somewhat more efficient as duplicates are removed as it sorts.  It is
-- essentially Data.List.sort,  with 'merge' replaced by 'union'.
nubSort :: Ord a => [a] -> [a]
nubSort = nubSortBy compare

-- |  The 'nubSortBy' function is the non-overloaded version of 'nubSort'.
nubSortBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortBy cmp = foldTree' (unionBy cmp) [] . runs
  where
    -- 'runs' partitions the input into sublists that are monotonic,
    -- contiguous,  and non-overlapping.   Descending runs are reversed
    -- and adjacent duplicates are eliminated,  so every run returned is
    -- strictly ascending.

    runs (a:b:xs)
      = case cmp a b of
          LT -> asc b (a:) xs
          EQ -> runs (a:xs)
          GT -> desc b [a] xs
    runs xs = [xs]

    desc a as []  = [a:as]
    desc a as (b:bs)
      = case cmp a b of
          LT -> (a:as) : runs (b:bs)
          EQ -> desc a as bs
          GT -> desc b (a:as) bs

    asc a as [] = [as [a]]
    asc a as (b:bs)
      = case cmp a b of
         LT -> asc b (\ys -> as (a:ys)) bs
         EQ -> asc a as bs
         GT -> as [a] : runs (b:bs)

-- |  The 'nubSortOn' function provides decorate-sort-undecorate for 'nubSort'.
nubSortOn :: Ord b => (a -> b) -> [a] -> [a]
nubSortOn f = map snd . nubSortOn' fst . map (\x -> (f x, x))

-- |  This variant of 'nubSortOn' recomputes the sorting key for each comparison.
nubSortOn' :: Ord b => (a -> b) -> [a] -> [a]
nubSortOn' f = nubSortBy (\x y -> compare (f x) (f y))

-- | On ordered lists,  'nub' is equivalent to 'Data.List.nub', except that
-- it runs in linear time instead of quadratic.   On unordered lists it also
-- removes elements that are smaller than any preceding element.
--
-- > nub [1,1,1,2,2] == [1,2]
-- > nub [2,0,1,3,3] == [2,3]
-- > nub = nubBy (<)

nub :: Ord a => [a] -> [a]
nub = nubBy (<)

-- | The 'nubBy' function is the greedy algorithm that returns a
-- sublist of its input such that:
--
-- > isSortedBy pred (nubBy pred xs) == True
--
-- This is true for all lists,  not just ordered lists,  and all binary
-- predicates,  not just total orders.   On infinite lists,  this statement
-- is true in a certain mathematical sense,  but not a computational one.
nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy p []     = []
nubBy p (x:xs) = x : loop x xs
  where
    loop _ [] = []
    loop x (y:ys)
       | p x y     = y : loop y ys
       | otherwise = loop x ys

-- | The function @'foldTree'' plus zero@ computes the sum of a list
-- using a balanced tree of operations.  'foldTree'' necessarily diverges
-- on infinite lists, hence it is a stricter variant of 'foldTree'.
-- 'foldTree'' is used in the implementation of 'sort' and 'nubSort'.

foldTree' :: (a -> a -> a) -> a -> [a] -> a
foldTree' plus zero xs
  = case xs of
      []    -> zero
      (_:_) -> loop xs
  where
    loop [x] = x
    loop xs  = loop (pairs xs)

    pairs (x:y:zs) = plus x y : pairs zs
    pairs zs       = zs

-- | The function @'foldTree' plus zero@ computes the sum of a list using
-- a sequence of balanced trees of operations.   Given an appropriate @plus@
-- operator,  this function can be productive on an infinite list, hence it
-- is lazier than 'foldTree''.   'foldTree' is used in the implementation of
-- 'mergeAll' and 'unionAll'.

foldTree :: (a -> a -> a) -> a -> [a] -> a
foldTree plus zero xs
  = case xs of
      []    -> zero
      (_:_) -> loop xs
  where
    loop [x]    = x
    loop (x:xs) = x `plus` loop (pairs xs)

    pairs (x:y:zs) = plus x y : pairs zs
    pairs zs       = zs

-- | The 'mergeAll' function merges a (potentially) infinite number of
-- ordered lists, under the assumption that the heads of the inner lists
-- are sorted.  An element is duplicated in the result as many times as
-- the total number of occurrences in all inner lists.
--
-- The 'mergeAll' function is closely related to @'foldr' 'merge' []@.
-- The former does not assume that the outer list is finite, whereas
-- the latter makes no assumption about the heads of the inner lists.
-- When both sets of assumptions are met,  these two functions are
-- equivalent.
--
-- This implementation of 'mergeAll'  uses a tree of comparisons, and is
-- based on input from Dave Bayer, Heinrich Apfelmus, Omar Antolin Camarena,
-- and Will Ness.
mergeAll :: Ord a => [[a]] -> [a]
mergeAll = mergeAllBy compare

-- | The 'mergeAllBy' function is the non-overloaded variant of the 'mergeAll' function.
mergeAllBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeAllBy cmp = foldTree merge' []
  where
    merge' []     ys = ys
    merge' (x:xs) ys = x : mergeBy cmp xs ys

-- | The 'unionAll' computes the union of a (potentially) infinite number
-- of lists,  under the assumption that the heads of the inner lists
-- are sorted.  The result will duplicate an element as many times as
-- the maximum number of occurrences in any single list.  Thus, the result
-- is a set if and only if every inner list is a set.
--
-- Analogous to 'mergeAll',  'unionAll' is closely related to
-- @'foldr' 'union' []@;  The outer does not assume that the outer list
-- is finite,  whereas the right fold does not assume anything about the
-- heads of the inner lists. When both sets of assumptions are met,  the
-- functions are equivalent.
--
-- This implementation is also based on implicit heaps,  providing
-- a tree of comparisons.
unionAll :: Ord a => [[a]] -> [a]
unionAll = unionAllBy compare

-- | The 'unionAllBy' function is the non-overloaded variant of the 'unionAll' function.
unionAllBy :: (a -> a -> Ordering) -> [[a]] -> [a]
unionAllBy cmp = foldTree union' []
  where
    msg = "Data.List.Ordered.unionAllBy:  the heads of the lists are not sorted"

    union' []     ys = ys
    union' (x:xs) ys = x : case ys of
                             []     -> xs
                             (y:yt) -> case cmp x y of
                                         LT -> unionBy cmp xs ys
                                         EQ -> unionBy cmp xs yt
                                         GT -> error msg

