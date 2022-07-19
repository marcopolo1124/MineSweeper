module ListZipper where

import Data.List
type ListZipper a = ([a], [a])

fromList :: [a] -> ListZipper a
fromList lst = ([], lst)

toList :: ListZipper a -> [a]
toList (left, right) = reverse left ++ right

getFocus :: ListZipper a -> a
getFocus (left, right) = head right

getLeft :: ListZipper a -> [a]
getLeft (x,y) = x

getRight :: ListZipper a -> [a]
getRight (x,y) = y

lengthZ :: ListZipper a -> Int
lengthZ (left, right) = length left + length right

applyToFocus :: (a -> a) -> ListZipper a -> ListZipper a
applyToFocus f (left, x:right) = (left, f x:right)
applyToFocus _ z = z

moveLeft :: ListZipper a -> Maybe (ListZipper a)
moveLeft (x:xs, right) = Just (xs, x:right)
moveLeft ([], _) = Nothing

moveRight :: ListZipper a -> Maybe(ListZipper a)
moveRight (_, [x]) = Nothing
moveRight (left, x:right) = Just(x:left, right)
moveRight (_, []) = Nothing

getIndex :: ListZipper a -> Int
getIndex (left, right) = length left

moveNRight :: Int -> ListZipper a -> Maybe(ListZipper a)
moveNRight 0 z = Just z
moveNRight n z = do
    newZ <- moveRight z
    moveNRight (n-1) newZ

moveNLeft :: Int -> ListZipper a -> Maybe(ListZipper a)
moveNLeft 0 z = Just z
moveNLeft n z = moveLeft z >>= moveNLeft (n-1)

toIndex :: Int -> ListZipper a -> Maybe(ListZipper a)
toIndex index z =
    let
        currentIndex = getIndex z
        indDifference = index - currentIndex
    in 
        if indDifference > 0
            then moveNRight indDifference z
            else moveNLeft (abs indDifference) z

replace :: a -> ListZipper a -> ListZipper a
replace x (left, _:right) = (left, x:right)
replace x (left, []) = (left, [])

replaceAt :: Int -> a -> ListZipper a -> Maybe (ListZipper a)
replaceAt n x z = replace x <$> toIndex n z

type NestedZipper a = ListZipper (ListZipper a)

nestedReplace :: a -> NestedZipper a -> NestedZipper a
nestedReplace x (left', (left, o:right):right') = (left', (left, x:right):right')
nestedReplace x nz = nz

nestedReplaceAt :: Int -> Int -> a -> NestedZipper a -> Maybe (NestedZipper a)
nestedReplaceAt x y o z = do
    row <- getFocus <$> toIndex y z
    newRow <- replaceAt x o row
    replaceAt y newRow z

nestedToIndex :: NestedZipper a -> Int -> Int -> Maybe (NestedZipper a)
nestedToIndex z x y = do
    toRow <- toIndex y z
    newRow <- toIndex x (getFocus toRow)
    return $ replace newRow toRow
    
nestedApply :: NestedZipper a -> (a -> a) -> NestedZipper a
nestedApply (left', (left, x:right):right') f = (left', (left, f x:right):right')
nestedApply z _ = z

nestedFocus :: NestedZipper a -> a
nestedFocus = getFocus . getFocus


nestedApplyAt :: NestedZipper a -> Int -> Int -> (a->a) -> Maybe (NestedZipper a)
nestedApplyAt z x y f = do
    newFocus <- nestedToIndex z x y
    return $ nestedApply newFocus f

rowList = fromList [1..10]
nestedList :: ListZipper (ListZipper Int)
nestedList = fromList $ replicate 5 rowList

(!!!) :: ListZipper a -> Int -> Maybe a
z !!! x = do
    moveTo <- toIndex x z
    return $ getFocus moveTo

infixr 4 !!!

(!?!) :: NestedZipper a -> (Int, Int) -> Maybe a
z !?! (x,y) = do
    moveToR <- toIndex y z
    let row = getFocus moveToR
    moveToC <- toIndex x row
    return $ getFocus moveToC

infixr 4 !?!


access = nestedList !?! (1,1)
replacedList = nestedReplaceAt 0 2 9 nestedList