import Data.List

sortFun a b
    | length a > length b = GT
    | otherwise           = LT

sortSubLists = sortBy sortFun
