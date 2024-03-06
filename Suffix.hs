module Suffix
    ( removeSuffix
    , removePrefix
    ) where

-- | Remove a matching suffix from a list
removeSuffix :: Eq a => [a] -> [a] -> Maybe [a]
removeSuffix xs ys = reverse <$> removePrefix (reverse xs) (reverse ys)

-- | Remove a matching prefix from a list
removePrefix :: Eq a => [a] -> [a] -> Maybe [a]
removePrefix [] ys = Just ys
removePrefix (x:xs) (y:ys)
    | x == y = removePrefix xs ys
removePrefix _ _ = Nothing
