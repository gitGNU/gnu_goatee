module Khumba.GoHS.Common ( listReplace
                          , listUpdate
                          , fromLeft
                          , fromRight
                          , onLeft
                          , onRight
                          , andEithers
                          ) where

listReplace :: Eq a => a -> a -> [a] -> [a]
listReplace from to = map $ replace from to
  where replace from to x = if x == from then to else x

listUpdate :: Show a => (a -> a) -> Int -> [a] -> [a]
listUpdate fn ix xs = listSet' ix xs
  where listSet' 0 (x':xs') = fn x':xs'
        listSet' ix' (x':xs') = x':listSet' (ix' - 1) xs'
        listSet' _ _ = error ("Cannot update index " ++ show ix ++
                              " of list " ++ show xs ++ ".")

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "fromLeft failed."

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight failed."

onLeft :: (a -> c) -> Either a b -> Either c b
f `onLeft` e = case e of
  Left x -> Left $ f x
  Right y -> Right y

onRight :: (b -> c) -> Either a b -> Either a c
f `onRight` e = case e of
  Left x -> Left x
  Right y -> Right $ f y

-- | If any item is a 'Left', then the list of 'Left's is returned, otherwise
-- the list of 'Right's is returned.
andEithers :: [Either a b] -> Either [a] [b]
andEithers xs = let (as, bs) = andEithers' xs
                in if not $ null as then Right bs else Left as

andEithers' :: [Either a b] -> ([a], [b])
andEithers' (Left a:rest) = let (as, bs) = andEithers' rest in (a:as, bs)
andEithers' (Right b:rest) = let (as, bs) = andEithers' rest in (as, b:bs)
andEithers' _ = ([], [])
