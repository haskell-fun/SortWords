module Lib where

someFunc :: IO ()
someFunc = (fmap sortAlphabetically source) >>= printList

source :: IO [String]
source = pure ["one", "two", "three"]

data CompareResult = Preceding | Equal | Following
  deriving (Eq)

newtype Comparator a = Comparator (a -> a -> CompareResult)

minim :: Comparator a -> a -> a -> a
minim (Comparator f) a b
    | f a b == Preceding = a
    | otherwise = b
      
minimumInList :: Comparator a -> [a] -> (a, Int)
minimumInList (Comparator f) list = let comp = Comparator (\x y -> f (fst x) (fst y)) in 
                                    foldl1 (minim comp) (zip list [0..]) 

sortList :: Comparator a -> [a] -> [a]
sortList _ [] = []
sortList comp list = let m = minimumInList comp list in 
                     (fst m : sortList comp (let (l1,l2) = splitAt (snd m) list in 
                                             l1 ++ tail l2)
                     )

compareStrings :: String -> String -> CompareResult
compareStrings s1 s2  
  | s1 < s2 = Preceding
  | s1 == s2 = Equal
  | otherwise = Following

sortAlphabetically :: [String] -> [String]
sortAlphabetically = sortList (Comparator compareStrings)

printList :: [String] -> IO ()
printList list = fmap (const ()) (traverse putStrLn list)