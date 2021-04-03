module Lib where

someFunc :: IO ()
someFunc = (fmap sortAlphabetically source) >>= printList

source :: IO [String]
source = pure ["one", "two", "three"]

sortAlphabetically :: [String] -> [String]
sortAlphabetically a = a

printList :: [String] -> IO ()
printList list = fmap (const ()) (traverse putStrLn list)