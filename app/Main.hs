module Main where

import Debug.Trace
import Data.Function (on)
import Data.List (sortBy, sortOn, groupBy)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..), comparing)
import Safe (atMay)
import UnorderedPair
import qualified Data.Map as Map (Map, empty, insertWith, toList)

_SONNET_PATH = "sonnets.txt"

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose k [] = []
choose k (x:xs) = map (x :) (choose (k - 1) xs) ++ choose k xs

removeEvery :: Int -> [a] -> [a]
removeEvery n = foldr step [] . zip [1 ..]
  where
    step (i, x) acc =
        if (i `mod` n) == 0
            then acc
            else x : acc

schemeToIndices :: String -> [[Int]]
schemeToIndices = map (map fst) . groupBy ((==) `on` snd) . sortOn snd . zip [0..]

rhymes :: String -> [String] -> [UPair String]
rhymes scheme ls = concatMap getPair rhymingPairs
  where
    rhymingPairs = schemeToIndices scheme
    getPair :: [Int] -> [UPair String]
    getPair =
        map (\[x, y] -> x# y) .
        choose 2 . mapMaybe (\i -> fmap rhyme (ls `atMay` i))
    rhyme = takeWhile (not . flip elem ".,:;!?") . last . words

stats :: Ord a => [a] -> Map.Map a Int
stats = foldr (\p -> Map.insertWith (+) p 1) Map.empty

main :: IO ()
main = do
    sonns <- getSonnets _SONNET_PATH
    let rhymingStats = stats $ concatMap (rhymes "ababcdcdefefgg") sonns
    putStrLn $ unlines $ formatMap rhymingStats
  where
    getSonnets path = do
        paragraphs <- splitOn "\n\n" <$> readFile _SONNET_PATH
        return $ map lines $ removeEvery 2 paragraphs
    formatMap :: (Ord k, Ord v, Show k, Show v) => Map.Map k v -> [String]
    formatMap = map format . sortBy (comparing (Down . snd)) . Map.toList
      where
        format (x, y) = show y ++ "\t" ++ show x
