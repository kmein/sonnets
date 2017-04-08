module Main where

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..), comparing)
import Safe (atMay)
import UnorderedPair
import qualified Data.Map as Map (Map, empty, insertWith, toList)

_SONNET_PATH = "sonnets.txt"

removeEvery :: Int -> [a] -> [a]
removeEvery n = foldr step [] . zip [1 ..]
  where
    step (i, x) acc =
        if (i `mod` n) == 0
            then acc
            else x : acc

rhymes :: [String] -> [UPair String]
rhymes ls = mapMaybe getPair rhymingPairs
  where
    rhymingPairs = [(0, 2), (1, 3), (4, 6), (5, 7), (8, 10), (9, 11), (12, 13)]
    getPair (a, b) =
        ( # ) <$> fmap rhyme (ls `atMay` a) <*> fmap rhyme (ls `atMay` b)
    rhyme = takeWhile (not . flip elem ".,:;!?") . last . words

stats :: [UPair String] -> Map.Map (UPair String) Int
stats = foldr (\p -> Map.insertWith (+) p 1) Map.empty

main :: IO ()
main = do
    sonns <- getSonnets _SONNET_PATH
    let rhymingStats = stats $ concatMap rhymes sonns
    putStrLn $ unlines $ formatMap rhymingStats
  where
    getSonnets path = do
        paragraphs <- splitOn "\n\n" <$> readFile _SONNET_PATH
        return $ map lines $ removeEvery 2 paragraphs
    formatMap :: (Ord k, Ord v, Show k, Show v) => Map.Map k v -> [String]
    formatMap = map format . sortBy (comparing (Down . snd)) . Map.toList
      where
        format (x, y) = show y ++ "\t" ++ show x
