module UnorderedPair
  ( UPair()
  , ( # )
  , toList
  ) where

import Data.List (intercalate)
import qualified Data.Set as Set

newtype UPair a = MkUPair
    { unUPair :: Set.Set a
    } deriving (Eq, Ord)

(#) :: Ord a => a -> a -> UPair a
x # y = MkUPair $ Set.fromList [x, y]

instance Show a =>
         Show (UPair a) where
    show pair = "{" ++ intercalate " " (map show xs) ++ "}"
      where
        xs = toList pair

toList :: UPair a -> [a]
toList = Set.elems . unUPair
