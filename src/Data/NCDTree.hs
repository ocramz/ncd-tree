{-# LANGUAGE RecordWildCards #-}
module Data.NCDTree
  ( Document(..)
  , VPTree(..)
  , ResultHeap
  , knnSearch
  , mkVPTree
  , ncd
  , compressedSize
  ) where

import Control.Monad.ST
import Data.String (IsString(..))
import Data.Ord (Down(..), comparing)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Data.Heap as H

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Algorithms.Merge as MV (sortBy)

import Codec.Compression.GZip (compress)

newtype Document = Document
    { docText :: BL.ByteString
    } deriving (Eq, Show)

instance IsString Document where
  fromString = Document . BLC.pack

data VPTree = VPNode { pivot    :: Document
                     , threshold :: Double
                     , inside   :: VPTree
                     , outside  :: VPTree }
            | VPLeaf { items :: [Document] }
            | VPEmpty
            deriving (Eq, Show)


-- max-heap
type ResultHeap = H.Heap (H.Entry (Down Double) Document)

deleteMax :: H.Heap a -> H.Heap a
deleteMax = H.deleteMin
viewMax :: H.Heap a -> Maybe (a, H.Heap a)
viewMax = H.viewMin

knnSearch :: Int -> Document -> VPTree -> ResultHeap
knnSearch k query root = go root H.empty
  where
    go VPEmpty h = h
    go (VPLeaf items) h =
        foldr (\item acc ->
            let d = ncd (docText query) (docText item)
            in insertNeighbor k (Down d) item acc
        ) h items
    go VPNode{..} h =
        let d = ncd (docText query) (docText pivot)
            -- 1. Tentatively add the pivot
            hWithPivot = insertNeighbor k (Down d) pivot h
            -- tau = getTau k hWithPivot

            -- 2. Determine search order (search the more promising side first)
            (near, far) = if d < threshold then (inside, outside) else (outside, inside)

            -- 3. Search the 'near' branch
            h1 = go near hWithPivot

            -- 4. Pruning: Search 'far' branch only if it could contain a better neighbor
            tau' = getTau k h1
            h2 = if Down (abs (d - threshold)) <= tau' || length h1 < k
                 then go far h1
                 else h1
        in h2

getTau :: Fractional a1 =>
          Int -> H.Heap (H.Entry (Down a1) a2) -> Down a1
getTau k heap
    | length heap < k = Down 1.0
    | otherwise = case viewMax heap of
        Just (H.Entry d _, _) -> d
        Nothing               -> Down 1.0

-- | Insert a neighbor and maintain the heap size at exactly k.

insertNeighbor :: (Ord a1, Fractional a1) =>
                  Int
               -> Down a1
               -> a2
               -> H.Heap (H.Entry (Down a1) a2)
               -> H.Heap (H.Entry (Down a1) a2)
insertNeighbor k dist doc heap
    | length heap < k = H.insert (H.Entry dist doc) heap
    | dist < getTau k heap = H.insert (H.Entry dist doc) (deleteMax heap)
    | otherwise = heap



-- | Calculate distances from pivot to all other elements
getD :: Document -> Document -> Double
getD other p = ncd (docText p) (docText other)


mkVPTree :: Int -> [Document] -> VPTree
mkVPTree leafThreshold docs = runST $ do
    mutVec <- V.thaw (V.fromList docs)
    build mutVec 0 (MV.length mutVec)
      where
        build vec start len = do
          if len == 0 then return VPEmpty
            else if len < leafThreshold then do
            finalVec <- V.freeze (MV.slice start len vec)
            return $ VPLeaf (V.toList finalVec)
            else
            do
              -- 1. Pick first element as pivot (vantage point)
              vp <- MV.read vec start
              -- 2. Sort the remaining elements by distance to vp
              let subStart = start + 1
                  subLen = len - 1
              sortSliceBy vec subStart subLen (comparing (getD vp))
              -- 3. Split at midpoint
              let mid = max 1 (subLen `div` 2)
                  insideStart = subStart
                  insideLen = mid
                  outsideStart = subStart + mid
                  outsideLen = subLen - mid
                  lastInsideIdx = insideStart + insideLen - 1
              -- 4. Find threshold as distance to last element in 'inside'
              lastInside <- MV.read vec lastInsideIdx
              let thresh = getD vp lastInside
              -- 5. Recurse on slices
              leftTree <- build vec insideStart insideLen
              rightTree <- build vec outsideStart outsideLen
              return $ VPNode vp thresh leftTree rightTree

-- | Sort a mutable vector slice in-place by a comparison function on elements
sortSliceBy :: MV.MVector s Document 
            -> Int 
            -> Int 
            -> (Document -> Document -> Ordering) 
            -> ST s ()
sortSliceBy vec start len cmp = do
  let sliced = MV.slice start len vec
  MV.sortBy cmp sliced




-- | Normalized Compression Distance
ncd :: BL.ByteString -> BL.ByteString -> Double
ncd s1 s2 =
    let
      c1 = compressedSize s1
      c2 = compressedSize s2
      c12 = compressedSize (BL.append s1 s2)
      maxC = max c1 c2
    in if maxC == 0 then 0 else (c12 - min c1 c2) / maxC

-- Compressed size in bytes
compressedSize :: BL.ByteString -> Double
compressedSize = fromIntegral . BL.length . compress
