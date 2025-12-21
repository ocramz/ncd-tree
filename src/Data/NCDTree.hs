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
import Data.List (sortBy)
import Data.Ord (Down(..), comparing)
import Data.Word (Word64)

import qualified Data.ByteString.Lazy as BL

import qualified Data.Heap as H

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Algorithms.Merge as MV (sortBy)

import Codec.Compression.GZip (compress)

newtype Document = Document
    { docText :: BL.ByteString
    } deriving (Eq, Show)

data VPTree = VPNode { pivot    :: Document
                     , threshold :: Double
                     , inside   :: VPTree
                     , outside  :: VPTree }
            | VPLeaf { items :: [Document] }
            | VPEmpty
            deriving (Eq, Show)


-- max-heap
type ResultHeap = H.Heap (H.Entry (Down Double) Document)

deleteMax = H.deleteMin
viewMax = H.viewMin

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

-- sorted :: V.Vector Document -> V.Vector Document
sortedWrt :: Document -> V.Vector Document -> V.Vector Document
sortedWrt p =
  V.modify $ MV.sortBy (\other -> comparing (getD other) p)


mkVPTree :: Int -> [Document] -> VPTree
mkVPTree leafThreshold docs = runST $ do
    mutVec <- V.thaw (V.fromList docs)
    build mutVec
      where
        build vec = do
          let
            len = MV.length vec
          if len == 0 then return VPEmpty
            else if len < leafThreshold then do
            finalItems <- V.freeze vec
            return $ VPLeaf (V.toList finalItems)
            else
            do
              -- 1. Pick first element as pivot (vantage point)
              vp <- MV.read vec 0
              let
                sub = MV.slice 1 (len - 1) vec
              frozenSub <- V.freeze sub
              let
                sorted = sortedWrt vp frozenSub
                lenWithoutPivot = V.length sorted
                mid = max 1 (lenWithoutPivot `div` 2)
                (inV, outV) = V.splitAt mid sorted
                thresh = getD vp (V.last inV)
              -- 4. Recurse
              inVM <- V.thaw inV
              outVM <- V.thaw outV
              leftTree <- build inVM
              rightTree <- build outVM
              return $ VPNode vp thresh leftTree rightTree




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
