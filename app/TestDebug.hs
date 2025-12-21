{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Heap as H
import Data.Ord (Down(..))
import Control.Monad (when)
import Data.NCDTree

extractDocuments :: VPTree -> [Document]
extractDocuments VPEmpty = []
extractDocuments (VPLeaf items) = items
extractDocuments VPNode{..} =
  pivot : extractDocuments inside ++ extractDocuments outside

heapToList :: H.Heap (H.Entry (Down Double) Document) -> [Document]
heapToList h = go h []
  where
    go heap acc = case H.viewMin heap of
      Just (H.Entry _ doc, rest) -> go rest (doc:acc)
      Nothing -> acc

main :: IO ()
main = do
  let doc = Document (BL.pack [0])
  let empty = BL.pack [0]
  let c1 = compressedSize empty
  let c12 = compressedSize (BL.append empty empty)
  putStrLn $ "compressedSize of single byte: " ++ show c1
  putStrLn $ "compressedSize of two same bytes: " ++ show c12
  putStrLn $ "ncd(x, x) = " ++ show (ncd empty empty)
  
  -- Try with larger data
  let doc2 = BL.pack [1..50]
  let c2_1 = compressedSize doc2
  let c2_2 = compressedSize (BL.append doc2 doc2)
  putStrLn $ "\ncompressedSize of 50 bytes: " ++ show c2_1
  putStrLn $ "compressedSize of 100 same bytes: " ++ show c2_2
  putStrLn $ "ncd(x, x) for 50 bytes = " ++ show (ncd doc2 doc2)
