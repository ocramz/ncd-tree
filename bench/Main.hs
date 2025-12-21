{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Heap as H
import Data.Ord (Down(..))
import Test.BenchPress
import Control.DeepSeq

import Data.NCDTree

instance NFData Document where
  rnf (Document bs) = rnf bs

instance NFData VPTree where
  rnf VPEmpty = ()
  rnf (VPLeaf items) = rnf items
  rnf (VPNode p t i o) = rnf p `seq` rnf t `seq` rnf i `seq` rnf o

-- NFData instance for the result heap - just evaluate its length
instance NFData a => NFData (H.Heap (H.Entry (Down Double) a)) where
  rnf h = rnf (length h)

-- | Generate a list of documents of given size
generateDocs :: Int -> Int -> [Document]
generateDocs count size = 
  [ Document (BL.replicate (fromIntegral size) byte)
  | (count', byte) <- zip [1..count] (cycle [0..255])
  ]

main :: IO ()
main = do
  putStrLn "NCD Benchmarks"
  putStrLn "=============="
  (cpuStats, wallStats) <- benchmark 100 (return ()) (\_ -> return ()) $ \_ -> 
    ncd (BL.pack [1..100]) (BL.pack [50..150]) `deepseq` return ()
  putStrLn "ncd - small documents (100 bytes each)"
  putStr "CPU: "
  printDetailedStats cpuStats
  putStr "Wall: "
  printDetailedStats wallStats
  
  (cpuStats, wallStats) <- benchmark 100 (return ()) (\_ -> return ()) $ \_ ->
    ncd (BL.replicate 1000 65) (BL.replicate 1000 66) `deepseq` return ()
  putStrLn "ncd - medium documents (1000 bytes each)"
  putStr "CPU: "
  printDetailedStats cpuStats
  putStr "Wall: "
  printDetailedStats wallStats
  
  (cpuStats, wallStats) <- benchmark 100 (return ()) (\_ -> return ()) $ \_ ->
    ncd (BL.replicate 10000 65) (BL.replicate 10000 66) `deepseq` return ()
  putStrLn "ncd - large documents (10000 bytes each)"
  putStr "CPU: "
  printDetailedStats cpuStats
  putStr "Wall: "
  printDetailedStats wallStats
  
  putStrLn "\nmkVPTree Benchmarks"
  putStrLn "==================="
  (cpuStats, wallStats) <- benchmark 10 (return ()) (\_ -> return ()) $ \_ ->
    mkVPTree 4 (generateDocs 10 100) `deepseq` return ()
  putStrLn "mkVPTree - 10 small docs (100 bytes each)"
  putStr "CPU: "
  printDetailedStats cpuStats
  putStr "Wall: "
  printDetailedStats wallStats
  
  (cpuStats, wallStats) <- benchmark 10 (return ()) (\_ -> return ()) $ \_ ->
    mkVPTree 4 (generateDocs 50 100) `deepseq` return ()
  putStrLn "mkVPTree - 50 small docs (100 bytes each)"
  putStr "CPU: "
  printDetailedStats cpuStats
  putStr "Wall: "
  printDetailedStats wallStats
  
  (cpuStats, wallStats) <- benchmark 5 (return ()) (\_ -> return ()) $ \_ ->
    mkVPTree 4 (generateDocs 100 100) `deepseq` return ()
  putStrLn "mkVPTree - 100 small docs (100 bytes each)"
  putStr "CPU: "
  printDetailedStats cpuStats
  putStr "Wall: "
  printDetailedStats wallStats
  
  putStrLn "\nknnSearch Benchmarks"
  putStrLn "===================="
  let tree50 = mkVPTree 4 (generateDocs 50 500)
      query50 = head (generateDocs 1 500)
  (cpuStats, wallStats) <- benchmark 50 (return tree50) (\_ -> return ()) $ \tree ->
    knnSearch 1 query50 tree `deepseq` return ()
  putStrLn "knnSearch - k=1 in 50 docs (500 bytes each)"
  putStr "CPU: "
  printDetailedStats cpuStats
  putStr "Wall: "
  printDetailedStats wallStats
  
  (cpuStats, wallStats) <- benchmark 50 (return tree50) (\_ -> return ()) $ \tree ->
    knnSearch 5 query50 tree `deepseq` return ()
  putStrLn "knnSearch - k=5 in 50 docs (500 bytes each)"
  putStr "CPU: "
  printDetailedStats cpuStats
  putStr "Wall: "
  printDetailedStats wallStats
  
  let tree100 = mkVPTree 4 (generateDocs 100 500)
      query100 = head (generateDocs 1 500)
  (cpuStats, wallStats) <- benchmark 50 (return tree100) (\_ -> return ()) $ \tree ->
    knnSearch 10 query100 tree `deepseq` return ()
  putStrLn "knnSearch - k=10 in 100 docs (500 bytes each)"
  putStr "CPU: "
  printDetailedStats cpuStats
  putStr "Wall: "
  printDetailedStats wallStats
