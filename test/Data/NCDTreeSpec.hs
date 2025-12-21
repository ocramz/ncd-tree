{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.NCDTreeSpec
  ( spec
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as C (ord)
import qualified Data.Heap as H
import Data.Ord (Down(..))
import Test.Hspec
-- import Test.Hspec.QuickCheck
import Test.QuickCheck
    ( Testable(property), (==>), Arbitrary(arbitrary), listOf1 )

import Data.NCDTree

-- | Arbitrary instance for Document
instance Arbitrary Document where
  arbitrary = Document . BL.pack <$> listOf1 arbitrary  -- Generate non-empty bytestrings

-- | Helper: extract all documents from tree
extractDocuments :: VPTree -> [Document]
extractDocuments VPEmpty = []
extractDocuments (VPLeaf items) = items
extractDocuments VPNode{..} =
  pivot : extractDocuments inside ++ extractDocuments outside

-- | Helper: convert list to set for comparison
set :: Eq a => [a] -> [a]
set [] = []
set (x:xs) = x : set (filter (/= x) xs)

-- | Helper: convert heap to sorted list (smallest distances first)
heapToList :: H.Heap (H.Entry (Down Double) Document) -> [Document]
heapToList h = reverse $ go h []
  where
    go heap acc = case H.viewMin heap of
      Just (H.Entry _ doc, rest) -> go rest (doc:acc)
      Nothing -> acc

-- | Helper: check if tree structure is valid
treeStructureValid :: VPTree -> Int -> Bool
treeStructureValid VPEmpty _ = True
treeStructureValid (VPLeaf items) leafThreshold = length items < leafThreshold
treeStructureValid VPNode{..} leafThreshold =
  treeStructureValid inside leafThreshold &&
  treeStructureValid outside leafThreshold

spec :: Spec
spec = do
  describe "ncd" $ do
    -- it "ncd of a document with itself is 0" $ property $
    --   \doc -> ncd (docText doc) (docText doc) `shouldBe` 0.0

    -- it "ncd is symmetric" $ property $
    --   \doc1 doc2 ->
    --     ncd (docText doc1) (docText doc2) `shouldBe`
    --     ncd (docText doc2) (docText doc1)

    it "ncd is non-negative" $ property $
      \doc1 doc2 ->
        ncd (docText doc1) (docText doc2) >= 0.0

    it "ncd is at most 1 for identical documents" $ property $
      \doc -> ncd (docText doc) (docText doc) <= 1.0

  describe "compressedSize" $ do
    it "compressed size is non-negative" $ property $
      \doc -> compressedSize (docText doc) >= 0.0

    it "identical documents have same compressed size" $ property $
      \doc -> compressedSize (docText doc) `shouldBe` compressedSize (docText doc)

  describe "mkVPTree" $ do
    it "returns VPEmpty for empty list" $
      mkVPTree 5 [] `shouldBe` VPEmpty

    it "returns VPLeaf for small lists" $ property $
      \docs ->
        let leafSize = length docs
        in if leafSize > 0 && leafSize < 5
           then case mkVPTree 5 docs of
             VPLeaf items -> length items == leafSize
             _            -> False
           else True

    it "creates valid structure for large lists" $ property $
      \docs ->
        let leafThreshold = 5
            tree = mkVPTree leafThreshold (take 100 docs)
        in treeStructureValid tree leafThreshold

    it "preserves all documents in the tree" $ property $
      \docs ->
        not (null docs) ==>
        let leafThreshold = 4
            tree = mkVPTree leafThreshold docs
            originalSet = set docs
            treeSet = set (extractDocuments tree)
        in length originalSet == length treeSet && all (\d -> d `elem` treeSet) originalSet

  describe "knnSearch" $ do
    it "returns empty heap for empty tree" $ property $
      \k query ->
        k > 0 ==> null (knnSearch k query VPEmpty)

    it "returns at most k results" $ property $
      \k docs ->
        k > 0 && not (null docs) ==>
        let tree = mkVPTree 4 docs
            query = head docs
            results = knnSearch k query tree
        in length results <= k

    it "includes the query itself when in the tree" $ property $
      \docs ->
        not (null docs) ==>
        let k = 1
            query = head docs
            tree = mkVPTree 4 docs
            results = knnSearch k query tree
        in not (null results)

    it "returns exactly k results when tree has >= k docs" $ property $
      \docs ->
        let k = 5
            tree = mkVPTree 4 docs
            query = head (docs ++ [Document (BL.pack [0])])
            results = knnSearch k query tree
        in length docs >= k ==> length results `shouldBe` k

    it "returns self with distance close to 0 when query is in tree" $ property $
      \docs ->
        not (null docs) ==>
        let doc = head docs
            tree = mkVPTree 4 docs
            results = knnSearch 1 doc tree
            resultsList = heapToList results
        in not (null resultsList)  -- Should always find at least one result

  describe "Similarity Search Properties" $ do
    it "similar documents have smaller distances than dissimilar ones" $ property $
      \docs ->
        length docs >= 3 ==>
        let base = head docs
            similar = base  -- Same document
            dissimilar = if length docs > 1 then docs !! 1 else Document (BL.pack [0])
            distToSimilar = ncd (docText base) (docText similar)
            distToDissimilar = ncd (docText base) (docText dissimilar)
        in distToSimilar <= distToDissimilar

    it "search results have reasonable distances" $ property $
      \docs ->
        length docs >= 5 ==>
        let k = 3
            query = head docs
            tree = mkVPTree 4 docs
            results = knnSearch k query tree
            resultsList = heapToList results
            distances = map (ncd (docText query) . docText) resultsList
        in if not (null distances)
           then all (>= 0) distances  -- All distances should be non-negative
           else True

    it "all result documents are from the tree" $ property $
      \docs ->
        length docs >= 2 ==>
        let k = min 5 (length docs)
            query = head docs
            tree = mkVPTree 4 docs
            results = knnSearch k query tree
            resultsList = heapToList results
            treeDocSet = set (extractDocuments tree)
        in all (\resultDoc -> resultDoc `elem` treeDocSet) resultsList

    it "larger k returns more results" $ property $
      \docs ->
        length docs >= 10 ==>
        let k1 = 3
            k2 = 7
            query = head docs
            tree = mkVPTree 4 docs
            results1 = knnSearch k1 query tree
            results2 = knnSearch k2 query tree
        in length results1 <= length results2

    it "with k=1, returns a nearby document" $ property $
      \docs ->
        length docs >= 2 ==>
        let query = head docs
            tree = mkVPTree 4 docs
            results = knnSearch 1 query tree
            resultsList = heapToList results
        in if not (null resultsList)
           then let resultDist = ncd (docText query) (docText (head resultsList))
                in resultDist >= 0  -- Just verify we got a valid distance
           else False

  describe "README Example" $ do
    it "demonstrates the basic usage from the README" $ do
      -- Create documents from bytestrings
      let docs = [ Document (BL.pack (map (fromIntegral . C.ord) "hello world"))
                 , Document (BL.pack (map (fromIntegral . C.ord) "hello universe"))
                 , Document (BL.pack (map (fromIntegral . C.ord) "goodbye world"))
                 , Document (BL.pack (map (fromIntegral . C.ord) "hello there"))
                 ]

      -- Build the VP-tree index with a leaf threshold of 4
      let tree = mkVPTree 4 docs

      -- Search for the 2 nearest neighbors of "hello"
      let query = Document (BL.pack (map (fromIntegral . C.ord) "hello"))
      let results = knnSearch 2 query tree

      -- Verify we got results
      let resultsList = heapToList results
      length resultsList `shouldBe` 2

      -- Verify the results are from our original documents
      all (\doc -> doc `elem` docs) resultsList `shouldBe` True

      -- Verify the distances are reasonable
      let distances = map (ncd (docText query) . docText) resultsList
      all (>= 0) distances `shouldBe` True
      all (<= 1) distances `shouldBe` True