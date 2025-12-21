# ncd-tree

## Overview

`ncd-tree` is a Haskell library for **similarity-based nearest neighbor search** using the **Normalized Compression Distance (NCD)** metric combined with **Vantage-Point (VP) trees**.

### What it does

The library enables you to:
- Define a notion of similarity between documents based on their compressibility with gzip
- Build an index structure (VP-tree) that accelerates k-nearest neighbor searches
- Query the index to find documents most similar to a given query document

### Why NCD?

The Normalized Compression Distance is a universal distance metric that works for any type of data (text, sequences, etc.) without requiring domain-specific features or models. It's based on the intuition that the most efficient way to describe the similarity between two objects is the length of the shortest program that computes one object given the other.

For two documents x and y:
```
NCD(x, y) = (C(xy) - min(C(x), C(y))) / max(C(x), C(y))
```
where C(s) is the compressed size of s using gzip.

### Why VP-trees?

VP-trees are a space-partitioning data structure that significantly accelerates nearest neighbor searches by using the triangle inequality to prune unpromising branches during traversal.

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.NCDTree

-- Create documents from bytestrings
let docs = [ Document (BL.pack "hello world")
           , Document (BL.pack "hello universe")
           , Document (BL.pack "goodbye world")
           , Document (BL.pack "hello there")
           ]

-- Build the VP-tree index with a leaf threshold of 4
let tree = mkVPTree 4 docs

-- Search for the 2 nearest neighbors of "hello"
let query = Document (BL.pack "hello")
let results = knnSearch 2 query tree

-- results is a max-heap with the closest documents
```

## Key Functions

- **`mkVPTree :: Int -> [Document] -> VPTree`** - Build a VP-tree index from a list of documents. The first argument is the leaf threshold (when to stop splitting nodes).

- **`knnSearch :: Int -> Document -> VPTree -> ResultHeap`** - Find the k nearest neighbors of a query document. Returns a heap of results ordered by distance (smallest first when extracted).

- **`ncd :: BL.ByteString -> BL.ByteString -> Double`** - Compute the normalized compression distance between two bytestrings directly.

## Characteristics

- **Universal metric**: Works with any data that can be compressed, no feature engineering needed
- **Approximate search**: VP-tree pruning makes it an approximate (but highly accurate in practice) nearest neighbor search
- **Lossless comparison**: Based on information-theoretic principles
- **Pure Haskell**: No external dependencies beyond compression libraries

## Testing

The library includes a comprehensive property-based test suite with over 3,100 generated test cases covering:
- Core distance and tree construction properties
- Similarity search correctness
- Edge cases and tree structure invariants

Run tests with:
```bash
stack test
```

## References

- Cleary, J. G., & Trigg, R. H. (1979). "K-W-nearest neighbor classification for statistically dependent data"
- Yianilos, P. N. (1993). "Data structures and algorithms for nearest neighbor search in general metric spaces"
- Li, M., Chen, X., Li, X., Ma, B., & Vitányi, P. M. (2004). "The similarity metric"
