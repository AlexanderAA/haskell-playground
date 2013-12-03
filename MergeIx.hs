{-# LANGUAGE DeriveDataTypeable, GADTs #-}

-- | Merging of indexed types

module MergeIx ( 
    main,
    merge,
    runTests,
    Col(..),
    Row(..),
    Val(..),
    SV(..)
) where

import           Data.Typeable
import qualified Data.IxSet                  as  IX
import qualified Test.QuickCheck             as  QC

--------------------------------------------------------------------------------
--Types-------------------------------------------------------------------------
data Col = Col Int        deriving (Eq, Show, Ord, Typeable)
data Row = Row Int        deriving (Eq, Show, Ord, Typeable)
data Val = Val Int        deriving (Eq, Show, Ord, Typeable)
data SV = SV Col Row Val  deriving (Eq, Show, Typeable)
--------------------------------------------------------------------------------

getIxCol :: SV -> [Col]
getIxCol (SV col _ _) = [col]

getIxRow :: SV -> [Row]
getIxRow (SV _ row _) = [row]

instance IX.Indexable SV where
    empty = IX.ixSet [IX.ixFun getIxCol, IX.ixFun getIxRow]

instance Ord SV where
    compare (SV acol arow aval) (SV bcol brow bval) = 
            case ccol of
                LT -> LT
                EQ -> crow
                GT -> GT
            where 
                ccol = compare acol bcol
                crow = compare arow brow


compareVal :: SV -> SV -> Ordering
compareVal a@(SV ac ar av) b@(SV bc br bv) = (compare av bv)

-- | Merge one element into IxSet
mergeSV :: SV -> IX.IxSet SV -> IX.IxSet SV
mergeSV el@(SV col row val) svset = do
            case ex of 
                Nothing ->  IX.insert el svset
                Just ex ->  case (compareVal ex el) of
                                LT -> IX.insert el $ IX.delete ex svset
                                EQ -> svset
                                GT -> svset
                
            where 
                ex = IX.getOne $ (IX.getEQ col . IX.getEQ row) svset

-- | Recursively merge list of elements into IxSet
mergeIxSV :: [SV] -> IX.IxSet SV -> IX.IxSet SV
mergeIxSV newSV@(x:xs) currentSV = mergeIxSV xs (mergeSV x currentSV)
mergeIxSV [] currentSV = currentSV

-- | Merge two IxSets
merge :: IX.IxSet SV -> IX.IxSet SV -> IX.IxSet SV
merge seqA seqB = mergeIxSV (IX.toList seqA) seqB

--------------------------------------------------------------------------------
--Tests-------------------------------------------------------------------------
instance QC.Arbitrary SV where
    arbitrary = do
        col <- QC.arbitrary
        row <- QC.arbitrary
        val <- QC.arbitrary
        return $ SV (Col col) (Row row) (Val val)

instance (a ~ SV) => QC.Arbitrary (IX.IxSet a) where
    arbitrary = do
        --svList <- QC.vector 500000 :: QC.Gen [SV] -- Test memory limits
        svList <- QC.arbitrary :: QC.Gen [SV]
        let ix = IX.fromList svList :: IX.IxSet SV
        return $ ix

-- | Length of (merge a b) and (union a b) are the same. 
testMergeResultLength :: IX.IxSet SV -> IX.IxSet SV -> Bool
testMergeResultLength a b = (IX.size $ merge a b) == (IX.size $ IX.union a b)

-- | The result does not depend on the order of merge
testMergeAssociativity :: IX.IxSet SV -> IX.IxSet SV -> Bool
testMergeAssociativity a b = (merge a b) == (merge b a)

runTests = do
    QC.quickCheck $ testMergeResultLength QC..&&. testMergeAssociativity
--------------------------------------------------------------------------------

runExamples = do 
    let seq0 = IX.fromList [(SV (Col 1) (Row 2) (Val 3)), 
                            (SV (Col 1) (Row 3) (Val 3)), 
                            (SV (Col 1) (Row 3) (Val 5)), 
                            (SV (Col 1) (Row 5) (Val 3))] :: IX.IxSet SV
    print $ seq0
    -- -------------------------------------------------------------------------
    let el = (SV (Col 1) (Row 2) (Val 7))
    print $ mergeSV el seq0
    -- -------------------------------------------------------------------------
    let seq1 = IX.fromList [(SV (Col 2) (Row 2) (Val 3)), 
                            (SV (Col 1) (Row 3) (Val 7)), 
                            (SV (Col 1) (Row 5) (Val 0))] :: IX.IxSet SV
    print seq1
    -- -------------------------------------------------------------------------
    print $ mergeIxSV (IX.toList seq1) seq0
    -- -------------------------------------------------------------------------
    print (IX.stats seq1)
    -- -------------------------------------------------------------------------
    --QC.sample (QC.arbitrary :: QC.Gen (IX.IxSet SV))
    -- -------------------------------------------------------------------------


main = do
    runExamples
    runTests
    
