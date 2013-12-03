-- | JSON array parsing in Haskell
--   Input data in JSON format (3 elements): "[1, 2, \"abc\"]"   
--   Output data: (SChange (Int 1) (Int 2) (String "abc"))

{-# LANGUAGE OverloadedStrings #-}

module ParseJsonArray ( 
    main 
) where

import           Data.Text                       (Text(..), pack, unpack)
import           Test.QuickCheck
import qualified Data.Aeson                  as  AE
import qualified Data.Aeson.Types            as  AT
import qualified Data.Attoparsec.Number      as  N
import qualified Data.Vector                 as  V

-- Data type to convert to and from---------------------------------------------
data SChange = SChange Integer Double Text  deriving (Eq, Show)

-- FromJSON --------------------------------------------------------------------
instance AT.FromJSON SChange where
    parseJSON (AT.Array a) =
        case ((V.length a) == 3) of
            True  -> return $ sChange (a V.! 0) (a V.! 1) (a V.! 2)
            False -> fail "parseJSON: Must be an array of exactly 3 elements."

-- | This function is not necessary. 
--   This is just another place to customize the conversion from JSON
sChange :: AT.Value -> AT.Value -> AT.Value -> SChange
sChange (AT.Number (N.I v)) (AT.Number (N.D u)) (AT.String w) =  SChange v u w
sChange _ _ _ = error "Invalid column or row"


-- ToJSON ----------------------------------------------------------------------
instance AT.ToJSON SChange where
    toJSON (SChange v u w) = 
        AT.Array (V.fromList [AT.toJSON v,AT.toJSON u,AT.toJSON w])


-- Testing ---------------------------------------------------------------------
-- | Let quickCheck know how to generate "Random" SChange instances
instance Arbitrary SChange where
    arbitrary = do
        v <- arbitrary
        u <- arbitrary
        let w = "abc"  -- XXX: add  instance Arbitrary Text
        return $ SChange v u w

-- | Test conversion to and from
testSChangeConv :: SChange -> Bool
testSChangeConv update = ((AE.decode . AE.encode) update) == (Just update)

main = do
    print "Running tests"
    (quickCheck) testSChangeConv
