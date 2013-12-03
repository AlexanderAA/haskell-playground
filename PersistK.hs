module PersistK (
    K(..)
) where

import qualified Data.Text                     as T
import qualified Database.Persist              as P
import qualified Database.Persist.TH           as PT
import qualified Database.Persist.Postgresql   as PG
import qualified Database.Esqueleto            as E

data K = K String deriving Show
instance E.PersistField K where
    toPersistValue (K s) = (E.PersistText . T.pack) s
    fromPersistValue (E.PersistText s) = Right $ (K (T.unpack s))


instance PG.PersistFieldSql K where
    sqlType _ = E.SqlString
