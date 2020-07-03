module Lib.DB.Impl.PostgreSQL.Functions where

import           Data.Maybe                     ( catMaybes )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..) )

pgArrayToList :: PGArray (Maybe a) -> [a]
pgArrayToList = catMaybes . fromPGArray

zipPGarrays :: PGArray (Maybe a) -> PGArray (Maybe b) -> [(a, b)]
zipPGarrays a1 a2 = zip (pgArrayToList a1) (pgArrayToList a2)
