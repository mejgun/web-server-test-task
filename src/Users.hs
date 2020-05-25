{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users
  ( module Users.GetUsers
  , module Users.CreateUser
  )
where

import           Users.CreateUser
import           Users.GetUsers

-- isAdmin :: Connection -> String -> IO Bool
-- isAdmin conn token = do
--   p <- query conn "select admin from users where token = ?" [token]
--   return $ case p of
--     [Only i] -> i
--     _        -> False
