{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AesonTut where

import Data.Aeson

import Data.HashMap.Strict qualified as HM
import Data.List qualified as L

import Data.Text
import GHC.Generics

import Data.ByteString.Lazy qualified as LB
import GHC.Base (Any)

data Person = Person
  {name :: Text, age :: Maybe Integer}
  deriving (Generic, Show)

-- -- Allows decoding from JSON
instance FromJSON Person where
  parseJSON = withObject "Person" $ \obj -> do
    personName <- obj .: "name"
    personAge <- obj .:? "age"
    return $ Person personName personAge

-- -- Allows encoding to JSON
instance ToJSON Person where
  -- toJSON :: Person -> Value
  toJSON (Person name age) = object ["name" .= name, "age" .= age]

jsonString :: LB.ByteString
jsonString = "{ \"age\": 20, \"name\": \"Ola\" }"

maybeFoo :: Maybe Person
maybeFoo = decode jsonString

newPerson :: Person
newPerson = Person{name = "Ola", age = Just 20}

customValue :: Value
customValue =
  object
    [ "list_price" .= (150000 :: Int)
    , "sale_price" .= (143000 :: Int)
    , "description" .= ("2-bedroom townhouse" :: String)
    ]

data UserType = User | Admin | CustomerSupport deriving (Show, Generic)

instance ToJSON UserType where
  toJSON :: UserType -> Value
  toJSON = \case
    User -> "user"
    Admin -> "admin"
    CustomerSupport -> "customer_support"

instance FromJSON UserType where
  parseJSON = withText "UserType" $ \case
    "user" -> return User
    "admin" -> return Admin
    "customer_support" -> return CustomerSupport
    _ -> fail "Error"

data APIResult = JSONData Value | Error' Text deriving (Show)

instance FromJSON APIResult where
  parseJSON = withObject "APIResult" $ \obj -> do
    ok <- obj .: "ok"
    if ok
      then JSONData <$> (obj .: "data")
      else Error' <$> (obj .: "error_msg")

goodData :: LB.ByteString
goodData = "{\"ok\":true,\"data\":{\"foo\":2}}"

badData :: LB.ByteString
badData = "{\"ok\":false,\"error_msg\":\"no_credentials\"}"

newtype JSONHashList a = HashList [a] deriving (Show)

-- instance (FromJSON a) => FromJSON (JSONHashList a) where
--   parseJSON = withObject "JSONHashList" $ \obj ->
--     let kvPairs = HM.toList obj
--         sorted = L.sortOn fst kvPairs
--         values = L.map snd sorted
--         parsed = mapM parseJSON values
--      in HashList <$> parsed
