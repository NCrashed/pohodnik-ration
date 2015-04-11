module Ration.Person where

data Person = Person {
    personName :: String
  , personSurname :: Maybe String
  , personPatronim :: Maybe String
}