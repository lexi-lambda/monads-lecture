module Lecture.Vault where

import Data.Maybe
import Text.Printf

import Lecture.Encrypt
import Lecture.World

type UserId = String
data UserData = UserData { favoriteFood :: String }

type EncryptionWorld = World EncryptionKey

database :: [(String, UserData)]
database =
  [ ("Alyssa", UserData "fyppq")
  , ("Ben"   , UserData "iqbqt") ]

getUserData :: UserId -> UserData
getUserData uId = fromJust (lookup uId database)

renderGreeting :: UserId -> EncryptionWorld String
renderGreeting uId = do
  food <- getUserFavoriteFood uId
  return $ printf "Hello, %v. Your favorite food is %v." uId food

getUserFavoriteFood :: UserId -> EncryptionWorld String
getUserFavoriteFood uId = decryptUserFavoriteFood (getUserData uId)

decryptUserFavoriteFood :: UserData -> EncryptionWorld String
decryptUserFavoriteFood userData = do
  key <- getWorld
  return $ decrypt key (favoriteFood userData)
