module Lecture.Logger where

import Prelude hiding (log)
import Lecture.Encrypt

data Logged a = Logged [String] a
  deriving (Eq, Show)

instance Functor Logged where
  fmap f (Logged strs x) = Logged strs (f x)

instance Applicative Logged where
  pure = Logged []
  (Logged strs f) <*> (Logged strs' x) = Logged (strs ++ strs') (f x)

instance Monad Logged where
  (Logged strs x) >>= f = case f x of
    (Logged strs' y) -> Logged (strs ++ strs') y

log :: String -> Logged ()
log str = Logged [str] ()

users :: [(String, String)]
users =
  [ ("alice", "sehhusjxehiurqjjuhoijqfbu")
  , ("bob", "fqiimeht") ]

authenticateUser :: String -> String -> Logged Bool
authenticateUser username enteredPassword = do
  log  "Authenticating user..."
  log ("  - username: " ++ username)
  log  "  - password: [REDACTED]"

  log "Looking up user..."
  case lookup username users of
    Nothing -> do
      log "No such user exists."
      return False

    (Just encryptedPassword) -> do
      log "User found."
      validateLogin encryptedPassword enteredPassword

decryptPassword :: String -> Logged String
decryptPassword encryptedPassword = do
  log "Decrypting password..."
  return (decrypt 42 encryptedPassword)

validateLogin :: String -> String -> Logged Bool
validateLogin encryptedPassword enteredPassword = do
  log "Validating password..."
  password <- decryptPassword encryptedPassword
  let isValid = password == enteredPassword
  log (if isValid then "Login successful." else "Login failed.")
  return isValid
