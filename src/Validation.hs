module Validation where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Data.Validation
import           Protolude hiding (Text)

import Types

isNameLongEnough :: Text -> Validation [Text] Text
isNameLongEnough name = if Text.length name >= 1
                        then Success name
                        else Failure ["Name must contain 1 or more characters"]

isNameShortEnough :: Text -> Validation [Text] Text
isNameShortEnough name = if Text.length name <= 80
                         then Success name
                         else Failure ["Name must be less than 80 characters"]

isValidName :: Text -> Validation [Text] Text
isValidName name = pure name <* isNameLongEnough name <* isNameShortEnough name

isEmailLongEnough :: Email -> Validation [Text] Email
isEmailLongEnough e@(Email email) =
  if Text.length email >= 3
  then Success e
  else Failure ["Email must be at least 3 characters"]

doesEmailHaveAtInMiddle :: Email -> Validation [Text] Email
doesEmailHaveAtInMiddle e@(Email email) =
  if not (Text.isPrefixOf "@" email) &&
     Text.isInfixOf "@" email &&
     not (Text.isSuffixOf "@" email)
  then Success e
  else Failure ["Email must contain an '@' character in the middle"]

isValidEmail :: Email -> Validation [Text] Text
isValidEmail e@(Email email) =
  pure email
  <* isEmailLongEnough e
  <* doesEmailHaveAtInMiddle e

isValidUser :: User Maybe state -> Validation [Text] [Text]
isValidUser user =
  let firstName = maybe "" identity (userFirstName user)
      lastName  = maybe "" identity (userLastName user)
      email     = maybe (Email "") identity (userEmail user)
  in sequenceA [ isValidName firstName
               , isValidName lastName
               , isValidEmail email
               ]
