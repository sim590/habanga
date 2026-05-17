module I18n where

import I18n.Types
import I18n.Fr (frTranslations)
import I18n.En (enTranslations)
import Data.List (isPrefixOf)
import Data.Default

data Language = French | English
  deriving (Show, Eq, Enum, Bounded)

instance Default Language where
    def = French 

languageFromString :: Maybe String -> Language
languageFromString mlang = case mlang of
  Just lang
    | "en" `isPrefixOf` lang -> English
  _                          -> French
    

getTranslations :: Language -> Translations
getTranslations French = frTranslations
getTranslations English = enTranslations

