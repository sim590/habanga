module I18n where

import Data.List (isPrefixOf)
import Data.Default

import I18n.Types
import I18n.Fr (frTranslations)
import I18n.En (enTranslations)


data Language = French | English
  deriving (Show, Eq, Enum, Bounded)

instance Default Language where
  def = French 
--make class Translation a where  --in core
--        frTranslations :: a
--        enTranslations :: a
--getTranslations :: Translations a => Language -> a where the translations type is chosen
--getTranslations French = frTranslations
--getTranslations English = enTranslations
--data Translations = Translations { tuiTranslations :: Translations
--                                 , nodeTranslations :: Translations
--                                 , guiTranslations :: Translations 
--                                 }

languageFromString :: Maybe String -> Language
languageFromString mlang = case mlang of
  Just lang
    | "en" `isPrefixOf` lang -> English
  _                          -> French
    

getTranslations :: Language -> Translations
getTranslations French = frTranslations
getTranslations English = enTranslations

