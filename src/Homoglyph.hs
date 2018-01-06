{-# LANGUAGE OverloadedStrings #-}

module Homoglyph where
    import qualified Data.Map.Strict as Map
    import Data.Text hiding (any)
    import qualified Data.Text.IO as TIO
    import Dhall hiding (Text)
    import Parser (mapping)

    pickConfusables :: [Text] -> Map.Map Text Text -> Map.Map Text Text
    pickConfusables charSet confusables =
        Map.filterWithKey (\k v -> isPart k || isPart v) confusables
            where isPart a = any (==a) charSet

    substitute :: Map.Map Text Text -> Text -> Text
    substitute map text = case Map.keys map of
                            [] -> text
                            (x:xs) -> substitute (newMap x) (newText x)
                where k = Map.keys map
                      newText x = replace x (map Map.! x) text
                      newMap x = Map.delete x map

    normalize :: Text -> IO Text
    normalize str = do
        characterSet <- input auto "./src/config/basic_latin.dhall"
        homoglyphs <- TIO.readFile "./src/config/homoglyphs"
        let homoMap = mapping homoglyphs
        let subset = pickConfusables characterSet homoMap
        return $ substitute subset str
