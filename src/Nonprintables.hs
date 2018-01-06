{-# LANGUAGE OverloadedStrings #-}

module Nonprintables where
    import Dhall hiding (Text)
    import Data.Text hiding (any, foldr)

    substitute :: Text -> [Text] -> Text
    substitute t np = foldr (\e a -> replace e "" a) t np

    normalize :: Text -> IO Text
    normalize str = do
        nonPrintables <- input auto "./src/config/non_printables.dhall"
        return $ substitute str nonPrintables
