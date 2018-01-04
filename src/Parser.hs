{-# LANGUAGE OverloadedStrings #-}

module Parser where
    import Text.ParserCombinators.Parsec
    import qualified Data.Map.Strict as Map
    import qualified Data.Text as T
    import Control.Monad (void)

    hex :: String
    hex = "0123456789ABCDEF"

    escape :: [String] -> [String]
    escape [] = []
    escape (x:xs) = ['\\' : x] ++ escape xs

    assoc :: Parser (T.Text, T.Text)
    assoc = do
        from <- many1 $ oneOf hex
        spaces
        char ';'
        spaces
        to1 <- many1 $ oneOf hex
        -- spaces
        -- to2 <- many1 $ oneOf hex
        manyTill anyChar (void (char '\n') <|> eof)
        let (x:xs) = escape [from, to1]
        return (T.pack x, T.pack $ concat xs)

    comments :: Parser ()
    comments = do
        (char '#') *> manyTill anyChar (void (char '\n') <|> eof)
        return ()

    newlines :: Parser ()
    newlines = do
        char '\n'
        return ()

    skip = many1 $ (comments <|> newlines)
    line = many1 $ (try skip) *> assoc

    parsing text = case parse line "homoglyphs" text of
                     Left err -> error (show err)
                     Right couple -> couple

    mapping :: String -> Map.Map T.Text T.Text
    mapping text = Map.fromList $ parsing text
