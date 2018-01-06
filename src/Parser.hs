{-# LANGUAGE OverloadedStrings #-}

module Parser where
    import Text.Parsec.Text
    import Text.Parsec.Combinator
    import Text.Parsec.Prim
    import Text.Parsec.Char
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
        spaces
        tail <- optionMaybe . many1 $ oneOf hex
        let to2 = maybe "" id tail

        manyTill anyChar (void (char '\n'))
        let (x:xs) = escape [from, to1, to2]
        return (T.pack x, T.pack $ concat xs)

    comments :: Parser ()
    comments = do
        (char '#') *> manyTill anyChar (void (char '\n'))
        return ()

    newlines :: Parser ()
    newlines = do
        char '\n'
        return ()

    skip :: Parser ()
    skip = optional . skipMany1 $ (comments <|> newlines)

    line :: Parser [(T.Text, T.Text)]
    line = many1 $ skip *> assoc <* (skip <|> eof)

    parsing text = case parse line "homoglyphs" text of
                     Left err -> error (show err)
                     Right couple -> couple

    mapping :: T.Text -> Map.Map T.Text T.Text
    mapping = Map.fromList . parsing
