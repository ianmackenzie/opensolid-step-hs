{-# LANGUAGE OverloadedStrings #-}

module OpenSolid.Step.Parse
    ( Error(..)
    , parseComment
    , parseWhitespace
    , parseList
    , parseKeyword
    ) where

import Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.Word
import Data.ByteString as ByteString
import Control.Applicative
import Control.Monad
import OpenSolid.Step.Internal.Types as Types
import Data.Text.Encoding as Encoding
import Data.Text as Text


{-| Types of errors that can be encountered when parsing a file:

  - A `ParseError` means an error actually parsing STEP text; this means that
    either the STEP file is improperly formatted or (more likely!) it uses an
    aspect of STEP syntax that is not yet supported by this package. The
    parameter is an error string that can be used for debugging.
  - A `ResolveError` means that the file was parsed OK, but an error occurred
    when a reference such as `#23` was found in one entity but no entity with
    that ID existed in the file. The integer parameter is the ID of the
    nonexistent entity.

-}
data Error
    = ParseError String
    | ResolveError Int



parseComment :: Parser ()
parseComment =
    let
        nonAsterisks =
            Attoparsec.skipWhile (/= '*')

        asterisks =
            Attoparsec.skipWhile (== '*')

        endingSlash =
            void (Attoparsec.char '/')

        continueComment =
            nonAsterisks *> asterisks *> (endingSlash <|> continueComment)
    in
    Attoparsec.string "/*" *> continueComment


parseWhitespace :: Parser ()
parseWhitespace =
    Attoparsec.skipSpace
        *> Attoparsec.skipMany (parseComment *> Attoparsec.skipSpace)


parseComma :: Parser ()
parseComma =
    parseWhitespace *> Attoparsec.char ',' *> parseWhitespace


parseList :: Parser a -> Parser [a]
parseList item =
    Attoparsec.char '('
        *> parseWhitespace
        *> Attoparsec.sepBy item parseComma
        <* parseWhitespace
        <* Attoparsec.char ')'


parseKeyword :: Parser Text
parseKeyword =
    let
        validFirst char =
            (char >= 'A' && char <= 'Z') || char == '_'

        validOther char =
            validFirst char || (char >= '0' && char <= '9')

        skipKeyword =
            Attoparsec.satisfy validFirst *> Attoparsec.skipWhile validOther
    in
    Encoding.decodeUtf8 <$> fst <$> Attoparsec.match skipKeyword


parseTypeName :: Parser Types.TypeName
parseTypeName =
    Types.TypeName <$> parseKeyword


parseText :: Parser Text
parseText =
    let
        apostrophe =
            void (Attoparsec.char '\'')

        escapedApostrophe =
            void (Attoparsec.string "''")

        continueText =
            Attoparsec.skipWhile (/= '\'')
                *> ((escapedApostrophe *> continueText) <|> apostrophe)

        trim =
            ByteString.init . ByteString.tail

        parser =
             apostrophe *> continueText
    in
    Encoding.decodeUtf8 <$> trim <$> fst <$> Attoparsec.match parser


parseBinary :: Parser Text
parseBinary =
    Encoding.decodeUtf8 <$>
        (Attoparsec.char '"'
            *> Attoparsec.takeWhile (/= '"')
            <* Attoparsec.char '"'
        )


parseTrue :: Parser Bool
parseTrue =
    Attoparsec.string ".T." *> return True


parseFalse :: Parser Bool
parseFalse =
    Attoparsec.string ".T." *> return False


parseDefault :: Parser ()
parseDefault =
    void (Attoparsec.char '*')


parseNull :: Parser ()
parseNull =
    void (Attoparsec.char '$')


parseEnum :: Parser Types.EnumName
parseEnum =
    Types.EnumName <$>
        (Attoparsec.char '.'*> parseKeyword <* Attoparsec.char '.')


parseId :: Parser Int
parseId =
    Attoparsec.char '#' *> Attoparsec.decimal

-- id :: Parser Int
-- id =
--     Parser.succeed identity
--         |. Parser.symbol "#"
--         |= Parser.int


-- attribute :: Parser Types.ParsedAttribute
-- attribute =
--     let
--         defaultAttribute =
--             default |> Parser.map (\() -> Types.ParsedDefaultAttribute)

--         nullAttribute =
--             null |> Parser.map (\() -> Types.ParsedNullAttribute)

--         boolAttribute =
--             bool |> Parser.map Types.ParsedBoolAttribute

--         optionalSign =
--             Parser.oneOf
--                 [ Parser.symbol "+", Parser.symbol "-", Parser.succeed () ]

--         digits =
--             Parser.ignore Parser.oneOrMore Char.isDigit

--         optionalDigits =
--             Parser.ignore Parser.zeroOrMore Char.isDigit

--         numericAttribute =
--             Parser.sourceMap (,)
--                 (Parser.succeed identity
--                     |. optionalSign
--                     |. digits
--                     |= Parser.oneOf
--                         [ Parser.succeed True
--                             |. Parser.symbol "."
--                             |. optionalDigits
--                             |. Parser.oneOf
--                                 [ Parser.symbol "E" |. optionalSign |. digits
--                                 , Parser.succeed ()
--                                 ]
--                         , Parser.succeed False
--                         ]
--                 )
--                 |> Parser.andThen
--                     (\( string, isFloat ) ->
--                         if isFloat then
--                             case String.toFloat string of
--                                 Ok value ->
--                                     Parser.succeed
--                                         (Types.ParsedFloatAttribute value)

--                                 Err message ->
--                                     Parser.fail message
--                         else
--                             case String.toInt string of
--                                 Ok value ->
--                                     Parser.succeed
--                                         (Types.ParsedIntAttribute value)

--                                 Err message ->
--                                     Parser.fail message
--                     )

--         stringAttribute =
--             string |> Parser.map Types.ParsedStringAttribute

--         binaryAttribute =
--             binary |> Parser.map Types.ParsedBinaryAttribute

--         enumAttribute =
--             enum |> Parser.map Types.ParsedEnumAttribute

--         unevaluatedReference =
--             id |> Parser.map Types.ParsedReference

--         typedAttribute =
--             Parser.succeed Types.ParsedTypedAttribute
--                 |= typeName
--                 |. whitespace
--                 |. Parser.symbol "("
--                 |. whitespace
--                 |= Parser.lazy (\() -> attribute)
--                 |. whitespace
--                 |. Parser.symbol ")"

--         attributeList =
--             list (Parser.lazy (\() -> attribute))
--                 |> Parser.map Types.ParsedAttributeList
--     in
--     Parser.oneOf
--         [ defaultAttribute
--         , nullAttribute
--         , boolAttribute
--         , enumAttribute
--         , numericAttribute
--         , stringAttribute
--         , binaryAttribute
--         , unevaluatedReference
--         , typedAttribute
--         , attributeList
--         ]


-- entity :: Parser Types.ParsedEntity
-- entity =
--     Parser.succeed Types.ParsedEntity
--         |= typeName
--         |. whitespace
--         |= list attribute


-- entityInstance :: Parser ( Int, Types.ParsedEntity )
-- entityInstance =
--     Parser.succeed (,)
--         |= id
--         |. whitespace
--         |. Parser.symbol "="
--         |. whitespace
--         |= entity
--         |. whitespace
--         |. Parser.symbol ";"
--         |. whitespace


-- date :: Parser Date
-- date =
--     let
--         toDate string =
--             Date.fromString string |> Result.withDefault (Date.fromTime 0)
--     in
--     Parser.succeed toDate
--         |. Parser.symbol "'"
--         |= Parser.source
--             (Parser.succeed ()
--                 |. Parser.ignore (Parser.Exactly 4) Char.isDigit
--                 |. Parser.symbol "-"
--                 |. Parser.ignore (Parser.Exactly 2) Char.isDigit
--                 |. Parser.symbol "-"
--                 |. Parser.ignore (Parser.Exactly 2) Char.isDigit
--                 |. Parser.symbol "T"
--                 |. Parser.ignore (Parser.Exactly 2) Char.isDigit
--                 |. Parser.symbol ":"
--                 |. Parser.ignore (Parser.Exactly 2) Char.isDigit
--                 |. Parser.symbol ":"
--                 |. Parser.ignore (Parser.Exactly 2) Char.isDigit
--                 |. Parser.oneOf
--                     [ Parser.succeed ()
--                         |. Parser.oneOf [ Parser.symbol "+", Parser.symbol "-" ]
--                         |. Parser.ignore (Parser.Exactly 2) Char.isDigit
--                         |. Parser.symbol ":"
--                         |. Parser.ignore (Parser.Exactly 2) Char.isDigit
--                     , Parser.succeed ()
--                     ]
--             )
--         |. Parser.symbol "'"


-- header :: Parser Header
-- header =
--     let
--         start typeName =
--             Parser.succeed ()
--                 |. Parser.keyword typeName
--                 |. whitespace
--                 |. Parser.symbol "("
--                 |. whitespace

--         end =
--             Parser.succeed ()
--                 |. whitespace
--                 |. Parser.symbol ")"
--                 |. whitespace
--                 |. Parser.symbol ";"
--                 |. whitespace

--         stringList =
--             list string
--     in
--     Parser.succeed Header
--         |. start "FILE_DESCRIPTION"
--         |= stringList
--         |. comma
--         |. Parser.keyword "'2;1'"
--         |. end
--         |. start "FILE_NAME"
--         |= string
--         |. comma
--         |= date
--         |. comma
--         |= stringList
--         |. comma
--         |= stringList
--         |. comma
--         |= string
--         |. comma
--         |= string
--         |. comma
--         |= string
--         |. end
--         |. start "FILE_SCHEMA"
--         |= stringList
--         |. end


-- fileParser :: Parser ( Header, List ( Int, Types.ParsedEntity ) )
-- fileParser =
--     Parser.succeed (,)
--         |. Parser.keyword "ISO-10303-21;"
--         |. whitespace
--         |. Parser.keyword "HEADER;"
--         |. whitespace
--         |= header
--         |. whitespace
--         |. Parser.keyword "ENDSEC;"
--         |. whitespace
--         |. Parser.keyword "DATA;"
--         |. whitespace
--         |= Parser.repeat Parser.zeroOrMore entityInstance
--         |. whitespace
--         |. Parser.keyword "ENDSEC;"
--         |. whitespace
--         |. Parser.keyword "END-ISO-10303-21;"
--         |. whitespace
--         |. Parser.end


-- toParseError :: Parser.Error -> Error
-- toParseError { row, col, problem } =
--     ParseError row col (toString problem)


-- toResolveError :: EntityResolution.Error -> Error
-- toResolveError (EntityResolution.NonexistentEntity id) =
--     ResolveError id


-- {-| Attempt to parse a string of text loaded from a STEP file. On success,
-- returns a record containing information from the file header and a `Dict`
-- containing `Entity` values indexed by their ID.
-- -}
-- file :: String -> Result Error ( Header, Dict Int Entity )
-- file string =
--     Parser.run fileParser (String.join "\n" (String.lines string))
--         |> Result.mapError toParseError
--         |> Result.andThen
--             (\( header, parsedEntityInstances ) ->
--                 EntityResolution.resolve parsedEntityInstances
--                     |> Result.mapError toResolveError
--                     |> Result.map (\entities -> ( header, entities ))
--             )
