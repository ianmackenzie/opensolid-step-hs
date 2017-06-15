{-# LANGUAGE OverloadedStrings #-}

module OpenSolid.Step.Parse
    ( Error(..)
    , comment
    , whitespace
    ) where

import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.Attoparsec.ByteString (Parser)
import Data.Word (Word8)
import qualified Data.ByteString as ByteString
import Control.Applicative
import Control.Monad
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


spaces :: Parser ()
spaces =
    Attoparsec.skipWhile (\word -> word == 32 || word == 10 || word == 13)


comment :: Parser ()
comment =
    let
        nonAsterisks =
            Attoparsec.skipWhile (/= 42)

        asterisks =
            Attoparsec.skipWhile (== 42)

        endingSlash =
            void (Attoparsec.word8 47)

        continueComment =
            nonAsterisks *> asterisks *> (endingSlash <|> continueComment)
    in
    Attoparsec.string "/*" *> continueComment


whitespace :: Parser ()
whitespace =
    spaces *> Attoparsec.skipMany (comment *> spaces)


-- comma :: Parser ()
-- comma =
--     Parser.succeed ()
--         |. whitespace
--         |. Parser.symbol ","
--         |. whitespace


-- list :: Parser a -> Parser (List a)
-- list item =
--     Parser.succeed identity
--         |. Parser.symbol "("
--         |. whitespace
--         |= Parser.oneOf
--             [ Parser.symbol ")" |> Parser.map (\() -> [])
--             , Parser.succeed (\first rest -> first : rest)
--                 |= item
--                 |= Parser.repeat Parser.zeroOrMore
--                     (Parser.succeed identity
--                         |. comma
--                         |= item
--                     )
--                 |. whitespace
--                 |. Parser.symbol ")"
--             ]


-- keyword :: Parser String
-- keyword =
--     let
--         validFirstCharacter character =
--             Char.isUpper character || character == '_'

--         validOtherCharacter character =
--             validFirstCharacter character || Char.isDigit character
--     in
--     Parser.source
--         (Parser.ignore (Parser.Exactly 1) validFirstCharacter
--             |. Parser.ignore Parser.zeroOrMore validOtherCharacter
--         )


-- typeName :: Parser Types.TypeName
-- typeName =
--     keyword |> Parser.map Types.TypeName


-- isBasic :: Char -> Bool
-- isBasic character =
--     character /= '\'' && character /= '\\'


-- isHexCharacter :: Char -> Bool
-- isHexCharacter character =
--     (character >= '0' && character <= '9')
--         || (character >= 'A' && character <= 'F')


-- hexDigit :: Parser Int
-- hexDigit =
--     Parser.oneOf
--         [ Parser.symbol "0" |> Parser.map (\() -> 0)
--         , Parser.symbol "1" |> Parser.map (\() -> 1)
--         , Parser.symbol "2" |> Parser.map (\() -> 2)
--         , Parser.symbol "3" |> Parser.map (\() -> 3)
--         , Parser.symbol "4" |> Parser.map (\() -> 4)
--         , Parser.symbol "5" |> Parser.map (\() -> 5)
--         , Parser.symbol "6" |> Parser.map (\() -> 6)
--         , Parser.symbol "7" |> Parser.map (\() -> 7)
--         , Parser.symbol "8" |> Parser.map (\() -> 8)
--         , Parser.symbol "9" |> Parser.map (\() -> 9)
--         , Parser.symbol "A" |> Parser.map (\() -> 10)
--         , Parser.symbol "B" |> Parser.map (\() -> 11)
--         , Parser.symbol "C" |> Parser.map (\() -> 12)
--         , Parser.symbol "D" |> Parser.map (\() -> 13)
--         , Parser.symbol "E" |> Parser.map (\() -> 14)
--         , Parser.symbol "F" |> Parser.map (\() -> 15)
--         ]


-- x0 :: Int -> Int -> String
-- x0 high low =
--     String.fromCodePoints [ Bitwise.shiftLeftBy 4 high + low ]


-- x2 :: List ( Int, Int, Int, Int ) -> String
-- x2 hexDigits =
--     let
--         codePoint ( a, b, c, d ) =
--             d
--                 + Bitwise.shiftLeftBy 4 c
--                 + Bitwise.shiftLeftBy 8 b
--                 + Bitwise.shiftLeftBy 12 a
--     in
--     String.fromCodePoints (List.map codePoint hexDigits)


-- x4 :: List ( Int, Int, Int, Int, Int, Int ) -> String
-- x4 hexDigits =
--     let
--         codePoint ( a, b, c, d, e, f ) =
--             f
--                 + Bitwise.shiftLeftBy 4 e
--                 + Bitwise.shiftLeftBy 8 d
--                 + Bitwise.shiftLeftBy 12 c
--                 + Bitwise.shiftLeftBy 16 b
--                 + Bitwise.shiftLeftBy 20 a
--     in
--     String.fromCodePoints (List.map codePoint hexDigits)


-- string :: Parser String
-- string =
--     Parser.succeed String.concat
--         |. Parser.symbol "'"
--         |= Parser.repeat Parser.zeroOrMore
--             (Parser.oneOf
--                 [ Parser.symbol "''" |> Parser.map (\() -> "'")
--                 , Parser.symbol "\\\\" |> Parser.map (\() -> "\\")
--                 , Parser.succeed x0
--                     |. Parser.symbol "\\X\\"
--                     |= hexDigit
--                     |= hexDigit
--                 , Parser.succeed x2
--                     |. Parser.symbol "\\X2\\"
--                     |= Parser.repeat Parser.oneOrMore
--                         (Parser.succeed (,,,)
--                             |= hexDigit
--                             |= hexDigit
--                             |= hexDigit
--                             |= hexDigit
--                         )
--                     |. Parser.symbol "\\X0\\"
--                 , Parser.succeed x4
--                     |. Parser.symbol "\\X4\\"
--                     |= Parser.repeat Parser.oneOrMore
--                         (Parser.succeed (,,,,,)
--                             |. Parser.symbol "0"
--                             |. Parser.symbol "0"
--                             |= hexDigit
--                             |= hexDigit
--                             |= hexDigit
--                             |= hexDigit
--                             |= hexDigit
--                             |= hexDigit
--                         )
--                     |. Parser.symbol "\\X0\\"
--                 , Parser.keep Parser.oneOrMore isBasic
--                 ]
--             )
--         |. Parser.symbol "'"


-- bool :: Parser Bool
-- bool =
--     Parser.oneOf
--         [ Parser.keyword ".T." |> Parser.map (\() -> True)
--         , Parser.keyword ".F." |> Parser.map (\() -> False)
--         ]


-- default :: Parser ()
-- default =
--     Parser.symbol "*"


-- null :: Parser ()
-- null =
--     Parser.symbol "$"


-- binary :: Parser String
-- binary =
--     Parser.succeed identity
--         |. Parser.symbol "\""
--         |= Parser.keep Parser.oneOrMore isHexCharacter
--         |. Parser.symbol "\""


-- enum :: Parser Types.EnumName
-- enum =
--     Parser.succeed identity
--         |. Parser.symbol "."
--         |= keyword
--         |. Parser.symbol "."
--         |> Parser.map Types.EnumName


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
