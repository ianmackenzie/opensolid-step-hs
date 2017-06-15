module OpenSolid.Step.Internal.Types where

import Data.Text

{-| Attribute value text such as "1", "2.", "'some string'", ".STEEL." or "#34".
-}
newtype AttributeValue
    = AttributeValue Text


{-| A capitalized type name like "IFCWALL".
-}
newtype TypeName
    = TypeName Text


{-| A capitalized enum name like "STEEL", with no leading or trailing periods.
-}
newtype EnumName
    = EnumName Text


{-| A single STEP entity.
-}
data Entity
    = Entity TypeName [Attribute]


{-| An attribute of a STEP entity.
-}
data Attribute
    = DefaultAttribute
    | NullAttribute
    | BoolAttribute Bool
    | IntAttribute Int
    | DoubleAttribute Double
    | TextAttribute Text
    | BinaryAttribute Text
    | EnumAttribute EnumName
    | ReferenceTo Entity
    | TypedAttribute TypeName Attribute
    | AttributeList [Attribute]


data ParsedAttribute
    = ParsedDefaultAttribute
    | ParsedNullAttribute
    | ParsedBoolAttribute Bool
    | ParsedIntAttribute Int
    | ParsedDoubleAttribute Double
    | ParsedTextAttribute Text
    | ParsedBinaryAttribute Text
    | ParsedEnumAttribute EnumName
    | ParsedReference Int
    | ParsedTypedAttribute TypeName ParsedAttribute
    | ParsedAttributeList [ParsedAttribute]


data ParsedEntity =
    ParsedEntity
        { typeName :: TypeName
        , parsedAttributes :: [ParsedAttribute]
        }
