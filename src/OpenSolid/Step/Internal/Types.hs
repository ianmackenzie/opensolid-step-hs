module OpenSolid.Step.Internal.Types where


{-| An attribute value string such as "1", "2.", "'some string'", ".STEEL." or
"#34".
-}
newtype AttributeValue
    = AttributeValue String


{-| A capitalized type name like "IFCWALL".
-}
newtype TypeName
    = TypeName String


{-| A capitalized enum name like "STEEL", with no leading or trailing periods.
-}
newtype EnumName
    = EnumName String


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
    | StringAttribute String
    | BinaryAttribute String
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
    | ParsedStringAttribute String
    | ParsedBinaryAttribute String
    | ParsedEnumAttribute EnumName
    | ParsedReference Int
    | ParsedTypedAttribute TypeName ParsedAttribute
    | ParsedAttributeList [ParsedAttribute]


data ParsedEntity =
    ParsedEntity
        { typeName :: TypeName
        , parsedAttributes :: [ParsedAttribute]
        }
