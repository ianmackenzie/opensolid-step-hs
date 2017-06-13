module OpenSolid.Step.Types
    ( Header(..)
    , Entity
    , Attribute
    ) where


import qualified Data.Time.Clock as Clock
import qualified OpenSolid.Step.Internal.Types as InternalTypes


{-| Represents the data stored in the header section of a STEP file.
-}
data Header =
    Header
        { fileDescription :: [String]
        , fileName :: String
        , timeStamp :: Clock.UTCTime
        , author :: [String]
        , organization :: [String]
        , preprocessorVersion :: String
        , originatingSystem :: String
        , authorization :: String
        , schemaIdentifiers :: [String]
        }


{-| Represents a single entity storied in the data section of a STEP file, such
as a point, curve, assembly or entire building.
-}
type Entity
    = InternalTypes.Entity


{-| Represents a single attribute of a STEP entity, such as X coordinate value,
GUID string, or a reference to another entity.
-}
type Attribute
    = InternalTypes.Attribute
