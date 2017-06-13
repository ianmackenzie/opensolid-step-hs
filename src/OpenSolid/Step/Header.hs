module OpenSolid.Step.Header
    ( fileDescription
    , fileName
    , timeStamp
    , author
    , organization
    , preprocessorVersion
    , originatingSystem
    , authorization
    , schemaIdentifiers
    ) where

import qualified Data.Time.Clock as Clock
import OpenSolid.Step.Types (Header(Header))
import qualified OpenSolid.Step.Types as Types


fileDescription :: Header -> [String]
fileDescription header =
    Types.fileDescription (header :: Header)


fileName :: Header -> String
fileName header =
    Types.fileName (header :: Header)


timeStamp :: Header -> Clock.UTCTime
timeStamp header =
    Types.timeStamp (header :: Header)


author :: Header -> [String]
author header =
    Types.author (header::Header)


organization :: Header -> [String]
organization header =
    Types.organization (header::Header)


preprocessorVersion :: Header -> String
preprocessorVersion header =
    Types.preprocessorVersion (header::Header)


originatingSystem :: Header -> String
originatingSystem header =
    Types.originatingSystem (header::Header)


authorization :: Header -> String
authorization header =
    Types.authorization (header::Header)


schemaIdentifiers :: Header -> [String]
schemaIdentifiers header =
    Types.schemaIdentifiers (header::Header)

