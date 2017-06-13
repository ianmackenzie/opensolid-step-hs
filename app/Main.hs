module Main where

import qualified OpenSolid.Step.Types as Step
import qualified Data.Time.Clock as Clock
import qualified OpenSolid.Step.Header as Step.Header


createHeader :: Clock.UTCTime -> Step.Header
createHeader time =
    Step.Header
        { Step.fileDescription = []
        , Step.fileName = "test.ifc"
        , Step.timeStamp = time
        , Step.author = ["Ian Mackenzie"]
        , Step.organization = []
        , Step.preprocessorVersion = "1.0.0"
        , Step.originatingSystem = "opensolid-step"
        , Step.authorization = ""
        , Step.schemaIdentifiers = []
        }


main :: IO ()
main = do
    now <- Clock.getCurrentTime
    print (Step.Header.timeStamp (createHeader now))
