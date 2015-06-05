

module SiteSelfie where

import Test.WebDriver
import Data.Text (Text)
import Data.ByteString.Lazy as BSL

data URL = ExternalURL Text
         | YesodHandler Text
         deriving (Show)



-- takeScreenshot :: String -> String -> IO ()
takeScreenshot url target = runSession defaultConfig $ do
              openPage url
              ss <- screenshot
              return ss
              -- BSL.writeFile target ss
  