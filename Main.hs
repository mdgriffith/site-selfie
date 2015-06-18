
import System.FilePath
import System.Directory (createDirectoryIfMissing)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Word

import qualified Data.ByteString.Lazy as L

import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent ( threadDelay )

import Test.WebDriver



data Base = Base { name::String, url::String }

type Branch = String

type Resolution = (Word, Word)

toURL :: Base -> Branch -> String
toURL base branch = (url base) ++ branch

formatPath :: String -> Base -> Branch -> UTCTime -> Resolution -> FilePath
formatPath root base branch date (w, h) = root 
                               </> (slugifyTime date)
                               </> (name base) ++ "_" ++ show w ++ "x" ++ show h
                               </> branch
                               </> "index.png"

-- formatPNG :: Base -> UTCTime -> FilePath
-- formatPNG base date = (name base) ++ "_" ++ (slugifyTime date) ++ ".png"

slugifyTime :: UTCTime -> String
slugifyTime utc = formatTime defaultTimeLocale "%F_%H-%M-%S" utc

bases :: [Base]
bases =  [Base { name="Dev",  
                 url="http://localhost:8000/"
               },
          Base { name="Live", 
                 url="http://mechanical-elephant.com/"
               }
          ]

resolutions :: [Resolution]
resolutions = [(1366, 768), (320, 320)]

branches :: [Branch]
branches = ["", "archive", "about"]

main :: IO ()
main = do 
          time <- getCurrentTime
          _ <- sequence $ runScreenshots time
          return ()
    where runScreenshots time = saveScreenshot "siteshots" time defaultConfig <$> bases <*> branches <*> resolutions

saveScreenshot :: String -> UTCTime -> WDConfig -> Base -> Branch -> Resolution -> IO ()
saveScreenshot root utc config base branch res = do
                runSession config $ do
                    openPage pageUrl
                    setWindowSize res
                    liftIO $ threadDelay 750
                    ss <- screenshot
                    window <- getCurrentWindow
                    closeWindow window
                    liftIO $ createDirectoryIfMissing True (takeDirectory filepath)
                    liftIO $ L.writeFile filepath ss
        where pageUrl = toURL base branch
              filepath = formatPath root base branch utc res






