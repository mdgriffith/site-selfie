
import System.FilePath
import System.Directory (createDirectoryIfMissing)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime, UTCTime)

import qualified Data.ByteString.Lazy as L

import Control.Applicative
import Control.Monad.IO.Class

import Test.WebDriver


data Base = Base { name::String, url::String, config::WDConfig }

newtype Branch = Branch String
               deriving (Show, Ord, Eq)

getBranch :: Branch -> String
getBranch (Branch t) = t

toURL :: Base -> Branch -> String
toURL base (Branch burl) = (url base) ++ burl

formatPath :: String -> Base -> Branch -> UTCTime -> FilePath
formatPath root base branch date = root 
                               </> (slugifyTime date)
                               </> (name base) 
                               </> (getBranch branch)
                               </> "index.png"

-- formatPNG :: Base -> UTCTime -> FilePath
-- formatPNG base date = (name base) ++ "_" ++ (slugifyTime date) ++ ".png"

slugifyTime :: UTCTime -> String
slugifyTime utc = formatTime defaultTimeLocale "%F_%H-%M-%S" utc

bases :: [Base]
bases =  [Base {name="Dev",  url="http://localhost:8000/",          config=defaultConfig},
          Base {name="Live", url="http://mechanical-elephant.com/", config=defaultConfig}]

branches :: [Branch]
branches = fmap Branch ["", "archive", "about"]

main :: IO ()
main = do 
          time <- getCurrentTime
          _ <- sequence $ runScreenshots time
          return ()
    where runScreenshots time = saveScreenshot "siteshots" time <$> bases <*> branches

saveScreenshot :: String -> UTCTime -> Base -> Branch -> IO ()
saveScreenshot root utc base branch = do
                                        runSession (config base) $ do
                                            openPage pageUrl
                                            ss <- screenshot
                                            liftIO $ createDirectoryIfMissing True (takeDirectory filepath)
                                            liftIO $ L.writeFile filepath ss
                                where pageUrl = toURL base branch
                                      filepath = formatPath root base branch utc






