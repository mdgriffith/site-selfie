
import System.FilePath
import System.Directory (createDirectoryIfMissing)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Word
import Data.List (nubBy, foldl')

import qualified Data.ByteString.Lazy as L

import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent ( threadDelay )

import Test.WebDriver
import Test.WebDriver.Commands.Wait

import Codec.Picture
import Codec.Picture.Types

import Debug.Trace


data Base = Base { baseName::String, url::String } deriving (Show, Eq)

type Page = String

type Resolution = (Word, Word)

type ConfigName = String

data NamedWDConfig = NamedWDConfig { configName::String
                                   , driverConfig::WDConfig 
                                   } deriving (Show, Eq)

instance Eq WDConfig
instance Show WDConfig

data SelfieConfig = SelfieConfig { bases :: [Base]
                                 , sitemap :: [Page]
                                 , resolutions :: [Resolution]
                                 , saveDir :: FilePath
                                 , webDriverConfigs :: [NamedWDConfig]
                                 } 


showResolution :: Resolution -> String
showResolution (w, h) = (show w) ++ "x" ++ (show h)

data Screenshot = Screenshot { filepath :: FilePath
                             , base :: Base
                             , page :: Page
                             , res :: Resolution
                             , wdConfigName :: String
                             } deriving (Show, Eq)


describe :: Screenshot -> String
describe ss = baseName (base ss) 
           ++ "@" ++ showResolution (res ss)
           ++ wdConfigName ss


data SSComparison = SSComparison { branch :: Page
                                 , description :: String
                                 , resolution :: Resolution
                                 , screenshot1 :: Screenshot
                                 , screenshot2 :: Screenshot
                                 , screenshotDiff :: FilePath
                                 , percentDiff :: Float
                                 }

ffWDConfig :: WDConfig
ffWDConfig = defaultConfig { wdCapabilities = defaultCaps {browser = firefox}}

chromeWDConfig :: WDConfig
chromeWDConfig = defaultConfig { wdCapabilities = defaultCaps {browser = chrome}}

selfieConfig :: SelfieConfig
selfieConfig = SelfieConfig {
                bases =  [ Base { baseName="Dev",  
                                  url="http://localhost:8000/"
                                }
                         , Base { baseName="Live", 
                                  url="http://mechanical-elephant.com/"
                                }
                         ]
              , sitemap = ["", "archive", "about"]
              , resolutions = [(1366, 768), (320, 320)]
              , saveDir = "siteshots"
              , webDriverConfigs = [ NamedWDConfig { configName="Firefox"
                                                   , driverConfig=ffWDConfig
                                                   }
                                   , NamedWDConfig { configName="Chrome"
                                                   , driverConfig=chromeWDConfig
                                                   }
                                   ]
             }


-- selfieConfig :: SelfieConfig
-- selfieConfig = SelfieConfig {
--                 bases =  [ Base { baseName="Dev",  
--                                   url="http://localhost:8000/"
--                                 }
--                          , Base { baseName="Live", 
--                                   url="http://mechanical-elephant.com/"
--                                 }
--                          ]
--               , sitemap = [""]
--               , resolutions = [(1366, 768)]
--               , saveDir = "siteshots"
--               , webDriverConfigs = [ NamedWDConfig { configName="Firefox"
--                                                    , driverConfig=ffWDConfig
--                                                    }
--                                    ]
--              }



toURL :: Base -> Page -> String
toURL base page = (url base) ++ page

formatPath :: String -> Base -> Page -> UTCTime -> Resolution -> ConfigName -> FilePath
formatPath dir base page date res cfgName  = dir 
                           </> (slugifyTime date)
                           </> nonEmpty page ++ "_"  ++ (showResolution res) 
                                ++ "_" ++ (baseName base) 
                                ++ "_" ++ cfgName <.> "png"
                      where nonEmpty "" = "index"
                            nonEmpty x = x

slugifyTime :: UTCTime -> String
slugifyTime utc = formatTime defaultTimeLocale "%F_%H-%M-%S" utc



-- Resolutions have to be the same to do a comparison
-- Base and DriverConfigs can be different to compare
pairScreenshots :: [Screenshot] -> [(Screenshot, Screenshot)]
pairScreenshots screenshots = nubBy duplicates (concatMap findComparable screenshots)
      
  where findComparable :: Screenshot -> [(Screenshot, Screenshot)]
        findComparable ss1 = fmap (pairWith ss1) (findAllComparableTo ss1)
        
        pairWith :: Screenshot -> Screenshot -> (Screenshot, Screenshot)
        pairWith s1 s2 = (s1, s2)

        findAllComparableTo :: Screenshot -> [Screenshot]
        findAllComparableTo ss = filter (isComparable ss) screenshots

        isComparable :: Screenshot -> Screenshot -> Bool
        isComparable ss1 ss2 = ss1 /= ss2
                            && (res ss1) == (res ss2)
                            && (page ss1) == (page ss2) 

        duplicates :: (Screenshot, Screenshot) -> (Screenshot, Screenshot) -> Bool
        duplicates pair1 pair2 = pair1 == pair2 
                              || (fst pair1 == snd pair2 
                                 && snd pair1 == fst pair2) 

main :: IO ()
main = do
         date <- getCurrentTime
         allScreenshots <- sequence $ runScreenshots date
         allComparisons <- sequence $ fmap compareScreenshots (pairScreenshots allScreenshots) 
         return ()
    where 
          cfg = selfieConfig

          runScreenshots date = saveScreenshot (saveDir cfg) date 
                             <$> (webDriverConfigs cfg) 
                             <*> (bases cfg) <*> (sitemap cfg) 
                             <*> (resolutions cfg)



saveScreenshot :: String -> UTCTime -> NamedWDConfig -> Base -> Page -> Resolution -> IO Screenshot
saveScreenshot root utc namedCfg bse pge resolut = do
                runSession config $ do
                    openPage pageUrl
                    setWindowSize resolut

                    -- waitWhile 5.0 (waitUntil 4.0 (expect False)) --Let some of the resizing animations take place
                    waitUntil 5.0 (expect False)
                          `onTimeout` return ()
                    ss <- screenshot
                    window <- getCurrentWindow
                    closeWindow window
                    liftIO $ createDirectoryIfMissing True (takeDirectory ssfilepath)
                    liftIO $ L.writeFile ssfilepath ss
                return $ Screenshot { filepath = ssfilepath
                                  , base = bse
                                  , page = pge
                                  , res = resolut
                                  , wdConfigName = configName namedCfg
                                  }
        where pageUrl = toURL bse pge
              ssfilepath = formatPath root bse pge utc resolut (configName namedCfg)
              config = driverConfig namedCfg


 


compareScreenshots :: (Screenshot, Screenshot) -> IO (Either String SSComparison)
compareScreenshots (ss1, ss2) = do
                    img1 <- readImage (filepath ss1) 
                    img2 <- readImage (filepath ss2)
                    imgOrError <- writeImage (diffImages img1 img2)
                    return (prepareResult imgOrError)

    where 
      imgDiffPath = (dropExtension (filepath ss1)) ++ "_vs_" ++ (takeFileName (filepath ss2))

      writeImage :: Either String (Image PixelRGBA8, Float) -> IO (Either String Float)
      writeImage (Right (imgDiff, diffScore)) = do
                                                  writePng imgDiffPath imgDiff
                                                  return (Right diffScore)
      writeImage (Left err) = return (Left err)
      
      prepareResult (Right score) = Right $ SSComparison { branch = page ss1
                                                       , description = describe ss1 
                                                                     ++ " vs " 
                                                                     ++ describe ss2
                                                       , resolution = res ss1
                                                       , screenshot1 = ss1
                                                       , screenshot2 = ss2
                                                       , screenshotDiff = imgDiffPath
                                                       , percentDiff = score
                                                       }
      prepareResult (Left err) = (Left err)





diffImages :: Either String DynamicImage -> Either String DynamicImage 
           -> Either String (Image PixelRGBA8, Float)
diffImages (Right (ImageRGBA8 img1)) (Right (ImageRGBA8 img2)) = Right $ (createImgDiff, calcDiffScore)

  where height = max (imageHeight img1) (imageHeight img2)
        width  = max (imageWidth img1)  (imageWidth img2)
        allPositions = zip [1..width] [1..height] 

        calcDiffScore :: Float
        calcDiffScore = (foldl' diffScore 0 allPositions) / ((fromIntegral (width*height))::Float)


        diffScore acc (x, y) = acc + (diffPixel (getPixel img1 x y) (getPixel img2 x y))

        createImgDiff = generateImage diffXY width height

        diffXY :: Int -> Int -> PixelRGBA8
        diffXY x y = PixelRGBA8 (127::Pixel8) (0::Pixel8) (0::Pixel8) ((alpha x y)::Pixel8)

        alpha x y = gate (diffPixel (getPixel img1 x y) (getPixel img2 x y))

        diffPixel :: PixelRGBA8 -> PixelRGBA8 -> Float
        diffPixel (PixelRGBA8 r1 g1 b1 _) (PixelRGBA8 r2 g2 b2 _) = ((absDiff r1 r2) 
                                                                   + (absDiff g1 g2) 
                                                                   + (absDiff b1 b2)) / ((fromInteger (3*127))::Float)

        absDiff a b = fromIntegral (abs $ ((fromIntegral a)::Int) - ((fromIntegral b)::Int)) :: Float

        getPixel img x y 
               | x < 0 || y < 0 = outofBoundsPixel
               | imageHeight img <= y = outofBoundsPixel
               | imageWidth img <= x = outofBoundsPixel
               | otherwise = pixelAt img x y
        outofBoundsPixel = PixelRGBA8 (127::Pixel8) (0::Pixel8) (0::Pixel8) (127::Pixel8)
        gate i = (round (127 * abs i))::Word8

diffImages _ _ = Left "Images are different formats"



