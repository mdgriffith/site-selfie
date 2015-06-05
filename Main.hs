{-# LANGUAGE OverloadedStrings #-}
import Test.WebDriver
import SiteSelfie


myConfig :: WDConfig
myConfig = defaultConfig

main :: IO ()
main = runSession myConfig $ do
  openPage "http://google.com"
  searchInput <- findElem (ByCSS "input[type='text']")
  sendKeys "Hello, World!" searchInput