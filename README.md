## Website Selfie

Takes screenshots of your website and calculates an image diff for two shots that are comparable.  These screendiffs are then ordered by most changed and listed in a static html file for review.

This allows you to quickly see the differences in how your website looks locally vs live, or between browsers.

This code was written in a weekend and hasn't been fully transformed into a usable library.  At the moment it uses http://mechanical-elephant.com as an example.

Index.html in the example folder is comparing a dev version of mechanical-elephant and the live version.  I moved the elephant slightly so that a change can be see.


# Installation
Clone this repository locally.

Install by running the following in the site-selfie directory

    cabal update
    cabal install --depenencies-only

Begin the Selenium Server and possibly the chromedriver.  Then run:

    cabal run

You'll have to modify Main.hs if you want to take screenshots of something other than http://mechanical-elephant.com.  Specifically, this section:

```hs

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
                               ]
         }
```


# Selenium Server

Install [Selenium Server](http://docs.seleniumhq.org/download/).  You can then start the selenium server with the following command:
    
    java -jar selenium-server-standalone-*.jar


# Notes

The [chromedriver](https://code.google.com/p/selenium/wiki/ChromeDriver),  which has to be installed and run separately if you want to test in Chrome, currently takes a screenshot of only the viewable area as opposed to the entire web page.  This makes the screenshots difficult to compare against something like firefox.  A [Python workaround](https://snipt.net/restrada/python-selenium-workaround-for-full-page-screenshot-using-chromedriver-2x/) is available that could be translated over.

