module Main where

import Happstack.Server

main = simpleHTTP nullConf $ serveDirectory EnableBrowsing [] "."
