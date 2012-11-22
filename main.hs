module Main
( main
) where

import RequestHandler
import Handlers

main = handleRequest "/~tniechci" handlers
