module HTHttpd.Types where

import System.IO (Handle(..))
import Data.Map as M -- probably meant to import qualified... still bringing in all of Data.Map unqualified but not using it.

-- our first approach is non-idiomatic and non-type-safe
data RequestLine = RequestLine {
   method :: String -- "Stringly" typed. Please no!
  ,uri :: String
  ,version :: String
} deriving (Eq, Show)

-- type synonyms are a false sense of security...
type HTTPHeaders = M.Map String String
type RouteParams = M.Map String String
-- we're definitely never going to accidentally send HTTPHeaders when we mean RouteParams, right?
type Route = String
-- ideally, we'd like to keep these pure and handle IO elsewhere.
type RouteHandler = RouteParams -> HTTPHeaders -> Handle -> IO ()
type RouteMap = [(Route, RouteHandler)] -- not actually a map, as there's nothing gained by that

data RequestMessage = RequestMessage {
   target :: RequestLine
  ,headers :: HTTPHeaders
} deriving (Eq, Show)

standardHeaders :: HTTPHeaders
standardHeaders = M.fromList [
   ("Server", "HTHttpd/0.0.1 (linux)")
  ,("Connection","close")
  ,("Content-Type","text/html; charset=UTF-8")
  ]
