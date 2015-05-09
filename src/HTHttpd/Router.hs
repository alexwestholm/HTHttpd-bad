module HTHttpd.Router where

import Data.List (isInfixOf)
import System.IO (Handle)
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import qualified Data.Map as M
-- do we need all of Data.List.Split, or just splitOn? No need for whole module.
import qualified Data.List.Split as SP

import HTHttpd.Types
import HTHttpd.Parser
import HTHttpd.Handlers
import HTHttpd.Routes

-- FFS, move guards below argument list!
matchRoute :: String -> Route -> Maybe RouteParams
matchRoute path route | route == "*" = Just $ M.fromList []
                      | length pathSects /= length routeSects = Nothing -- route doesn't match if different number of path segments
                      | otherwise = case componentMatch of
                        Just routeParams -> Just $ M.union getParams routeParams
                        _ -> Nothing
                      where urlParts = SP.splitOn "?" path
                            pathSects = SP.splitOn "/" (urlParts !! 0)
                            routeSects = SP.splitOn "/" route
                            componentMatch = pathParams pathSects routeSects $ M.fromList []
                            getParams = if length urlParts > 1 then parseGetParams (urlParts !! 1) else M.empty
                            -- this should return Maybe to indicate bad matches
                            pathParams [] _ m = Just m -- return accumulator
                            pathParams (p:ps) (r:rs) m | p /= r && (not (isInfixOf ":" r)) = Nothing
                                                       | p /= r = pathParams ps rs $ M.insert r p m
                                                       | otherwise = pathParams ps rs m

-- iterate over RouteMap keys looking for matches, holding onto RouteParams if match occurs
-- then dispatching to the key in the RouteMap
-- should failure handler be a param? Probably... more modular
-- failure handler should also be at beginning of param list for currying
dispatchRouteMap :: RequestMessage -> RouteMap -> Handle -> IO ()
dispatchRouteMap req [] h = send404 h
dispatchRouteMap req ((rt, handler):rts) h =
  case matchRoute (uri $ target req) rt of 
    Nothing -> dispatchRouteMap req rts h
    Just params -> handler params (headers req) h

dispatchGetRoute :: RequestMessage -> RouteMap -> Handle -> IO ()
dispatchGetRoute req rts h = do
  homeDir <- getEnv "HOME" -- not very x-platform!
  let path = pathPart (uri $ target req)
      docRoot = homeDir ++ "/web"
      filePath = docRoot ++ path
  filePresent <- doesFileExist filePath
  if filePresent then sendFile filePath h else dispatchRouteMap req rts h
  where pathPart = dropWhile (/= '/')

dispatchRequest :: RequestMessage -> Handle -> IO ()
dispatchRequest req h = do
  -- it's possible for us to enable a new method in the parse function, but fail to provide a handler... bad!
  case method (target req) of
    "GET" -> dispatchGetRoute req getRoutes h
    "POST" -> dispatchRouteMap req postRoutes h
    "PUT" -> dispatchRouteMap req putRoutes h
    "DELETE" -> dispatchRouteMap req deleteRoutes h
