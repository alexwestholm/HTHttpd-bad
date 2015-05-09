module HTHttpd.Routes where

import qualified Data.Map as M

import HTHttpd.Handlers
import HTHttpd.Types

getRoutes :: RouteMap
getRoutes = [
   ("/cool", \p hd h -> send200 h Nothing "<html><body><h1>COOL!!!</h1></body></html>")
  ,("/hello/:name", \p hd h -> case M.lookup ":name" p of
                                 Just hiGuy -> send200 h Nothing ("<html><body>Sup, "++hiGuy++"?</body></html>")
                                 Nothing -> send404 h)
  ,("*",\p hd h -> send200 h Nothing "<html><body><p>Yeah, server works</p></body></html>")
  ]

postRoutes :: RouteMap
postRoutes = []

deleteRoutes :: RouteMap
deleteRoutes = []

putRoutes :: RouteMap
putRoutes = []
