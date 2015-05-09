module HTHttpd.Parser where

import qualified Data.List.Split as SP
import qualified Data.Map as M

import HTHttpd.Types

parseRequestLine :: String -> Maybe RequestLine
parseRequestLine raw | length components < 3 = Nothing
                 -- Not a good way to encode business logic
                 | not (method `elem` ["GET","PUT","POST","DELETE"]) = Nothing
                 -- Ditto
                 | not (version `elem` ["HTTP/1.1","HTTP/1.0"]) = Nothing
                 | otherwise = Just $ RequestLine method uri version
                 -- There's a better way to handle string splitting...
                 where (method, uri, version) = (components !! 0, components !! 1, components !! 2)
                       components = words raw

parseHeaders :: [String] -> HTTPHeaders
parseHeaders raw = foldr spliceHeader M.empty raw
  where spliceHeader l m | length parts > 1 = M.insert headerName headerVal m
                         | otherwise = m
                         where parts = SP.splitOn ":" l
                               headerName = parts !! 0
                               headerVal = parts !! 1

parseMessage :: [String] -> Maybe RequestMessage
parseMessage raw | length strippedRaw < 1 = Nothing
                 | target /= Nothing =
                   case target of
                     Just req -> Just $ RequestMessage req $ parseHeaders headerLines
                     _ -> Nothing
                 | otherwise = Nothing
                 where strippedRaw = dropWhile (== "") raw -- per RFC 2616, leading whitespace is stripped
                       ([reqLine], headerLines) = splitAt 1 strippedRaw
                       target = parseRequestLine reqLine

-- this doesn't look exactly like parseHeaders, does it? DRY it up.
parseGetParams :: String -> RouteParams
parseGetParams raw = foldr addGetParam M.empty $ SP.splitOn "&" raw
  where addGetParam p m | length parts > 1 = M.insert paramName paramVal m
                        | otherwise = m
                        where parts = SP.splitOn "=" p
                              paramName = parts !! 0
                              paramVal = parts !! 1
