module HTHttpd.Handlers where

import Data.Time (getCurrentTime)
-- why import Handle value constructors when we never construct a value?
import System.IO (Handle (..), hPutStr, hPutStrLn, hFlush)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

import HTHttpd.Types

-- this whole family of functions needs to be DRYed up.
send404 :: Handle -> IO ()
send404 h = sendResponse "HTTP/1.1 404 Not Found" Nothing errBody h
  where errBody = "<!DOCTYPE html>" ++
                  "<meta charset=\"UTF-8\">" ++
                  "<head><title>404 Not Found</title></head>" ++
                  "<body><h1>Not Found</h1>" ++
                  "<p>But we didn't look too hard...</p>" ++
                  "</body></html>"

send500 :: Handle -> IO ()
send500 h = sendResponse "HTTP/1.1 500 Internal Server Error" Nothing errBody h
  where errBody = "<!DOCTYPE html>" ++
                  "<meta charset=\"UTF-8\">" ++
                  "<head><title>500 Internal Server Error</title></head>" ++
                  "<body><h1>Internal Server Error</h1>" ++
                  "<p>Our dog ate our homework...</p>" ++
                  "</body></html>"

send400 :: Handle -> IO ()
send400 h = sendResponse "HTTP/1.1 400 Bad Request" Nothing errBody h
  where errBody = "<!DOCTYPE html>" ++
                  "<meta charset=\"UTF-8\">" ++
                  "<head><title>400 Bad Request</title></head>" ++
                  "<body><h1>Bad Request</h1>" ++
                  "<p>What exactly are you trying to do, friend?</p>" ++
                  "</body></html>"

-- poor parameter placement makes this way more verbose than it should be...
send200 :: Handle -> Maybe HTTPHeaders -> String -> IO ()
send200 h hdrs bd = sendResponse "HTTP/1.1 200 Ok" hdrs bd h

sendFile :: String -> Handle -> IO ()
sendFile path h = do
  body <- readFile path
  send200 h Nothing body

sendHeaders :: HTTPHeaders -> Handle -> IO ()
sendHeaders hdrs h = do
  mapM_ printHeader (M.toList hdrs)
  hPutStr h "\n" -- extra newline to separate headers from body
  where printHeader (name,val) = hPutStrLn h $ name ++ ": " ++ val

-- why are there params of type String... NOOOOOO!!!!!
sendResponse :: String -> Maybe HTTPHeaders -> String -> Handle -> IO ()
sendResponse status headers body h = do
  sentAt <- getCurrentTime
  let freshHeaders = M.fromList [("Content-Length", show $ BS.length bsBody),("Date", show sentAt)]
      updatedHeaders = M.union standardHeaders $
        case headers of
          Just hdrs -> M.union hdrs freshHeaders
          _ -> freshHeaders
  putStrLn $ status ++ " (" ++ show sentAt ++ ")"
  hPutStrLn h status
  sendHeaders updatedHeaders h
  hPutStr h body
  hFlush h -- ensure that anything buffered gets written...
  where bsBody = BS.pack body

