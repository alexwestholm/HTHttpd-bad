module Main where

import Control.Exception (finally)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Network (withSocketsDo, listenOn, PortID (PortNumber), accept)
import System.IO (Handle, hGetLine, hSetBuffering, hClose, BufferMode (LineBuffering))

import HTHttpd.Parser
import HTHttpd.Router
import HTHttpd.Handlers

readMessage :: Handle -> IO [String]
readMessage h = readMessage' h []

readMessage' :: Handle -> [String] -> IO [String]
readMessage' h l = do
  raw <- hGetLine h
  let nput = takeWhile (/='\r') raw
  if nput == ""
  then if length l > 0 then return l else readMessage' h l
  else readMessage' h $ l ++ [nput]

processRequest :: Handle -> IO ()
processRequest h = do
  fullReq <- readMessage h
  let parsed = parseMessage fullReq
  case parsed of
    Just msg -> dispatchRequest msg h
    _ -> send400 h

main :: IO ()
main = withSocketsDo $ do -- for cross platform compatibility
  sckt <- listenOn $ PortNumber 31337
  forever $ do
    (hndl, host, port) <- accept sckt
    forkIO $ do
      hSetBuffering hndl LineBuffering
      finally (processRequest hndl)
              (hClose hndl)
