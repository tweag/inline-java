{-# LANGUAGE ViewPatterns #-}

import Foreign.JNI (withJVM)
import Directory.Server (server)
import Directory.Server.Monad
import System.Directory
import System.Environment
import System.IO


main :: IO ()
main = do
  port <- getPortFromCommandLine
  cwd <- getCurrentDirectory
  withJVM [] $ runLServer
    Environment {envRootDirectory = cwd}
    (server port)

getPortFromCommandLine :: IO Int
getPortFromCommandLine = do
    args <- getArgs
    case args of
      [reads -> ((port, _) : _)] -> return port
      _ -> do
        hPutStrLn stderr "USAGE: server <port>"
        error "Couldn't read port number from command line."
