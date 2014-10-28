{-# LANGUAGE PackageImports #-}
import "rockit" RockIt ( noBuffering, simpleGET, downloadPlaylist, getPlaylist, Result(..) )
import "rockit" RockIt.Template ( parse )
import "base" System.Environment ( getArgs, getProgName )
import "base" System.Exit ( exitFailure )
import "base" Data.List ( isInfixOf )
import "base" System.IO ( hPutStrLn, stderr )

defaultTemplate :: String
defaultTemplate = "%p/%n - %a - %t%e"

main :: IO ()
main = do
    args <- getArgs
    if ["-h"] `isInfixOf` args then
      usage
    else
      parseArgs args

doMagic :: String -> String -> IO ()
doMagic template url = do
    noBuffering
    {-res <- simpleGET "http://www.rockit.it/finebeforeyoucame/album/ormai/18579"-}
    res <- simpleGET url
    case res of
      Left err -> fail err
      Right r  ->
        case getPlaylist r of
          Just playlist -> do
            e <- downloadPlaylist (parse template) playlist
            case e of
              OK       -> return ()
              Error e' -> die e'
          Nothing       -> do
            die "Invalid URL"
  where
  die x = do
    hPutStrLn stderr $ "ERROR: " ++ x
    exitFailure

usage :: IO ()
usage = do
  n <- getProgName
  putStrLn $ "USAGE: " ++ n ++ " [pathTemplate] url"
  putStrLn $ "       " ++ n ++ " -h"

parseArgs :: [String] -> IO ()
parseArgs [url]           = doMagic defaultTemplate url
parseArgs [template, url] = doMagic template url
parseArgs _               = do
    usage
    exitFailure
