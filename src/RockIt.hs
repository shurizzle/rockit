{-# LANGUAGE PackageImports, QuasiQuotes, OverloadedStrings, DeriveDataTypeable #-}

module RockIt
  (
    Result(..)
  , simpleGET
  , downloadPlaylist
  , downloadTrack
  , noBuffering
  , Playlist(..)
  , getPlaylist
  , TrackInfo(..)
  , parseTrackInfo
  ) where

import "base" Data.List ( isInfixOf )
import "base" System.IO ( hSetBuffering, BufferMode(NoBuffering), stdout, openBinaryFile, hClose, IOMode(..) )
import "base" Data.Char ( intToDigit )
import "terminal-progress-bar" System.ProgressBar ( percentage, noLabel )
import "xml-types" Data.XML.Types ( Name(..) )
import "xml-conduit" Text.XML ( Node(..), Element(..) )
import "xml-conduit" Text.XML.Cursor ( fromDocument )
import "xml-conduit" Text.XML.Cursor.Generic ( Cursor(..) )
import "html-conduit" Text.HTML.DOM ( parseLBS )
import "dom-selector" Text.XML.Scraping ( innerText )
import "dom-selector" Text.XML.Selector.TH
import "text" Data.Text as T ( pack, unpack )
import "text" Data.Text.Lazy as TL ( unpack )
import "bytestring" Data.ByteString ( ByteString )
import "bytestring" Data.ByteString.Char8 as B ( pack, unpack )
import "bytestring" Data.ByteString.Lazy.Char8 as LC8 ( pack )
import "bytestring" Data.ByteString.Lazy as BS ( hPut )
import "containers" Data.Map as M ( lookup )
import "json" Text.JSON.Generic ( Data, Typeable, decodeJSON )
import "bytestring-progress" System.ProgressBar.ByteString ( mkByteStringProgressBar )
import "network-uri" Network.URI ( parseURI )
import "HTTP" Network.HTTP ( simpleHTTP, Request(..), Response(..), HStream(..) )
import "HTTP" Network.HTTP.Base ( urlEncodeVars, mkRequest, RequestMethod(..) )
import "HTTP" Network.HTTP.Headers ( findHeader, HeaderName(..), replaceHeader )
import "HTTP" Network.BufferType
import "cryptohash" Crypto.Hash ( Digest, MD5(..), hash, digestToHexByteString )
import "filepath" System.FilePath ( takeExtension )
import "system-filepath" Filesystem.Path.CurrentOS ( dirname, fromText, encode )
import "directory" System.Directory ( createDirectoryIfMissing )
import RockIt.Template ( Format, render )

data Result = OK
            | Error String
  deriving (Show)

payload :: String
payload = "-celhailaragazza"

md5 :: ByteString -> Digest MD5
md5 = hash

md5s :: ByteString -> ByteString
md5s = digestToHexByteString . md5

crPutStr :: String -> IO ()
crPutStr str = putStr $ '\r' : str

createDirectory :: FilePath -> IO ()
createDirectory =
    createDirectoryIfMissing True . B.unpack . encode . dirname . fromText . T.pack

download :: String -> FilePath -> IO Result
download uri path = do
    createDirectory path
    fhndl <- openBinaryFile path WriteMode
    http <- simpleHTTP $ request uri
    case http of
      Left  x    -> return . Error $ "Couldn't download file: " ++ show x
      Right resp -> do
        let Just size = read `fmap` findHeader HdrContentLength resp
        mkByteStringProgressBar (rspBody resp) crPutStr 80 size
                                  noLabel percentage
            >>= BS.hPut fhndl
        putStrLn ""
        hClose fhndl
        return OK

downloadPlaylist :: [Format] -> Playlist -> IO Result
downloadPlaylist format playlist =
    downloadPlaylist' format ids' (name playlist) size
  where
  size = length $ ids playlist
  indexes = take size $ iterate (+ 1) 1
  ids' = zipWith (,) (ids playlist) indexes

downloadPlaylist' :: [Format] -> [(Int, Int)] -> String -> Int -> IO Result
downloadPlaylist' _ [] _ _ = return OK
downloadPlaylist' format ((id', i):xs) playlistName size = do
    dl <- downloadTrack format playlistName size id' i
    case dl of
      OK  -> downloadPlaylist' format xs playlistName size
      res -> return res

downloadTrack :: [Format] -> String -> Int -> Int -> Int -> IO Result
downloadTrack format playlistName playlistSize trackID position = do
    t <- getTrackInfos trackID
    case t of
      Left err -> return $ Error err
      Right t' -> do
        putStrLn $ "Downloading [" ++ progress ++ "] " ++ path t'
        download (url t') (path t')
  where
  progress = show position ++ "/" ++ show playlistSize
  ext t  = takeExtension (url t)
  number = formatNumber position playlistSize
  path t = render format playlistName (author t) (title t) (ext t) position number

formatNumber :: Int -> Int -> String
formatNumber curr tot = pad ++ c
  where
  c = show curr
  l = maximum [2, length $ show tot]
  pad = take (l - (length c)) $ repeat '0'

noBuffering :: IO ()
noBuffering = hSetBuffering stdout NoBuffering

data Playlist = Playlist { ids :: [Int],
                           name :: String }
  deriving (Eq, Show)

toInt :: String -> Int
toInt = read

getAttribute :: String -> Element -> Maybe String
getAttribute name_ (Element _ attrs _) = M.lookup name' attrs >>= Just . T.unpack
  where
  name' = Name { nameLocalName = (T.pack name_),
                 nameNamespace = Nothing,
                 namePrefix    = Nothing }

getIDs :: Cursor Node -> [Int]
getIDs = foldl combo [] . queryT [jq| li.item li.play a |]
  where
  toElement_ (NodeElement e) = Just e
  toElement_ _ = Nothing
  toElement = toElement_ . node

  mapper x = toElement x >>= getAttribute "rel"
  mapFilter acc Nothing  = acc
  mapFilter acc (Just s) = acc ++ [toInt s]
  combo acc = mapFilter acc . mapper

getTitle :: Cursor Node -> String
getTitle = TL.unpack . innerText . head . queryT [jq| a.nome-album |]

getPlaylist :: String -> Maybe Playlist
getPlaylist str =
    if length ids' > 0 then
      Just Playlist { ids  = ids',
                      name = name' }
    else
      Nothing
  where
  doc = fromDocument $ parseLBS $ LC8.pack str
  ids' = getIDs doc
  name' = getTitle doc

data TrackInfo = TrackInfo { url    :: String,
                             title  :: String,
                             author :: String,
                             album  :: String }
  deriving (Eq, Show, Data, Typeable)

parseTrackInfo :: String -> TrackInfo
parseTrackInfo = decodeJSON

getRequest :: (BufferType ty) => String -> Request ty
getRequest uri =
    case parseURI uri of
      Nothing -> error $ "getRequest: Not a valid URL - " ++ uri
      Just u  -> mkRequest GET u

setRequestBody :: (BufferType ty) => Request ty -> (String, ty) -> Request ty
setRequestBody req (typ, body) = req' { rqBody = body }
  where
  req' = replaceHeader HdrContentType typ .
         replaceHeader HdrContentLength (show $ length $ (buf_toStr bufferOps) body) $
         req

postRequestWithBody :: (BufferType ty) => String -> String -> ty -> Request ty
postRequestWithBody uri typ body =
    case parseURI uri of
      Nothing -> error $ "postRequestWithBody: Not a valid URL - " ++ uri
      Just u  -> setRequestBody (mkRequest POST u) (typ, body)

simpleMkPOST :: (BufferType ty) => String -> [(String, String)] -> Request ty
simpleMkPOST uri params = postRequestWithBody uri typ body
  where
  typ = "application/x-www-form-urlencoded"
  body = buf_fromStr bufferOps $ urlEncodeVars params

request :: (BufferType ty) => String -> Request ty
request uri = setReferer $
    if "ww2.rockit" `isInfixOf` uri then
      simpleMkPOST uri [("rockitID", rockitID)]
    else
      getRequest uri
  where
  rockitID = B.unpack $ md5s $ B.pack $ uri ++ payload
  setReferer = replaceHeader HdrReferer "http://www.rockit.it/web/js/playswf/new1027448.swf"

get :: (HStream ty) => Request ty -> IO (Either String ty)
get req = do
    http <- simpleHTTP req
    case http of
      Left e    -> return . Left $ show e
      Right res ->
        case rspCode res of
          (2,0,0) -> return . Right $ rspBody res
          _       -> return . Left $ httpError res
  where
  showRspCode (a,b,c) = map intToDigit [a,b,c]
  httpError res = showRspCode (rspCode res) ++ " " ++ rspReason res

simplePOST :: (HStream ty) => String -> [(String, String)] -> IO (Either String ty)
simplePOST = (get .) . simpleMkPOST

simpleGET :: (HStream ty) => String -> IO (Either String ty)
simpleGET = get . getRequest

getTrackInfos :: Int -> IO (Either String TrackInfo)
getTrackInfos id' = do
    res <- simplePOST "http://www.rockit.it/web/include/ajax.play.php" [("id", show id'), ("0k", "ok")]
    case res of
      Left x    -> return $ Left x
      Right rsp -> return . Right $ parseTrackInfo rsp
