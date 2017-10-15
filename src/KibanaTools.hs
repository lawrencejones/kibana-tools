{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.IO
import Options
import Data.Maybe
import qualified Data.Vector as Vector (null,(++),toList)
import GHC.Exts (toList)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C (putStrLn)
import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Lens (key, nth)
import Data.Aeson.Types
import Data.Map
import qualified Data.Map as Map

type KibanaHost = String
type KibanaQuery = Value
type ScrollIdentifier = String

data MainOptions = MainOptions
   { optKibanaHost :: KibanaHost
   , optQueryFile :: String
   , optPerPage :: Int
   }

instance Options.Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "kibana-host" "https://kibana.gocardless.io:9200"
            "Target kibana host"
        <*> simpleOption "query-file" "/tmp/query.json"
            "File containing JSON query"
        <*> simpleOption "per-page" 1000 "Paginate by N results"

main :: IO ()
main = runCommand $ \opts args -> do
  runMain opts (head args)
  return ()

runMain opts "query" = do
    queryContents <- B.readFile (optQueryFile opts)
    let query = fromJust.decode $ queryContents :: KibanaQuery
    results <- queryKibanaCommand (optKibanaHost opts) (optPerPage opts) query
    C.putStrLn $ encodePretty results

queryKibanaCommand :: KibanaHost -> Int -> KibanaQuery -> IO [Value]
queryKibanaCommand host perPage query = do
    initialPage <- postWith opts url (toJSON query)
    paginate (makeScroller host (getId initialPage)) [] initialPage

    where
      opts    = defaults & param "size" .~ [T.pack (show perPage)] & param "scroll" .~ ["1m"]
      url     = host ++ "/_search"
      getId   = (^.. responseBody . key "_scroll_id")
      getHits = (^.. responseBody . key "hits" . key "hits")

      paginate scroller results page = do
        hPutStrLn stderr "Paginating..."
        case getHits page of
          [Array rs]
            | Vector.null rs -> return results
            | otherwise      -> scroller >>= paginate scroller (results ++ Vector.toList rs)

makeScroller :: KibanaHost -> [Value] -> IO (Response B.ByteString)
makeScroller host [String id] = post url body
    where
      url  = host ++ "/_search/scroll"
      body = object ["scroll" .= ("1m" :: T.Text), "scroll_id" .= id]
