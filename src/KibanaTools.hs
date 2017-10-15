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
   }

instance Options.Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "kibana-host" "https://kibana.gocardless.io:9200"
            "Target kibana host"
        <*> simpleOption "query-file" "/tmp/query.json"
            "File containing JSON query"

main :: IO ()
main = runCommand $ \opts args -> do
  runMain opts (head args)
  return ()

runMain opts "query" = do
    queryContents <- B.readFile (optQueryFile opts)
    results <- queryKibanaCommand (optKibanaHost opts) (fromJust.decode $ queryContents :: KibanaQuery)
    C.putStrLn $ encodePretty results

queryKibanaCommand :: KibanaHost -> KibanaQuery -> IO [Value]
queryKibanaCommand host query = do
    response <- postWith opts url (toJSON query)
    paginateQuery (makeScroller host (getId response)) (getHits response)
    where
      opts    = defaults & param "size" .~ ["10000"] & param "scroll" .~ ["1m"]
      url     = host ++ "/_search"
      getId   = (^.. responseBody . key "_scroll_id")
      getHits = (^.. responseBody . key "hits" . key "hits")

      paginateQuery scroller results = do
        hPutStrLn stderr "Paginating..."
        page <- scroller
        case getHits page of
          [Array rs]
            | Vector.null rs -> return results
            | otherwise      -> paginateQuery scroller (results ++ Vector.toList rs)

makeScroller :: KibanaHost -> [Value] -> IO (Response B.ByteString)
makeScroller host [String id] = post url body
    where
      url  = host ++ "/_search/scroll"
      body = object ["scroll" .= ("1m" :: T.Text), "scroll_id" .= id]
