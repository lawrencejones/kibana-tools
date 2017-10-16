{-# LANGUAGE OverloadedStrings #-}

import System.Environment ()
import System.IO (stderr, hPutStrLn)
import Options
import Data.Maybe
import Control.Monad
import Control.Monad.Loops
import Debug.Trace
import qualified Data.Vector as Vector (toList)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C (putStrLn)
import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Lens (key)

type KibanaHost = String
type KibanaQuery = Value

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
    page    <- postWith opts url query
    results <- unfoldM (getResultsM (scroller host (getId page)))

    return $ case getResults page of
      Just rs -> concat (rs : results)
      Nothing -> []

    where
      url     = host ++ "/_search"
      opts    = defaults & param "size" .~ [T.pack (show perPage)] & param "scroll" .~ ["1m"]
      getId   = (^.. responseBody . key "_scroll_id")
      getHits = (^.. responseBody . key "hits" . key "hits")

      getResultsM :: IO (Response B.ByteString) -> IO (Maybe [Value])
      getResultsM = fmap getResults

      getResults :: Response B.ByteString -> Maybe [Value]
      getResults page =
        case unwrapHits (getHits page) of
          [] -> Nothing
          hs -> Just hs

      unwrapHits hs = case hs of [Array hs'] -> Vector.toList hs'

scroller :: KibanaHost -> [Value] -> IO (Response B.ByteString)
scroller host [String sid] = do
    hPutStrLn stderr "Scrolling..."
    post url body
    where
      url  = host ++ "/_search/scroll"
      body = object ["scroll" .= ("1m" :: T.Text), "scroll_id" .= sid]
