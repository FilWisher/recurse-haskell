# recurse-haskell

Bindings to the Recurse Center API in Haskell

```hs
import Web.Recurse
import qualified Data.ByteString as BS

token :: BS.ByteString
token = "..."

main :: IO ()
main = do
    profiles <- runRecurse token $ do
        batch <- head <$> getBatches
        getProfilesByBatch (batchID batch) $ defaultProfileQuery
            { query = Just "haskell"
            }
    forM_ profiles print
```
