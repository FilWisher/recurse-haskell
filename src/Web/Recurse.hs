{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RecordWildCards            #-}

module Web.Recurse where

import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Catch

import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Client.TLS

import GHC.Generics

import           Data.String
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Aeson as JSON

type Token = BS.ByteString
type BatchID = Int
type ProfileID = Int
type LocationID = Int

data RecurseConfig = RecurseConfig
    { configToken :: Token
    , configMgr   :: Manager
    }

-- | Monad for running requests to the Recurse Center API.
newtype RecurseM a = RecurseM
    { unRecurseM :: ReaderT RecurseConfig IO a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader RecurseConfig
        , MonadIO
        , MonadThrow
        )

token :: IsString s => s
token = "753d7e68ebc70c80816c899173e0c0e38c0fbff6bbf45739a9b0e86f619de3b6"

-- | The V1 API URL
baseurl :: IsString s => s
baseurl = "https://www.recurse.com/api/v1"

-- | Run the RecurseM action with the given API token and HTTP manager.
runRecurseWithManager :: Token -> Manager -> RecurseM a -> IO a
runRecurseWithManager tk mgr m = runReaderT (unRecurseM m) (RecurseConfig tk mgr)

-- | Run the RecurseM action with the given API token. A new HTTP manager is
--   created for each call to `runRecurse`.
--
--  Example:
--    profiles <- runRecurse token $ do
--        batch <- head <$> getBatches
--        getProfilesByBatch (batchID batch) $ defaultProfileQuery
--            { query = Just "haskell"
--            }
--    forM_ profiles print
--
runRecurse :: Token -> RecurseM a -> IO a
runRecurse tk m = newManager tlsManagerSettings >>= flip (runRecurseWithManager tk) m

-- | An exception representing an error from the HTTP layer.
newtype HTTPError = HTTPError Status deriving (Show)
instance Exception HTTPError

-- | An exception representing an error during JSON decoding.
newtype DecodeError = DecodeError String deriving (Show)
instance Exception DecodeError

-- | Members of the ToQueryString class can be serialized to query string
--   formatted strict ByteString (omitting the initial '&').
class ToQueryString a where
    toQueryString :: a -> BS.ByteString

instance ToQueryString BS.ByteString where
    toQueryString = id

-- | Make a request to the Recurse Center API with a query string. Throws an
--   exception with `throwM` on non-200 responses and when the response body
--   cannot be decoded to JSON.
requestRecurseWithQuery :: (ToQueryString a, JSON.FromJSON b) => String -> a -> RecurseM b
requestRecurseWithQuery path query = do
    mgr <- configMgr <$> ask
    req <- parseRequest (baseurl <> path)
    liftIO $ print req
        { requestHeaders = ("Authorization", "Bearer " <> token) : requestHeaders req
        , queryString = toQueryString query
        }
    resp <- liftIO $ flip httpLbs mgr req 
        { requestHeaders = ("Authorization", "Bearer " <> token) : requestHeaders req
        , queryString = toQueryString query
        }
    if responseStatus resp /= ok200
        then throwM (HTTPError $ responseStatus resp)
        else case JSON.eitherDecode (responseBody resp) of
            Left err -> throwM (DecodeError err)
            Right v -> return v

-- | Make a request to the Recurse Center API. Throws an exception with
--   `throwM` on non-200 responses and when the response body cannot be decoded
--   to JSON.
requestRecurse :: JSON.FromJSON a => String -> RecurseM a
requestRecurse path = requestRecurseWithQuery path ("" :: BS.ByteString)

-- | A Recurse Center Batch (i.e a class or a cohort).
data Batch = Batch
    { batchId        :: BatchID
    , batchName      :: Text
    , batchStartDate :: Text
    , batchEndDate   :: Text
    }
    deriving (Show, Generic)

instance JSON.FromJSON Batch where
    parseJSON = JSON.genericParseJSON JSON.defaultOptions
        { JSON.fieldLabelModifier = JSON.camelTo2 '_' . drop 5
        }

-- | Return all batches.
getBatches :: RecurseM [Batch]
getBatches = requestRecurse "/batches"

-- | Return a specific batch by its BatchID.
getBatch :: BatchID -> RecurseM (Maybe Batch)
getBatch batchId = requestRecurse $ "/batches/" <> show batchId

-- | A Recurse Center profile. Represents a profile registered in the RC
--   directory.
data Profile = Profile
    { profileID          :: ProfileID
    , profileFirstName   :: Text
    , profileLastName    :: Text
    , profileEmail       :: Text
    , profileGithub      :: Maybe Text
    , profileTwitter     :: Maybe Text
    , profileBioHl       :: Maybe Text
    , profileBeforeRcHl  :: Maybe Text
    , profileDuringRcHl  :: Maybe Text
    , profileInterestsHl :: Maybe Text
    }
    deriving (Show, Generic)

instance JSON.FromJSON Profile where
    parseJSON = JSON.genericParseJSON JSON.defaultOptions
        { JSON.fieldLabelModifier = JSON.camelTo2 '_' . drop 7
        }

-- | The role for a specific profile.
data Role = Recurser | Resident | Facilitator | Faculty
    deriving (Show)

-- | The possible query parameters when requesting profiles.
data ProfileQuery = ProfileQuery
    { query      :: Maybe BS.ByteString
    , batchID    :: Maybe BatchID
    , locationID :: Maybe LocationID
    , role       :: Maybe Role
    , limit      :: Maybe Int
    , offset     :: Maybe Int
    }

instance ToQueryString ProfileQuery where
    toQueryString (ProfileQuery{..}) =
        BS.intercalate "&" $ filter (not . BS.null)
            [ maybe "" ("query="<>)       query
            , maybe "" ("batch_id="<>)    $ fmap showBS batchID
            , maybe "" ("location_id="<>) $ fmap showBS locationID
            , maybe "" ("role="<>)        $ fmap showBS role
            , maybe "" ("limit="<>)       $ fmap showBS limit
            , maybe "" ("offset="<>)      $ fmap showBS offset
            ]
        where
            showBS :: Show a => a -> BS.ByteString
            showBS = B8.pack . show 

-- | An empty query
defaultProfileQuery :: ProfileQuery
defaultProfileQuery = ProfileQuery Nothing Nothing Nothing Nothing Nothing Nothing

-- | Return every `Profile` that matches the query.
getProfiles :: ProfileQuery -> RecurseM [Profile]
getProfiles query = requestRecurseWithQuery "/profiles" query

-- | Return every `Profile` enrolled on this batch.
getProfilesByBatch :: BatchID -> ProfileQuery -> RecurseM [Profile]
getProfilesByBatch bid query =
    getProfiles query
        { batchID = Just bid
        }
