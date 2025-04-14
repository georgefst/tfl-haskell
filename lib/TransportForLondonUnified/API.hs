{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module TransportForLondonUnified.API
  ( -- * Client and Server
    Config(..)
  , TransportForLondonUnifiedBackend(..)
  , createTransportForLondonUnifiedClient
  , runTransportForLondonUnifiedServer
  , runTransportForLondonUnifiedMiddlewareServer
  , runTransportForLondonUnifiedClient
  , runTransportForLondonUnifiedClientWithManager
  , callTransportForLondonUnified
  , TransportForLondonUnifiedClient
  , TransportForLondonUnifiedClientError(..)
  -- ** Servant
  , TransportForLondonUnifiedAPI
  -- ** Plain WAI Application
  , serverWaiApplicationTransportForLondonUnified
  -- ** Authentication
  , TransportForLondonUnifiedAuth(..)
  , clientAuth
  , Protected
  ) where

import           TransportForLondonUnified.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import qualified Data.Aeson                         as Aeson
import           Data.ByteString                    (ByteString)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as BSL
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware, Request, requestHeaders)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serveWithContextT, throwError)
import           Servant.API                        hiding (addHeader)
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.API.Experimental.Auth      (AuthProtect)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context ((:.), EmptyContext))
import           Servant.Server.Experimental.Auth   (AuthHandler, AuthServerData, mkAuthHandler)
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList

newtype JSONQueryParam a = JSONQueryParam
  { fromJsonQueryParam :: a
  } deriving (Functor, Foldable, Traversable)

instance Aeson.ToJSON a => ToHttpApiData (JSONQueryParam a) where
  toQueryParam = T.decodeUtf8 . BSL.toStrict . Aeson.encode . fromJsonQueryParam

instance Aeson.FromJSON a => FromHttpApiData (JSONQueryParam a) where
  parseQueryParam = either (Left . T.pack) (Right . JSONQueryParam) . Aeson.eitherDecodeStrict . T.encodeUtf8


-- | Servant type-level API, generated from the OpenAPI spec for TransportForLondonUnified.
type TransportForLondonUnifiedAPI
    =    "AccidentStats" :> Capture "year" Int :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesAccidentStatsAccidentDetail] -- 'accidentStatsGet' route
    :<|> "AirQuality" :> Verb 'GET 200 '[JSON] Value -- 'airQualityGet' route
    :<|> "BikePoint" :> Capture "id" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesPlace -- 'bikePointGet' route
    :<|> "BikePoint" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'bikePointGetAll' route
    :<|> "BikePoint" :> "Search" :> QueryParam "query" Text :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'bikePointSearch' route
    :<|> "Cabwise" :> "search" :> QueryParam "lat" Double :> QueryParam "lon" Double :> QueryParam "optype" Text :> QueryParam "wc" Text :> QueryParam "radius" Double :> QueryParam "name" Text :> QueryParam "maxResults" Int :> QueryParam "legacyFormat" Bool :> QueryParam "forceXml" Bool :> QueryParam "twentyFourSevenOnly" Bool :> Verb 'GET 200 '[JSON] Value -- 'cabwiseGet' route
    :<|> "Journey" :> "JourneyResults" :> Capture "from" Text :> "to" :> Capture "to" Text :> QueryParam "via" Text :> QueryParam "nationalSearch" Bool :> QueryParam "date" Text :> QueryParam "time" Text :> QueryParam "timeIs" Text :> QueryParam "journeyPreference" Text :> QueryParam "mode" (QueryList 'MultiParamArray (Text)) :> QueryParam "accessibilityPreference" (QueryList 'MultiParamArray (Text)) :> QueryParam "fromName" Text :> QueryParam "toName" Text :> QueryParam "viaName" Text :> QueryParam "maxTransferMinutes" Text :> QueryParam "maxWalkingMinutes" Text :> QueryParam "walkingSpeed" Text :> QueryParam "cyclePreference" Text :> QueryParam "adjustment" Text :> QueryParam "bikeProficiency" (QueryList 'MultiParamArray (Text)) :> QueryParam "alternativeCycle" Bool :> QueryParam "alternativeWalking" Bool :> QueryParam "applyHtmlMarkup" Bool :> QueryParam "useMultiModalCall" Bool :> QueryParam "walkingOptimization" Bool :> QueryParam "taxiOnlyTrip" Bool :> QueryParam "routeBetweenEntrances" Bool :> QueryParam "useRealTimeLiveArrivals" Bool :> QueryParam "calcOneDirection" Bool :> QueryParam "includeAlternativeRoutes" Bool :> QueryParam "overrideMultiModalScenario" Int :> QueryParam "combineTransferLegs" Bool :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesJourneyPlannerItineraryResult -- 'journeyJourneyResults' route
    :<|> "Journey" :> "Meta" :> "Modes" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesMode] -- 'journeyMeta' route
    :<|> "Line" :> Capture "ids" [Text] :> "Arrivals" :> Capture "stopPointId" Text :> QueryParam "direction" Text :> QueryParam "destinationStationId" Text :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPrediction] -- 'lineArrivals' route
    :<|> "Line" :> Capture "ids" [Text] :> "Disruption" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesDisruption] -- 'lineDisruption' route
    :<|> "Line" :> "Mode" :> Capture "modes" [Text] :> "Disruption" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesDisruption] -- 'lineDisruptionByMode' route
    :<|> "Line" :> Capture "ids" [Text] :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineGet' route
    :<|> "Line" :> "Mode" :> Capture "modes" [Text] :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineGetByMode' route
    :<|> "Line" :> Capture "ids" [Text] :> "Route" :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineLineRoutesByIds' route
    :<|> "Line" :> "Meta" :> "DisruptionCategories" :> Verb 'GET 200 '[JSON] [Text] -- 'lineMetaDisruptionCategories' route
    :<|> "Line" :> "Meta" :> "Modes" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesMode] -- 'lineMetaModes' route
    :<|> "Line" :> "Meta" :> "ServiceTypes" :> Verb 'GET 200 '[JSON] [Text] -- 'lineMetaServiceTypes' route
    :<|> "Line" :> "Meta" :> "Severity" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStatusSeverity] -- 'lineMetaSeverity' route
    :<|> "Line" :> "Route" :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineRoute' route
    :<|> "Line" :> "Mode" :> Capture "modes" [Text] :> "Route" :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineRouteByMode' route
    :<|> "Line" :> Capture "id" Text :> "Route" :> "Sequence" :> Capture "direction" Text :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> QueryParam "excludeCrowding" Bool :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesRouteSequence -- 'lineRouteSequence' route
    :<|> "Line" :> "Search" :> Capture "query" Text :> QueryParam "modes" (QueryList 'MultiParamArray (Text)) :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesRouteSearchResponse -- 'lineSearch' route
    :<|> "Line" :> Capture "ids" [Text] :> "Status" :> Capture "startDate" Text :> "to" :> Capture "endDate" Text :> QueryParam "detail" Bool :> QueryParam "dateRange.startDate" UTCTime :> QueryParam "dateRange.endDate" UTCTime :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineStatus' route
    :<|> "Line" :> Capture "ids" [Text] :> "Status" :> QueryParam "detail" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineStatusByIds' route
    :<|> "Line" :> "Mode" :> Capture "modes" [Text] :> "Status" :> QueryParam "detail" Bool :> QueryParam "severityLevel" Text :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineStatusByMode' route
    :<|> "Line" :> "Status" :> Capture "severity" Int :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineStatusBySeverity' route
    :<|> "Line" :> Capture "id" Text :> "StopPoints" :> QueryParam "tflOperatedNationalRailStationsOnly" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'lineStopPoints' route
    :<|> "Line" :> Capture "id" Text :> "Timetable" :> Capture "fromStopPointId" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesTimetableResponse -- 'lineTimetable' route
    :<|> "Line" :> Capture "id" Text :> "Timetable" :> Capture "fromStopPointId" Text :> "to" :> Capture "toStopPointId" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesTimetableResponse -- 'lineTimetableTo' route
    :<|> "Mode" :> Capture "mode" Text :> "Arrivals" :> QueryParam "count" Int :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPrediction] -- 'modeArrivals' route
    :<|> "Mode" :> "ActiveServiceTypes" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesActiveServiceType] -- 'modeGetActiveServiceTypes' route
    :<|> "Occupancy" :> "CarPark" :> Capture "id" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesCarParkOccupancy -- 'occupancyGet' route
    :<|> "Occupancy" :> "ChargeConnector" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesChargeConnectorOccupancy] -- 'occupancyGetAllChargeConnectorStatus' route
    :<|> "Occupancy" :> "BikePoints" :> Capture "ids" [Text] :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesBikePointOccupancy] -- 'occupancyGetBikePointsOccupancies' route
    :<|> "Occupancy" :> "ChargeConnector" :> Capture "ids" [Text] :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesChargeConnectorOccupancy] -- 'occupancyGetChargeConnectorStatus' route
    :<|> "Occupancy" :> "CarPark" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesCarParkOccupancy] -- 'occupancyGet_0' route
    :<|> "Place" :> Capture "id" Text :> QueryParam "includeChildren" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'placeGet' route
    :<|> "Place" :> Capture "type" [Text] :> "At" :> Capture "Lat" null :> Capture "Lon" null :> QueryParam "lat" Text :> QueryParam "lon" Text :> QueryParam "location.lat" Double :> QueryParam "location.lon" Double :> Verb 'GET 200 '[JSON] Value -- 'placeGetAt' route
    :<|> "Place" :> QueryParam "radius" Double :> QueryParam "categories" (QueryList 'MultiParamArray (Text)) :> QueryParam "includeChildren" Bool :> QueryParam "type" (QueryList 'MultiParamArray (Text)) :> QueryParam "activeOnly" Bool :> QueryParam "numberOfPlacesToReturn" Int :> QueryParam "placeGeo.swLat" Double :> QueryParam "placeGeo.swLon" Double :> QueryParam "placeGeo.neLat" Double :> QueryParam "placeGeo.neLon" Double :> QueryParam "placeGeo.lat" Double :> QueryParam "placeGeo.lon" Double :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'placeGetByGeo' route
    :<|> "Place" :> "Type" :> Capture "types" [Text] :> QueryParam "activeOnly" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'placeGetByType' route
    :<|> "Place" :> Capture "type" [Text] :> "overlay" :> Capture "z" Int :> Capture "Lat" null :> Capture "Lon" null :> Capture "width" Int :> Capture "height" Int :> QueryParam "lat" Text :> QueryParam "lon" Text :> QueryParam "location.lat" Double :> QueryParam "location.lon" Double :> Verb 'GET 200 '[JSON] Value -- 'placeGetOverlay' route
    :<|> "Place" :> "Address" :> "Streets" :> Capture "Postcode" null :> QueryParam "postcode" Text :> QueryParam "postcodeInput.postcode" Text :> Verb 'GET 200 '[JSON] Value -- 'placeGetStreetsByPostCode' route
    :<|> "Place" :> "Meta" :> "Categories" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlaceCategory] -- 'placeMetaCategories' route
    :<|> "Place" :> "Meta" :> "PlaceTypes" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlaceCategory] -- 'placeMetaPlaceTypes' route
    :<|> "Place" :> "Search" :> QueryParam "name" Text :> QueryParam "types" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'placeSearch' route
    :<|> "Road" :> "all" :> "Street" :> "Disruption" :> QueryParam "startDate" UTCTime :> QueryParam "endDate" UTCTime :> Verb 'GET 200 '[JSON] Value -- 'roadDisruptedStreets' route
    :<|> "Road" :> Capture "ids" [Text] :> "Disruption" :> QueryParam "stripContent" Bool :> QueryParam "severities" (QueryList 'MultiParamArray (Text)) :> QueryParam "categories" (QueryList 'MultiParamArray (Text)) :> QueryParam "closures" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesRoadDisruption] -- 'roadDisruption' route
    :<|> "Road" :> "all" :> "Disruption" :> Capture "disruptionIds" [Text] :> QueryParam "stripContent" Bool :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesRoadDisruption -- 'roadDisruptionById' route
    :<|> "Road" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesRoadCorridor] -- 'roadGet' route
    :<|> "Road" :> Capture "ids" [Text] :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesRoadCorridor] -- 'roadGet_0' route
    :<|> "Road" :> "Meta" :> "Categories" :> Verb 'GET 200 '[JSON] [Text] -- 'roadMetaCategories' route
    :<|> "Road" :> "Meta" :> "Severities" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStatusSeverity] -- 'roadMetaSeverities' route
    :<|> "Road" :> Capture "ids" [Text] :> "Status" :> QueryParam "dateRangeNullable.startDate" UTCTime :> QueryParam "dateRangeNullable.endDate" UTCTime :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesRoadCorridor] -- 'roadStatus' route
    :<|> "Search" :> "BusSchedules" :> QueryParam "query" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesSearchResponse -- 'searchBusSchedules' route
    :<|> "Search" :> QueryParam "query" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesSearchResponse -- 'searchGet' route
    :<|> "Search" :> "Meta" :> "Categories" :> Verb 'GET 200 '[JSON] [Text] -- 'searchMetaCategories' route
    :<|> "Search" :> "Meta" :> "SearchProviders" :> Verb 'GET 200 '[JSON] [Text] -- 'searchMetaSearchProviders' route
    :<|> "Search" :> "Meta" :> "Sorts" :> Verb 'GET 200 '[JSON] [Text] -- 'searchMetaSorts' route
    :<|> "StopPoint" :> Capture "id" Text :> "ArrivalDepartures" :> QueryParam "lineIds" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesArrivalDeparture] -- 'stopPointArrivalDepartures' route
    :<|> "StopPoint" :> Capture "id" Text :> "Arrivals" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPrediction] -- 'stopPointArrivals' route
    :<|> "StopPoint" :> Capture "id" Text :> "Crowding" :> Capture "line" Text :> QueryParam "direction" Text :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'stopPointCrowding' route
    :<|> "StopPoint" :> Capture "id" Text :> "DirectionTo" :> Capture "toStopPointId" Text :> QueryParam "lineId" Text :> Verb 'GET 200 '[JSON] Text -- 'stopPointDirection' route
    :<|> "StopPoint" :> Capture "ids" [Text] :> "Disruption" :> QueryParam "getFamily" Bool :> QueryParam "includeRouteBlockedStops" Bool :> QueryParam "flattenResponse" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesDisruptedPoint] -- 'stopPointDisruption' route
    :<|> "StopPoint" :> "Mode" :> Capture "modes" [Text] :> "Disruption" :> QueryParam "includeRouteBlockedStops" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesDisruptedPoint] -- 'stopPointDisruptionByMode' route
    :<|> "StopPoint" :> Capture "ids" [Text] :> QueryParam "includeCrowdingData" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'stopPointGet' route
    :<|> "StopPoint" :> QueryParam "stopTypes" (QueryList 'MultiParamArray (Text)) :> QueryParam "radius" Int :> QueryParam "useStopPointHierarchy" Bool :> QueryParam "modes" (QueryList 'MultiParamArray (Text)) :> QueryParam "categories" (QueryList 'MultiParamArray (Text)) :> QueryParam "returnLines" Bool :> QueryParam "location.lat" Double :> QueryParam "location.lon" Double :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesStopPointsResponse -- 'stopPointGetByGeoPoint' route
    :<|> "StopPoint" :> "Mode" :> Capture "modes" [Text] :> QueryParam "page" Int :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesStopPointsResponse -- 'stopPointGetByMode' route
    :<|> "StopPoint" :> "Sms" :> Capture "id" Text :> QueryParam "output" Text :> Verb 'GET 200 '[JSON] Value -- 'stopPointGetBySms' route
    :<|> "StopPoint" :> "Type" :> Capture "types" [Text] :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'stopPointGetByType' route
    :<|> "StopPoint" :> "Type" :> Capture "types" [Text] :> "page" :> Capture "page" Int :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'stopPointGetByTypeWithPagination' route
    :<|> "StopPoint" :> Capture "stopPointId" Text :> "CarParks" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'stopPointGetCarParksById' route
    :<|> "StopPoint" :> "ServiceTypes" :> QueryParam "id" Text :> QueryParam "lineIds" (QueryList 'MultiParamArray (Text)) :> QueryParam "modes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLineServiceType] -- 'stopPointGetServiceTypes' route
    :<|> "StopPoint" :> Capture "stopPointId" Text :> "TaxiRanks" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'stopPointGetTaxiRanksByIds' route
    :<|> "StopPoint" :> Capture "id" Text :> "placeTypes" :> QueryParam "placeTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'stopPointGet_0' route
    :<|> "StopPoint" :> "Meta" :> "Categories" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPointCategory] -- 'stopPointMetaCategories' route
    :<|> "StopPoint" :> "Meta" :> "Modes" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesMode] -- 'stopPointMetaModes' route
    :<|> "StopPoint" :> "Meta" :> "StopTypes" :> Verb 'GET 200 '[JSON] [Text] -- 'stopPointMetaStopTypes' route
    :<|> "StopPoint" :> Capture "id" Text :> "CanReachOnLine" :> Capture "lineId" Text :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'stopPointReachableFrom' route
    :<|> "StopPoint" :> Capture "id" Text :> "Route" :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPointRouteSection] -- 'stopPointRoute' route
    :<|> "StopPoint" :> "Search" :> Capture "query" Text :> QueryParam "modes" (QueryList 'MultiParamArray (Text)) :> QueryParam "faresOnly" Bool :> QueryParam "maxResults" Int :> QueryParam "lines" (QueryList 'MultiParamArray (Text)) :> QueryParam "includeHubs" Bool :> QueryParam "tflOperatedNationalRailStationsOnly" Bool :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesSearchResponse -- 'stopPointSearch' route
    :<|> "StopPoint" :> "Search" :> QueryParam "query" Text :> QueryParam "modes" (QueryList 'MultiParamArray (Text)) :> QueryParam "faresOnly" Bool :> QueryParam "maxResults" Int :> QueryParam "lines" (QueryList 'MultiParamArray (Text)) :> QueryParam "includeHubs" Bool :> QueryParam "tflOperatedNationalRailStationsOnly" Bool :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesSearchResponse -- 'stopPointSearch_0' route
    :<|> "TravelTimes" :> "compareOverlay" :> Capture "z" Int :> "mapcenter" :> Capture "mapCenterLat" Double :> Capture "mapCenterLon" Double :> "pinlocation" :> Capture "pinLat" Double :> Capture "pinLon" Double :> "dimensions" :> Capture "width" Int :> Capture "height" Int :> QueryParam "scenarioTitle" Text :> QueryParam "timeOfDayId" Text :> QueryParam "modeId" Text :> QueryParam "direction" Text :> QueryParam "travelTimeInterval" Int :> QueryParam "compareType" Text :> QueryParam "compareValue" Text :> Verb 'GET 200 '[JSON] Value -- 'travelTimeGetCompareOverlay' route
    :<|> "TravelTimes" :> "overlay" :> Capture "z" Int :> "mapcenter" :> Capture "mapCenterLat" Double :> Capture "mapCenterLon" Double :> "pinlocation" :> Capture "pinLat" Double :> Capture "pinLon" Double :> "dimensions" :> Capture "width" Int :> Capture "height" Int :> QueryParam "scenarioTitle" Text :> QueryParam "timeOfDayId" Text :> QueryParam "modeId" Text :> QueryParam "direction" Text :> QueryParam "travelTimeInterval" Int :> Verb 'GET 200 '[JSON] Value -- 'travelTimeGetOverlay' route
    :<|> "Vehicle" :> Capture "ids" [Text] :> "Arrivals" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPrediction] -- 'vehicleGet' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype TransportForLondonUnifiedClientError = TransportForLondonUnifiedClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for TransportForLondonUnified.
-- The backend can be used both for the client and the server. The client generated from the TransportForLondonUnified OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createTransportForLondonUnifiedClient@). Alternatively, provided
-- a backend, the API can be served using @runTransportForLondonUnifiedMiddlewareServer@.
data TransportForLondonUnifiedBackend a m = TransportForLondonUnifiedBackend
  { accidentStatsGet :: Int -> m [TflApiPresentationEntitiesAccidentStatsAccidentDetail]{- ^  -}
  , airQualityGet :: m Value{- ^  -}
  , bikePointGet :: Text -> m TflApiPresentationEntitiesPlace{- ^  -}
  , bikePointGetAll :: m [TflApiPresentationEntitiesPlace]{- ^  -}
  , bikePointSearch :: Maybe Text -> m [TflApiPresentationEntitiesPlace]{- ^  -}
  , cabwiseGet :: Maybe Double -> Maybe Double -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Text -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> m Value{- ^  -}
  , journeyJourneyResults :: Text -> Text -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe [Text] -> Maybe [Text] -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe [Text] -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Int -> Maybe Bool -> m TflApiPresentationEntitiesJourneyPlannerItineraryResult{- ^  -}
  , journeyMeta :: m [TflApiPresentationEntitiesMode]{- ^  -}
  , lineArrivals :: [Text] -> Text -> Maybe Text -> Maybe Text -> m [TflApiPresentationEntitiesPrediction]{- ^  -}
  , lineDisruption :: [Text] -> m [TflApiPresentationEntitiesDisruption]{- ^  -}
  , lineDisruptionByMode :: [Text] -> m [TflApiPresentationEntitiesDisruption]{- ^  -}
  , lineGet :: [Text] -> m [TflApiPresentationEntitiesLine]{- ^  -}
  , lineGetByMode :: [Text] -> m [TflApiPresentationEntitiesLine]{- ^  -}
  , lineLineRoutesByIds :: [Text] -> Maybe [Text] -> m [TflApiPresentationEntitiesLine]{- ^  -}
  , lineMetaDisruptionCategories :: m [Text]{- ^  -}
  , lineMetaModes :: m [TflApiPresentationEntitiesMode]{- ^  -}
  , lineMetaServiceTypes :: m [Text]{- ^  -}
  , lineMetaSeverity :: m [TflApiPresentationEntitiesStatusSeverity]{- ^  -}
  , lineRoute :: Maybe [Text] -> m [TflApiPresentationEntitiesLine]{- ^  -}
  , lineRouteByMode :: [Text] -> Maybe [Text] -> m [TflApiPresentationEntitiesLine]{- ^  -}
  , lineRouteSequence :: Text -> Text -> Maybe [Text] -> Maybe Bool -> m TflApiPresentationEntitiesRouteSequence{- ^  -}
  , lineSearch :: Text -> Maybe [Text] -> Maybe [Text] -> m TflApiPresentationEntitiesRouteSearchResponse{- ^  -}
  , lineStatus :: [Text] -> Text -> Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> m [TflApiPresentationEntitiesLine]{- ^  -}
  , lineStatusByIds :: [Text] -> Maybe Bool -> m [TflApiPresentationEntitiesLine]{- ^  -}
  , lineStatusByMode :: [Text] -> Maybe Bool -> Maybe Text -> m [TflApiPresentationEntitiesLine]{- ^  -}
  , lineStatusBySeverity :: Int -> m [TflApiPresentationEntitiesLine]{- ^  -}
  , lineStopPoints :: Text -> Maybe Bool -> m [TflApiPresentationEntitiesStopPoint]{- ^  -}
  , lineTimetable :: Text -> Text -> m TflApiPresentationEntitiesTimetableResponse{- ^  -}
  , lineTimetableTo :: Text -> Text -> Text -> m TflApiPresentationEntitiesTimetableResponse{- ^  -}
  , modeArrivals :: Text -> Maybe Int -> m [TflApiPresentationEntitiesPrediction]{- ^  -}
  , modeGetActiveServiceTypes :: m [TflApiPresentationEntitiesActiveServiceType]{- ^  -}
  , occupancyGet :: Text -> m TflApiPresentationEntitiesCarParkOccupancy{- ^  -}
  , occupancyGetAllChargeConnectorStatus :: m [TflApiPresentationEntitiesChargeConnectorOccupancy]{- ^  -}
  , occupancyGetBikePointsOccupancies :: [Text] -> m [TflApiPresentationEntitiesBikePointOccupancy]{- ^  -}
  , occupancyGetChargeConnectorStatus :: [Text] -> m [TflApiPresentationEntitiesChargeConnectorOccupancy]{- ^  -}
  , occupancyGet_0 :: m [TflApiPresentationEntitiesCarParkOccupancy]{- ^  -}
  , placeGet :: Text -> Maybe Bool -> m [TflApiPresentationEntitiesPlace]{- ^  -}
  , placeGetAt :: [Text] -> null -> null -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> m Value{- ^  -}
  , placeGetByGeo :: Maybe Double -> Maybe [Text] -> Maybe Bool -> Maybe [Text] -> Maybe Bool -> Maybe Int -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> m [TflApiPresentationEntitiesStopPoint]{- ^  -}
  , placeGetByType :: [Text] -> Maybe Bool -> m [TflApiPresentationEntitiesPlace]{- ^  -}
  , placeGetOverlay :: [Text] -> Int -> null -> null -> Int -> Int -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> m Value{- ^  -}
  , placeGetStreetsByPostCode :: null -> Maybe Text -> Maybe Text -> m Value{- ^  -}
  , placeMetaCategories :: m [TflApiPresentationEntitiesPlaceCategory]{- ^  -}
  , placeMetaPlaceTypes :: m [TflApiPresentationEntitiesPlaceCategory]{- ^  -}
  , placeSearch :: Maybe Text -> Maybe [Text] -> m [TflApiPresentationEntitiesPlace]{- ^  -}
  , roadDisruptedStreets :: Maybe UTCTime -> Maybe UTCTime -> m Value{- ^  -}
  , roadDisruption :: [Text] -> Maybe Bool -> Maybe [Text] -> Maybe [Text] -> Maybe Bool -> m [TflApiPresentationEntitiesRoadDisruption]{- ^  -}
  , roadDisruptionById :: [Text] -> Maybe Bool -> m TflApiPresentationEntitiesRoadDisruption{- ^  -}
  , roadGet :: m [TflApiPresentationEntitiesRoadCorridor]{- ^  -}
  , roadGet_0 :: [Text] -> m [TflApiPresentationEntitiesRoadCorridor]{- ^  -}
  , roadMetaCategories :: m [Text]{- ^  -}
  , roadMetaSeverities :: m [TflApiPresentationEntitiesStatusSeverity]{- ^  -}
  , roadStatus :: [Text] -> Maybe UTCTime -> Maybe UTCTime -> m [TflApiPresentationEntitiesRoadCorridor]{- ^  -}
  , searchBusSchedules :: Maybe Text -> m TflApiPresentationEntitiesSearchResponse{- ^  -}
  , searchGet :: Maybe Text -> m TflApiPresentationEntitiesSearchResponse{- ^  -}
  , searchMetaCategories :: m [Text]{- ^  -}
  , searchMetaSearchProviders :: m [Text]{- ^  -}
  , searchMetaSorts :: m [Text]{- ^  -}
  , stopPointArrivalDepartures :: Text -> Maybe [Text] -> m [TflApiPresentationEntitiesArrivalDeparture]{- ^  -}
  , stopPointArrivals :: Text -> m [TflApiPresentationEntitiesPrediction]{- ^  -}
  , stopPointCrowding :: Text -> Text -> Maybe Text -> m [TflApiPresentationEntitiesStopPoint]{- ^  -}
  , stopPointDirection :: Text -> Text -> Maybe Text -> m Text{- ^  -}
  , stopPointDisruption :: [Text] -> Maybe Bool -> Maybe Bool -> Maybe Bool -> m [TflApiPresentationEntitiesDisruptedPoint]{- ^  -}
  , stopPointDisruptionByMode :: [Text] -> Maybe Bool -> m [TflApiPresentationEntitiesDisruptedPoint]{- ^  -}
  , stopPointGet :: [Text] -> Maybe Bool -> m [TflApiPresentationEntitiesStopPoint]{- ^  -}
  , stopPointGetByGeoPoint :: Maybe [Text] -> Maybe Int -> Maybe Bool -> Maybe [Text] -> Maybe [Text] -> Maybe Bool -> Maybe Double -> Maybe Double -> m TflApiPresentationEntitiesStopPointsResponse{- ^  -}
  , stopPointGetByMode :: [Text] -> Maybe Int -> m TflApiPresentationEntitiesStopPointsResponse{- ^  -}
  , stopPointGetBySms :: Text -> Maybe Text -> m Value{- ^  -}
  , stopPointGetByType :: [Text] -> m [TflApiPresentationEntitiesStopPoint]{- ^  -}
  , stopPointGetByTypeWithPagination :: [Text] -> Int -> m [TflApiPresentationEntitiesStopPoint]{- ^  -}
  , stopPointGetCarParksById :: Text -> m [TflApiPresentationEntitiesPlace]{- ^  -}
  , stopPointGetServiceTypes :: Maybe Text -> Maybe [Text] -> Maybe [Text] -> m [TflApiPresentationEntitiesLineServiceType]{- ^  -}
  , stopPointGetTaxiRanksByIds :: Text -> m [TflApiPresentationEntitiesPlace]{- ^  -}
  , stopPointGet_0 :: Text -> Maybe [Text] -> m [TflApiPresentationEntitiesPlace]{- ^  -}
  , stopPointMetaCategories :: m [TflApiPresentationEntitiesStopPointCategory]{- ^  -}
  , stopPointMetaModes :: m [TflApiPresentationEntitiesMode]{- ^  -}
  , stopPointMetaStopTypes :: m [Text]{- ^  -}
  , stopPointReachableFrom :: Text -> Text -> Maybe [Text] -> m [TflApiPresentationEntitiesStopPoint]{- ^  -}
  , stopPointRoute :: Text -> Maybe [Text] -> m [TflApiPresentationEntitiesStopPointRouteSection]{- ^  -}
  , stopPointSearch :: Text -> Maybe [Text] -> Maybe Bool -> Maybe Int -> Maybe [Text] -> Maybe Bool -> Maybe Bool -> m TflApiPresentationEntitiesSearchResponse{- ^  -}
  , stopPointSearch_0 :: Maybe Text -> Maybe [Text] -> Maybe Bool -> Maybe Int -> Maybe [Text] -> Maybe Bool -> Maybe Bool -> m TflApiPresentationEntitiesSearchResponse{- ^  -}
  , travelTimeGetCompareOverlay :: Int -> Double -> Double -> Double -> Double -> Int -> Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> m Value{- ^  -}
  , travelTimeGetOverlay :: Int -> Double -> Double -> Double -> Double -> Int -> Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> m Value{- ^  -}
  , vehicleGet :: [Text] -> m [TflApiPresentationEntitiesPrediction]{- ^  -}
  }

-- | Authentication settings for TransportForLondonUnified.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data TransportForLondonUnifiedAuth = TransportForLondonUnifiedAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }

newtype TransportForLondonUnifiedClient a = TransportForLondonUnifiedClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative TransportForLondonUnifiedClient where
  pure x = TransportForLondonUnifiedClient (\_ -> pure x)
  (TransportForLondonUnifiedClient f) <*> (TransportForLondonUnifiedClient x) =
    TransportForLondonUnifiedClient (\env -> f env <*> x env)

instance Monad TransportForLondonUnifiedClient where
  (TransportForLondonUnifiedClient a) >>= f =
    TransportForLondonUnifiedClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO TransportForLondonUnifiedClient where
  liftIO io = TransportForLondonUnifiedClient (\_ -> liftIO io)

createTransportForLondonUnifiedClient :: TransportForLondonUnifiedBackend AuthClient TransportForLondonUnifiedClient
createTransportForLondonUnifiedClient = TransportForLondonUnifiedBackend{..}
  where
    ((coerce -> accidentStatsGet) :<|>
     (coerce -> airQualityGet) :<|>
     (coerce -> bikePointGet) :<|>
     (coerce -> bikePointGetAll) :<|>
     (coerce -> bikePointSearch) :<|>
     (coerce -> cabwiseGet) :<|>
     (coerce -> journeyJourneyResults) :<|>
     (coerce -> journeyMeta) :<|>
     (coerce -> lineArrivals) :<|>
     (coerce -> lineDisruption) :<|>
     (coerce -> lineDisruptionByMode) :<|>
     (coerce -> lineGet) :<|>
     (coerce -> lineGetByMode) :<|>
     (coerce -> lineLineRoutesByIds) :<|>
     (coerce -> lineMetaDisruptionCategories) :<|>
     (coerce -> lineMetaModes) :<|>
     (coerce -> lineMetaServiceTypes) :<|>
     (coerce -> lineMetaSeverity) :<|>
     (coerce -> lineRoute) :<|>
     (coerce -> lineRouteByMode) :<|>
     (coerce -> lineRouteSequence) :<|>
     (coerce -> lineSearch) :<|>
     (coerce -> lineStatus) :<|>
     (coerce -> lineStatusByIds) :<|>
     (coerce -> lineStatusByMode) :<|>
     (coerce -> lineStatusBySeverity) :<|>
     (coerce -> lineStopPoints) :<|>
     (coerce -> lineTimetable) :<|>
     (coerce -> lineTimetableTo) :<|>
     (coerce -> modeArrivals) :<|>
     (coerce -> modeGetActiveServiceTypes) :<|>
     (coerce -> occupancyGet) :<|>
     (coerce -> occupancyGetAllChargeConnectorStatus) :<|>
     (coerce -> occupancyGetBikePointsOccupancies) :<|>
     (coerce -> occupancyGetChargeConnectorStatus) :<|>
     (coerce -> occupancyGet_0) :<|>
     (coerce -> placeGet) :<|>
     (coerce -> placeGetAt) :<|>
     (coerce -> placeGetByGeo) :<|>
     (coerce -> placeGetByType) :<|>
     (coerce -> placeGetOverlay) :<|>
     (coerce -> placeGetStreetsByPostCode) :<|>
     (coerce -> placeMetaCategories) :<|>
     (coerce -> placeMetaPlaceTypes) :<|>
     (coerce -> placeSearch) :<|>
     (coerce -> roadDisruptedStreets) :<|>
     (coerce -> roadDisruption) :<|>
     (coerce -> roadDisruptionById) :<|>
     (coerce -> roadGet) :<|>
     (coerce -> roadGet_0) :<|>
     (coerce -> roadMetaCategories) :<|>
     (coerce -> roadMetaSeverities) :<|>
     (coerce -> roadStatus) :<|>
     (coerce -> searchBusSchedules) :<|>
     (coerce -> searchGet) :<|>
     (coerce -> searchMetaCategories) :<|>
     (coerce -> searchMetaSearchProviders) :<|>
     (coerce -> searchMetaSorts) :<|>
     (coerce -> stopPointArrivalDepartures) :<|>
     (coerce -> stopPointArrivals) :<|>
     (coerce -> stopPointCrowding) :<|>
     (coerce -> stopPointDirection) :<|>
     (coerce -> stopPointDisruption) :<|>
     (coerce -> stopPointDisruptionByMode) :<|>
     (coerce -> stopPointGet) :<|>
     (coerce -> stopPointGetByGeoPoint) :<|>
     (coerce -> stopPointGetByMode) :<|>
     (coerce -> stopPointGetBySms) :<|>
     (coerce -> stopPointGetByType) :<|>
     (coerce -> stopPointGetByTypeWithPagination) :<|>
     (coerce -> stopPointGetCarParksById) :<|>
     (coerce -> stopPointGetServiceTypes) :<|>
     (coerce -> stopPointGetTaxiRanksByIds) :<|>
     (coerce -> stopPointGet_0) :<|>
     (coerce -> stopPointMetaCategories) :<|>
     (coerce -> stopPointMetaModes) :<|>
     (coerce -> stopPointMetaStopTypes) :<|>
     (coerce -> stopPointReachableFrom) :<|>
     (coerce -> stopPointRoute) :<|>
     (coerce -> stopPointSearch) :<|>
     (coerce -> stopPointSearch_0) :<|>
     (coerce -> travelTimeGetCompareOverlay) :<|>
     (coerce -> travelTimeGetOverlay) :<|>
     (coerce -> vehicleGet) :<|>
     _) = client (Proxy :: Proxy TransportForLondonUnifiedAPI)

-- | Run requests in the TransportForLondonUnifiedClient monad.
runTransportForLondonUnifiedClient :: Config -> TransportForLondonUnifiedClient a -> ExceptT ClientError IO a
runTransportForLondonUnifiedClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runTransportForLondonUnifiedClientWithManager manager clientConfig cl

-- | Run requests in the TransportForLondonUnifiedClient monad using a custom manager.
runTransportForLondonUnifiedClientWithManager :: Manager -> Config -> TransportForLondonUnifiedClient a -> ExceptT ClientError IO a
runTransportForLondonUnifiedClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a TransportForLondonUnifiedClientError
callTransportForLondonUnified
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> TransportForLondonUnifiedClient a -> m a
callTransportForLondonUnified env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (TransportForLondonUnifiedClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the TransportForLondonUnified server at the provided host and port.
runTransportForLondonUnifiedServer
  :: (MonadIO m, MonadThrow m)
  => Config -> TransportForLondonUnifiedAuth -> TransportForLondonUnifiedBackend AuthServer (ExceptT ServerError IO) -> m ()
runTransportForLondonUnifiedServer config auth backend = runTransportForLondonUnifiedMiddlewareServer config requestMiddlewareId auth backend

-- | Run the TransportForLondonUnified server at the provided host and port.
runTransportForLondonUnifiedMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> TransportForLondonUnifiedAuth -> TransportForLondonUnifiedBackend AuthServer (ExceptT ServerError IO) -> m ()
runTransportForLondonUnifiedMiddlewareServer Config{..} middleware auth backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationTransportForLondonUnified auth backend

-- | Plain "Network.Wai" Application for the TransportForLondonUnified server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationTransportForLondonUnified :: TransportForLondonUnifiedAuth -> TransportForLondonUnifiedBackend AuthServer (ExceptT ServerError IO) -> Application
serverWaiApplicationTransportForLondonUnified auth backend = serveWithContextT (Proxy :: Proxy TransportForLondonUnifiedAPI) context id (serverFromBackend backend)
  where
    context = serverContext auth
    serverFromBackend TransportForLondonUnifiedBackend{..} =
      (coerce accidentStatsGet :<|>
       coerce airQualityGet :<|>
       coerce bikePointGet :<|>
       coerce bikePointGetAll :<|>
       coerce bikePointSearch :<|>
       coerce cabwiseGet :<|>
       coerce journeyJourneyResults :<|>
       coerce journeyMeta :<|>
       coerce lineArrivals :<|>
       coerce lineDisruption :<|>
       coerce lineDisruptionByMode :<|>
       coerce lineGet :<|>
       coerce lineGetByMode :<|>
       coerce lineLineRoutesByIds :<|>
       coerce lineMetaDisruptionCategories :<|>
       coerce lineMetaModes :<|>
       coerce lineMetaServiceTypes :<|>
       coerce lineMetaSeverity :<|>
       coerce lineRoute :<|>
       coerce lineRouteByMode :<|>
       coerce lineRouteSequence :<|>
       coerce lineSearch :<|>
       coerce lineStatus :<|>
       coerce lineStatusByIds :<|>
       coerce lineStatusByMode :<|>
       coerce lineStatusBySeverity :<|>
       coerce lineStopPoints :<|>
       coerce lineTimetable :<|>
       coerce lineTimetableTo :<|>
       coerce modeArrivals :<|>
       coerce modeGetActiveServiceTypes :<|>
       coerce occupancyGet :<|>
       coerce occupancyGetAllChargeConnectorStatus :<|>
       coerce occupancyGetBikePointsOccupancies :<|>
       coerce occupancyGetChargeConnectorStatus :<|>
       coerce occupancyGet_0 :<|>
       coerce placeGet :<|>
       coerce placeGetAt :<|>
       coerce placeGetByGeo :<|>
       coerce placeGetByType :<|>
       coerce placeGetOverlay :<|>
       coerce placeGetStreetsByPostCode :<|>
       coerce placeMetaCategories :<|>
       coerce placeMetaPlaceTypes :<|>
       coerce placeSearch :<|>
       coerce roadDisruptedStreets :<|>
       coerce roadDisruption :<|>
       coerce roadDisruptionById :<|>
       coerce roadGet :<|>
       coerce roadGet_0 :<|>
       coerce roadMetaCategories :<|>
       coerce roadMetaSeverities :<|>
       coerce roadStatus :<|>
       coerce searchBusSchedules :<|>
       coerce searchGet :<|>
       coerce searchMetaCategories :<|>
       coerce searchMetaSearchProviders :<|>
       coerce searchMetaSorts :<|>
       coerce stopPointArrivalDepartures :<|>
       coerce stopPointArrivals :<|>
       coerce stopPointCrowding :<|>
       coerce stopPointDirection :<|>
       coerce stopPointDisruption :<|>
       coerce stopPointDisruptionByMode :<|>
       coerce stopPointGet :<|>
       coerce stopPointGetByGeoPoint :<|>
       coerce stopPointGetByMode :<|>
       coerce stopPointGetBySms :<|>
       coerce stopPointGetByType :<|>
       coerce stopPointGetByTypeWithPagination :<|>
       coerce stopPointGetCarParksById :<|>
       coerce stopPointGetServiceTypes :<|>
       coerce stopPointGetTaxiRanksByIds :<|>
       coerce stopPointGet_0 :<|>
       coerce stopPointMetaCategories :<|>
       coerce stopPointMetaModes :<|>
       coerce stopPointMetaStopTypes :<|>
       coerce stopPointReachableFrom :<|>
       coerce stopPointRoute :<|>
       coerce stopPointSearch :<|>
       coerce stopPointSearch_0 :<|>
       coerce travelTimeGetCompareOverlay :<|>
       coerce travelTimeGetOverlay :<|>
       coerce vehicleGet :<|>
       serveDirectoryFileServer "static")

-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: TransportForLondonUnifiedAuth -> AuthHandler Request AuthServer
authHandler TransportForLondonUnifiedAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "app_key" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "app_key")

serverContext :: TransportForLondonUnifiedAuth -> Context (AuthHandler Request AuthServer ': '[])
serverContext auth = authHandler auth :. EmptyContext
