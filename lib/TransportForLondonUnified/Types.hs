{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module TransportForLondonUnified.Types (
  SystemDataSpatialDbGeography (..),
  SystemDataSpatialDbGeographyWellKnownValue (..),
  TflApiCommonApiVersionInfo (..),
  TflApiCommonDateRange (..),
  TflApiCommonDateRangeNullable (..),
  TflApiCommonGeoPoint (..),
  TflApiCommonJourneyPlannerJpElevation (..),
  TflApiCommonPlaceGeo (..),
  TflApiCommonPostcodeInput (..),
  TflApiPresentationEntitiesAccidentStatsAccidentDetail (..),
  TflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary (..),
  TflApiPresentationEntitiesAccidentStatsCasualty (..),
  TflApiPresentationEntitiesAccidentStatsVehicle (..),
  TflApiPresentationEntitiesActiveServiceType (..),
  TflApiPresentationEntitiesAdditionalProperties (..),
  TflApiPresentationEntitiesArrivalDeparture (..),
  TflApiPresentationEntitiesArrivalDepartureWithLine (..),
  TflApiPresentationEntitiesBay (..),
  TflApiPresentationEntitiesBikePointOccupancy (..),
  TflApiPresentationEntitiesCarParkOccupancy (..),
  TflApiPresentationEntitiesChargeConnectorOccupancy (..),
  TflApiPresentationEntitiesCoordinate (..),
  TflApiPresentationEntitiesCrowding (..),
  TflApiPresentationEntitiesCycleSuperhighway (..),
  TflApiPresentationEntitiesDisruptedPoint (..),
  TflApiPresentationEntitiesDisruptedRoute (..),
  TflApiPresentationEntitiesDisruption (..),
  TflApiPresentationEntitiesFaresFare (..),
  TflApiPresentationEntitiesFaresFareBounds (..),
  TflApiPresentationEntitiesFaresFareDetails (..),
  TflApiPresentationEntitiesFaresFareStation (..),
  TflApiPresentationEntitiesFaresFaresMode (..),
  TflApiPresentationEntitiesFaresFaresPeriod (..),
  TflApiPresentationEntitiesFaresFaresSection (..),
  TflApiPresentationEntitiesFaresJourney (..),
  TflApiPresentationEntitiesFaresPassengerType (..),
  TflApiPresentationEntitiesFaresRecommendation (..),
  TflApiPresentationEntitiesFaresRecommendationResponse (..),
  TflApiPresentationEntitiesFaresTicket (..),
  TflApiPresentationEntitiesFaresTicketTime (..),
  TflApiPresentationEntitiesFaresTicketType (..),
  TflApiPresentationEntitiesGeoCodeSearchMatch (..),
  TflApiPresentationEntitiesIdentifier (..),
  TflApiPresentationEntitiesInstruction (..),
  TflApiPresentationEntitiesInstructionStep (..),
  TflApiPresentationEntitiesInterval (..),
  TflApiPresentationEntitiesJourneyPlannerFare (..),
  TflApiPresentationEntitiesJourneyPlannerFareCaveat (..),
  TflApiPresentationEntitiesJourneyPlannerFareTap (..),
  TflApiPresentationEntitiesJourneyPlannerFareTapDetails (..),
  TflApiPresentationEntitiesJourneyPlannerItineraryResult (..),
  TflApiPresentationEntitiesJourneyPlannerJourney (..),
  TflApiPresentationEntitiesJourneyPlannerJourneyFare (..),
  TflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData (..),
  TflApiPresentationEntitiesJourneyPlannerJourneyVector (..),
  TflApiPresentationEntitiesJourneyPlannerLeg (..),
  TflApiPresentationEntitiesJourneyPlannerObstacle (..),
  TflApiPresentationEntitiesJourneyPlannerPath (..),
  TflApiPresentationEntitiesJourneyPlannerPlannedWork (..),
  TflApiPresentationEntitiesJourneyPlannerRouteOption (..),
  TflApiPresentationEntitiesJourneyPlannerSearchCriteria (..),
  TflApiPresentationEntitiesJourneyPlannerTimeAdjustment (..),
  TflApiPresentationEntitiesJourneyPlannerTimeAdjustments (..),
  TflApiPresentationEntitiesKnownJourney (..),
  TflApiPresentationEntitiesLine (..),
  TflApiPresentationEntitiesLineGroup (..),
  TflApiPresentationEntitiesLineModeGroup (..),
  TflApiPresentationEntitiesLineRouteSection (..),
  TflApiPresentationEntitiesLineServiceType (..),
  TflApiPresentationEntitiesLineServiceTypeInfo (..),
  TflApiPresentationEntitiesLineSpecificServiceType (..),
  TflApiPresentationEntitiesLineStatus (..),
  TflApiPresentationEntitiesMatchedRoute (..),
  TflApiPresentationEntitiesMatchedRouteSections (..),
  TflApiPresentationEntitiesMatchedStop (..),
  TflApiPresentationEntitiesMessage (..),
  TflApiPresentationEntitiesMode (..),
  TflApiPresentationEntitiesNetworkStatus (..),
  TflApiPresentationEntitiesOrderedRoute (..),
  TflApiPresentationEntitiesPassengerFlow (..),
  TflApiPresentationEntitiesPathAttribute (..),
  TflApiPresentationEntitiesPeriod (..),
  TflApiPresentationEntitiesPlace (..),
  TflApiPresentationEntitiesPlaceCategory (..),
  TflApiPresentationEntitiesPlacePolygon (..),
  TflApiPresentationEntitiesPoint (..),
  TflApiPresentationEntitiesPrediction (..),
  TflApiPresentationEntitiesPredictionTiming (..),
  TflApiPresentationEntitiesRedirect (..),
  TflApiPresentationEntitiesRoadCorridor (..),
  TflApiPresentationEntitiesRoadDisruption (..),
  TflApiPresentationEntitiesRoadDisruptionImpactArea (..),
  TflApiPresentationEntitiesRoadDisruptionLine (..),
  TflApiPresentationEntitiesRoadDisruptionSchedule (..),
  TflApiPresentationEntitiesRoadProject (..),
  TflApiPresentationEntitiesRouteSearchMatch (..),
  TflApiPresentationEntitiesRouteSearchResponse (..),
  TflApiPresentationEntitiesRouteSectionNaptanEntrySequence (..),
  TflApiPresentationEntitiesRouteSequence (..),
  TflApiPresentationEntitiesSchedule (..),
  TflApiPresentationEntitiesSearchMatch (..),
  TflApiPresentationEntitiesSearchResponse (..),
  TflApiPresentationEntitiesServiceFrequency (..),
  TflApiPresentationEntitiesStationInterval (..),
  TflApiPresentationEntitiesStatusSeverity (..),
  TflApiPresentationEntitiesStopPoint (..),
  TflApiPresentationEntitiesStopPointCategory (..),
  TflApiPresentationEntitiesStopPointRouteSection (..),
  TflApiPresentationEntitiesStopPointSequence (..),
  TflApiPresentationEntitiesStopPointsResponse (..),
  TflApiPresentationEntitiesStreet (..),
  TflApiPresentationEntitiesStreetSegment (..),
  TflApiPresentationEntitiesTimetable (..),
  TflApiPresentationEntitiesTimetableResponse (..),
  TflApiPresentationEntitiesTimetableRoute (..),
  TflApiPresentationEntitiesTimetablesDisambiguation (..),
  TflApiPresentationEntitiesTimetablesDisambiguationOption (..),
  TflApiPresentationEntitiesTrainLoading (..),
  TflApiPresentationEntitiesTwentyFourHourClockTime (..),
  TflApiPresentationEntitiesValidityPeriod (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value (..), FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)


-- | 
data SystemDataSpatialDbGeography = SystemDataSpatialDbGeography
  { systemDataSpatialDbGeographyGeography :: Maybe SystemDataSpatialDbGeographyWellKnownValue -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SystemDataSpatialDbGeography where
  parseJSON = genericParseJSON optionsSystemDataSpatialDbGeography
instance ToJSON SystemDataSpatialDbGeography where
  toJSON = genericToJSON optionsSystemDataSpatialDbGeography
instance ToSchema SystemDataSpatialDbGeography where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsSystemDataSpatialDbGeography

optionsSystemDataSpatialDbGeography :: Options
optionsSystemDataSpatialDbGeography =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("systemDataSpatialDbGeographyGeography", "geography")
      ]


-- | 
data SystemDataSpatialDbGeographyWellKnownValue = SystemDataSpatialDbGeographyWellKnownValue
  { systemDataSpatialDbGeographyWellKnownValueCoordinateSystemId :: Maybe Int -- ^ 
  , systemDataSpatialDbGeographyWellKnownValueWellKnownText :: Maybe Text -- ^ 
  , systemDataSpatialDbGeographyWellKnownValueWellKnownBinary :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SystemDataSpatialDbGeographyWellKnownValue where
  parseJSON = genericParseJSON optionsSystemDataSpatialDbGeographyWellKnownValue
instance ToJSON SystemDataSpatialDbGeographyWellKnownValue where
  toJSON = genericToJSON optionsSystemDataSpatialDbGeographyWellKnownValue
instance ToSchema SystemDataSpatialDbGeographyWellKnownValue where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsSystemDataSpatialDbGeographyWellKnownValue

optionsSystemDataSpatialDbGeographyWellKnownValue :: Options
optionsSystemDataSpatialDbGeographyWellKnownValue =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("systemDataSpatialDbGeographyWellKnownValueCoordinateSystemId", "coordinateSystemId")
      , ("systemDataSpatialDbGeographyWellKnownValueWellKnownText", "wellKnownText")
      , ("systemDataSpatialDbGeographyWellKnownValueWellKnownBinary", "wellKnownBinary")
      ]


-- | 
data TflApiCommonApiVersionInfo = TflApiCommonApiVersionInfo
  { tflApiCommonApiVersionInfoLabel :: Maybe Text -- ^ 
  , tflApiCommonApiVersionInfoTimestamp :: Maybe UTCTime -- ^ 
  , tflApiCommonApiVersionInfoVersion :: Maybe Text -- ^ 
  , tflApiCommonApiVersionInfoAssemblies :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiCommonApiVersionInfo where
  parseJSON = genericParseJSON optionsTflApiCommonApiVersionInfo
instance ToJSON TflApiCommonApiVersionInfo where
  toJSON = genericToJSON optionsTflApiCommonApiVersionInfo
instance ToSchema TflApiCommonApiVersionInfo where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiCommonApiVersionInfo

optionsTflApiCommonApiVersionInfo :: Options
optionsTflApiCommonApiVersionInfo =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiCommonApiVersionInfoLabel", "label")
      , ("tflApiCommonApiVersionInfoTimestamp", "timestamp")
      , ("tflApiCommonApiVersionInfoVersion", "version")
      , ("tflApiCommonApiVersionInfoAssemblies", "assemblies")
      ]


-- | 
data TflApiCommonDateRange = TflApiCommonDateRange
  { tflApiCommonDateRangeStartDate :: Maybe UTCTime -- ^ 
  , tflApiCommonDateRangeEndDate :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiCommonDateRange where
  parseJSON = genericParseJSON optionsTflApiCommonDateRange
instance ToJSON TflApiCommonDateRange where
  toJSON = genericToJSON optionsTflApiCommonDateRange
instance ToSchema TflApiCommonDateRange where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiCommonDateRange

optionsTflApiCommonDateRange :: Options
optionsTflApiCommonDateRange =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiCommonDateRangeStartDate", "startDate")
      , ("tflApiCommonDateRangeEndDate", "endDate")
      ]


-- | 
data TflApiCommonDateRangeNullable = TflApiCommonDateRangeNullable
  { tflApiCommonDateRangeNullableStartDate :: Maybe UTCTime -- ^ 
  , tflApiCommonDateRangeNullableEndDate :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiCommonDateRangeNullable where
  parseJSON = genericParseJSON optionsTflApiCommonDateRangeNullable
instance ToJSON TflApiCommonDateRangeNullable where
  toJSON = genericToJSON optionsTflApiCommonDateRangeNullable
instance ToSchema TflApiCommonDateRangeNullable where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiCommonDateRangeNullable

optionsTflApiCommonDateRangeNullable :: Options
optionsTflApiCommonDateRangeNullable =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiCommonDateRangeNullableStartDate", "startDate")
      , ("tflApiCommonDateRangeNullableEndDate", "endDate")
      ]


-- | 
data TflApiCommonGeoPoint = TflApiCommonGeoPoint
  { tflApiCommonGeoPointLat :: Double -- ^ 
  , tflApiCommonGeoPointLon :: Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiCommonGeoPoint where
  parseJSON = genericParseJSON optionsTflApiCommonGeoPoint
instance ToJSON TflApiCommonGeoPoint where
  toJSON = genericToJSON optionsTflApiCommonGeoPoint
instance ToSchema TflApiCommonGeoPoint where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiCommonGeoPoint

optionsTflApiCommonGeoPoint :: Options
optionsTflApiCommonGeoPoint =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiCommonGeoPointLat", "lat")
      , ("tflApiCommonGeoPointLon", "lon")
      ]


-- | 
data TflApiCommonJourneyPlannerJpElevation = TflApiCommonJourneyPlannerJpElevation
  { tflApiCommonJourneyPlannerJpElevationDistance :: Maybe Int -- ^ 
  , tflApiCommonJourneyPlannerJpElevationStartLat :: Maybe Double -- ^ 
  , tflApiCommonJourneyPlannerJpElevationStartLon :: Maybe Double -- ^ 
  , tflApiCommonJourneyPlannerJpElevationEndLat :: Maybe Double -- ^ 
  , tflApiCommonJourneyPlannerJpElevationEndLon :: Maybe Double -- ^ 
  , tflApiCommonJourneyPlannerJpElevationHeightFromPreviousPoint :: Maybe Int -- ^ 
  , tflApiCommonJourneyPlannerJpElevationGradient :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiCommonJourneyPlannerJpElevation where
  parseJSON = genericParseJSON optionsTflApiCommonJourneyPlannerJpElevation
instance ToJSON TflApiCommonJourneyPlannerJpElevation where
  toJSON = genericToJSON optionsTflApiCommonJourneyPlannerJpElevation
instance ToSchema TflApiCommonJourneyPlannerJpElevation where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiCommonJourneyPlannerJpElevation

optionsTflApiCommonJourneyPlannerJpElevation :: Options
optionsTflApiCommonJourneyPlannerJpElevation =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiCommonJourneyPlannerJpElevationDistance", "distance")
      , ("tflApiCommonJourneyPlannerJpElevationStartLat", "startLat")
      , ("tflApiCommonJourneyPlannerJpElevationStartLon", "startLon")
      , ("tflApiCommonJourneyPlannerJpElevationEndLat", "endLat")
      , ("tflApiCommonJourneyPlannerJpElevationEndLon", "endLon")
      , ("tflApiCommonJourneyPlannerJpElevationHeightFromPreviousPoint", "heightFromPreviousPoint")
      , ("tflApiCommonJourneyPlannerJpElevationGradient", "gradient")
      ]


-- | 
data TflApiCommonPlaceGeo = TflApiCommonPlaceGeo
  { tflApiCommonPlaceGeoSwLat :: Maybe Double -- ^ 
  , tflApiCommonPlaceGeoSwLon :: Maybe Double -- ^ 
  , tflApiCommonPlaceGeoNeLat :: Maybe Double -- ^ 
  , tflApiCommonPlaceGeoNeLon :: Maybe Double -- ^ 
  , tflApiCommonPlaceGeoLat :: Maybe Double -- ^ 
  , tflApiCommonPlaceGeoLon :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiCommonPlaceGeo where
  parseJSON = genericParseJSON optionsTflApiCommonPlaceGeo
instance ToJSON TflApiCommonPlaceGeo where
  toJSON = genericToJSON optionsTflApiCommonPlaceGeo
instance ToSchema TflApiCommonPlaceGeo where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiCommonPlaceGeo

optionsTflApiCommonPlaceGeo :: Options
optionsTflApiCommonPlaceGeo =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiCommonPlaceGeoSwLat", "swLat")
      , ("tflApiCommonPlaceGeoSwLon", "swLon")
      , ("tflApiCommonPlaceGeoNeLat", "neLat")
      , ("tflApiCommonPlaceGeoNeLon", "neLon")
      , ("tflApiCommonPlaceGeoLat", "lat")
      , ("tflApiCommonPlaceGeoLon", "lon")
      ]


-- | 
data TflApiCommonPostcodeInput = TflApiCommonPostcodeInput
  { tflApiCommonPostcodeInputPostcode :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiCommonPostcodeInput where
  parseJSON = genericParseJSON optionsTflApiCommonPostcodeInput
instance ToJSON TflApiCommonPostcodeInput where
  toJSON = genericToJSON optionsTflApiCommonPostcodeInput
instance ToSchema TflApiCommonPostcodeInput where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiCommonPostcodeInput

optionsTflApiCommonPostcodeInput :: Options
optionsTflApiCommonPostcodeInput =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiCommonPostcodeInputPostcode", "postcode")
      ]


-- | 
data TflApiPresentationEntitiesAccidentStatsAccidentDetail = TflApiPresentationEntitiesAccidentStatsAccidentDetail
  { tflApiPresentationEntitiesAccidentStatsAccidentDetailId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesAccidentStatsAccidentDetailLat :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesAccidentStatsAccidentDetailLon :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesAccidentStatsAccidentDetailLocation :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesAccidentStatsAccidentDetailDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesAccidentStatsAccidentDetailSeverity :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesAccidentStatsAccidentDetailBorough :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesAccidentStatsAccidentDetailCasualties :: Maybe [TflApiPresentationEntitiesAccidentStatsCasualty] -- ^ 
  , tflApiPresentationEntitiesAccidentStatsAccidentDetailVehicles :: Maybe [TflApiPresentationEntitiesAccidentStatsVehicle] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesAccidentStatsAccidentDetail where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesAccidentStatsAccidentDetail
instance ToJSON TflApiPresentationEntitiesAccidentStatsAccidentDetail where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesAccidentStatsAccidentDetail
instance ToSchema TflApiPresentationEntitiesAccidentStatsAccidentDetail where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesAccidentStatsAccidentDetail

optionsTflApiPresentationEntitiesAccidentStatsAccidentDetail :: Options
optionsTflApiPresentationEntitiesAccidentStatsAccidentDetail =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesAccidentStatsAccidentDetailId", "id")
      , ("tflApiPresentationEntitiesAccidentStatsAccidentDetailLat", "lat")
      , ("tflApiPresentationEntitiesAccidentStatsAccidentDetailLon", "lon")
      , ("tflApiPresentationEntitiesAccidentStatsAccidentDetailLocation", "location")
      , ("tflApiPresentationEntitiesAccidentStatsAccidentDetailDate", "date")
      , ("tflApiPresentationEntitiesAccidentStatsAccidentDetailSeverity", "severity")
      , ("tflApiPresentationEntitiesAccidentStatsAccidentDetailBorough", "borough")
      , ("tflApiPresentationEntitiesAccidentStatsAccidentDetailCasualties", "casualties")
      , ("tflApiPresentationEntitiesAccidentStatsAccidentDetailVehicles", "vehicles")
      ]


-- | 
data TflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary = TflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary
  { tflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummaryYear :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummaryBorough :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummaryAccidents :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary
instance ToJSON TflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary
instance ToSchema TflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary

optionsTflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary :: Options
optionsTflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummaryYear", "year")
      , ("tflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummaryBorough", "borough")
      , ("tflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummaryAccidents", "accidents")
      ]


-- | 
data TflApiPresentationEntitiesAccidentStatsCasualty = TflApiPresentationEntitiesAccidentStatsCasualty
  { tflApiPresentationEntitiesAccidentStatsCasualtyAge :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesAccidentStatsCasualtyClass :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesAccidentStatsCasualtySeverity :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesAccidentStatsCasualtyMode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesAccidentStatsCasualtyAgeBand :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesAccidentStatsCasualty where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesAccidentStatsCasualty
instance ToJSON TflApiPresentationEntitiesAccidentStatsCasualty where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesAccidentStatsCasualty
instance ToSchema TflApiPresentationEntitiesAccidentStatsCasualty where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesAccidentStatsCasualty

optionsTflApiPresentationEntitiesAccidentStatsCasualty :: Options
optionsTflApiPresentationEntitiesAccidentStatsCasualty =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesAccidentStatsCasualtyAge", "age")
      , ("tflApiPresentationEntitiesAccidentStatsCasualtyClass", "class")
      , ("tflApiPresentationEntitiesAccidentStatsCasualtySeverity", "severity")
      , ("tflApiPresentationEntitiesAccidentStatsCasualtyMode", "mode")
      , ("tflApiPresentationEntitiesAccidentStatsCasualtyAgeBand", "ageBand")
      ]


-- | 
data TflApiPresentationEntitiesAccidentStatsVehicle = TflApiPresentationEntitiesAccidentStatsVehicle
  { tflApiPresentationEntitiesAccidentStatsVehicleType :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesAccidentStatsVehicle where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesAccidentStatsVehicle
instance ToJSON TflApiPresentationEntitiesAccidentStatsVehicle where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesAccidentStatsVehicle
instance ToSchema TflApiPresentationEntitiesAccidentStatsVehicle where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesAccidentStatsVehicle

optionsTflApiPresentationEntitiesAccidentStatsVehicle :: Options
optionsTflApiPresentationEntitiesAccidentStatsVehicle =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesAccidentStatsVehicleType", "type")
      ]


-- | 
data TflApiPresentationEntitiesActiveServiceType = TflApiPresentationEntitiesActiveServiceType
  { tflApiPresentationEntitiesActiveServiceTypeMode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesActiveServiceTypeServiceType :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesActiveServiceType where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesActiveServiceType
instance ToJSON TflApiPresentationEntitiesActiveServiceType where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesActiveServiceType
instance ToSchema TflApiPresentationEntitiesActiveServiceType where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesActiveServiceType

optionsTflApiPresentationEntitiesActiveServiceType :: Options
optionsTflApiPresentationEntitiesActiveServiceType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesActiveServiceTypeMode", "mode")
      , ("tflApiPresentationEntitiesActiveServiceTypeServiceType", "serviceType")
      ]


-- | 
data TflApiPresentationEntitiesAdditionalProperties = TflApiPresentationEntitiesAdditionalProperties
  { tflApiPresentationEntitiesAdditionalPropertiesCategory :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesAdditionalPropertiesKey :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesAdditionalPropertiesSourceSystemKey :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesAdditionalPropertiesValue :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesAdditionalPropertiesModified :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesAdditionalProperties where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesAdditionalProperties
instance ToJSON TflApiPresentationEntitiesAdditionalProperties where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesAdditionalProperties
instance ToSchema TflApiPresentationEntitiesAdditionalProperties where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesAdditionalProperties

optionsTflApiPresentationEntitiesAdditionalProperties :: Options
optionsTflApiPresentationEntitiesAdditionalProperties =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesAdditionalPropertiesCategory", "category")
      , ("tflApiPresentationEntitiesAdditionalPropertiesKey", "key")
      , ("tflApiPresentationEntitiesAdditionalPropertiesSourceSystemKey", "sourceSystemKey")
      , ("tflApiPresentationEntitiesAdditionalPropertiesValue", "value")
      , ("tflApiPresentationEntitiesAdditionalPropertiesModified", "modified")
      ]


-- | DTO to capture the prediction details
data TflApiPresentationEntitiesArrivalDeparture = TflApiPresentationEntitiesArrivalDeparture
  { tflApiPresentationEntitiesArrivalDeparturePlatformName :: Maybe Text -- ^ Platform name (for bus, this is the stop letter)
  , tflApiPresentationEntitiesArrivalDepartureDestinationNaptanId :: Maybe Text -- ^ Naptan Identifier for the prediction's destination
  , tflApiPresentationEntitiesArrivalDepartureDestinationName :: Maybe Text -- ^ Name of the destination
  , tflApiPresentationEntitiesArrivalDepartureNaptanId :: Maybe Text -- ^ Identifier for the prediction
  , tflApiPresentationEntitiesArrivalDepartureStationName :: Maybe Text -- ^ Station name
  , tflApiPresentationEntitiesArrivalDepartureEstimatedTimeOfArrival :: Maybe UTCTime -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureScheduledTimeOfArrival :: Maybe UTCTime -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureEstimatedTimeOfDeparture :: Maybe UTCTime -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureScheduledTimeOfDeparture :: Maybe UTCTime -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureMinutesAndSecondsToArrival :: Maybe Text -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureMinutesAndSecondsToDeparture :: Maybe Text -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureCause :: Maybe Text -- ^ Reason for cancellation or delay
  , tflApiPresentationEntitiesArrivalDepartureDepartureStatus :: Maybe Text -- ^ Status of departure
  , tflApiPresentationEntitiesArrivalDepartureTiming :: Maybe TflApiPresentationEntitiesPredictionTiming -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesArrivalDeparture where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesArrivalDeparture
instance ToJSON TflApiPresentationEntitiesArrivalDeparture where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesArrivalDeparture
instance ToSchema TflApiPresentationEntitiesArrivalDeparture where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesArrivalDeparture

optionsTflApiPresentationEntitiesArrivalDeparture :: Options
optionsTflApiPresentationEntitiesArrivalDeparture =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesArrivalDeparturePlatformName", "platformName")
      , ("tflApiPresentationEntitiesArrivalDepartureDestinationNaptanId", "destinationNaptanId")
      , ("tflApiPresentationEntitiesArrivalDepartureDestinationName", "destinationName")
      , ("tflApiPresentationEntitiesArrivalDepartureNaptanId", "naptanId")
      , ("tflApiPresentationEntitiesArrivalDepartureStationName", "stationName")
      , ("tflApiPresentationEntitiesArrivalDepartureEstimatedTimeOfArrival", "estimatedTimeOfArrival")
      , ("tflApiPresentationEntitiesArrivalDepartureScheduledTimeOfArrival", "scheduledTimeOfArrival")
      , ("tflApiPresentationEntitiesArrivalDepartureEstimatedTimeOfDeparture", "estimatedTimeOfDeparture")
      , ("tflApiPresentationEntitiesArrivalDepartureScheduledTimeOfDeparture", "scheduledTimeOfDeparture")
      , ("tflApiPresentationEntitiesArrivalDepartureMinutesAndSecondsToArrival", "minutesAndSecondsToArrival")
      , ("tflApiPresentationEntitiesArrivalDepartureMinutesAndSecondsToDeparture", "minutesAndSecondsToDeparture")
      , ("tflApiPresentationEntitiesArrivalDepartureCause", "cause")
      , ("tflApiPresentationEntitiesArrivalDepartureDepartureStatus", "departureStatus")
      , ("tflApiPresentationEntitiesArrivalDepartureTiming", "timing")
      ]


-- | DTO to capture the prediction details
data TflApiPresentationEntitiesArrivalDepartureWithLine = TflApiPresentationEntitiesArrivalDepartureWithLine
  { tflApiPresentationEntitiesArrivalDepartureWithLineLineId :: Maybe Text -- ^ Train operating company LineId
  , tflApiPresentationEntitiesArrivalDepartureWithLineLineName :: Maybe Text -- ^ Train operating company LineName
  , tflApiPresentationEntitiesArrivalDepartureWithLineVehicleId :: Maybe Text -- ^ Train operating company VehicleId
  , tflApiPresentationEntitiesArrivalDepartureWithLinePlatformName :: Maybe Text -- ^ Platform name (for bus, this is the stop letter)
  , tflApiPresentationEntitiesArrivalDepartureWithLineDestinationNaptanId :: Maybe Text -- ^ Naptan Identifier for the prediction's destination
  , tflApiPresentationEntitiesArrivalDepartureWithLineDestinationName :: Maybe Text -- ^ Name of the destination
  , tflApiPresentationEntitiesArrivalDepartureWithLineNaptanId :: Maybe Text -- ^ Identifier for the prediction
  , tflApiPresentationEntitiesArrivalDepartureWithLineStationName :: Maybe Text -- ^ Station name
  , tflApiPresentationEntitiesArrivalDepartureWithLineEstimatedTimeOfArrival :: Maybe UTCTime -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureWithLineScheduledTimeOfArrival :: Maybe UTCTime -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureWithLineEstimatedTimeOfDeparture :: Maybe UTCTime -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureWithLineScheduledTimeOfDeparture :: Maybe UTCTime -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureWithLineMinutesAndSecondsToArrival :: Maybe Text -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureWithLineMinutesAndSecondsToDeparture :: Maybe Text -- ^ Estimated time of arrival
  , tflApiPresentationEntitiesArrivalDepartureWithLineCause :: Maybe Text -- ^ Reason for cancellation or delay
  , tflApiPresentationEntitiesArrivalDepartureWithLineDepartureStatus :: Maybe Text -- ^ Status of departure
  , tflApiPresentationEntitiesArrivalDepartureWithLineTiming :: Maybe TflApiPresentationEntitiesPredictionTiming -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesArrivalDepartureWithLine where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesArrivalDepartureWithLine
instance ToJSON TflApiPresentationEntitiesArrivalDepartureWithLine where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesArrivalDepartureWithLine
instance ToSchema TflApiPresentationEntitiesArrivalDepartureWithLine where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesArrivalDepartureWithLine

optionsTflApiPresentationEntitiesArrivalDepartureWithLine :: Options
optionsTflApiPresentationEntitiesArrivalDepartureWithLine =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesArrivalDepartureWithLineLineId", "lineId")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineLineName", "lineName")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineVehicleId", "vehicleId")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLinePlatformName", "platformName")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineDestinationNaptanId", "destinationNaptanId")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineDestinationName", "destinationName")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineNaptanId", "naptanId")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineStationName", "stationName")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineEstimatedTimeOfArrival", "estimatedTimeOfArrival")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineScheduledTimeOfArrival", "scheduledTimeOfArrival")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineEstimatedTimeOfDeparture", "estimatedTimeOfDeparture")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineScheduledTimeOfDeparture", "scheduledTimeOfDeparture")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineMinutesAndSecondsToArrival", "minutesAndSecondsToArrival")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineMinutesAndSecondsToDeparture", "minutesAndSecondsToDeparture")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineCause", "cause")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineDepartureStatus", "departureStatus")
      , ("tflApiPresentationEntitiesArrivalDepartureWithLineTiming", "timing")
      ]


-- | 
data TflApiPresentationEntitiesBay = TflApiPresentationEntitiesBay
  { tflApiPresentationEntitiesBayBayType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesBayBayCount :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesBayFree :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesBayOccupied :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesBay where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesBay
instance ToJSON TflApiPresentationEntitiesBay where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesBay
instance ToSchema TflApiPresentationEntitiesBay where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesBay

optionsTflApiPresentationEntitiesBay :: Options
optionsTflApiPresentationEntitiesBay =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesBayBayType", "bayType")
      , ("tflApiPresentationEntitiesBayBayCount", "bayCount")
      , ("tflApiPresentationEntitiesBayFree", "free")
      , ("tflApiPresentationEntitiesBayOccupied", "occupied")
      ]


-- | Bike point occupancy
data TflApiPresentationEntitiesBikePointOccupancy = TflApiPresentationEntitiesBikePointOccupancy
  { tflApiPresentationEntitiesBikePointOccupancyId :: Maybe Text -- ^ Id of the bike point such as BikePoints_1
  , tflApiPresentationEntitiesBikePointOccupancyName :: Maybe Text -- ^ Name / Common name of the bike point
  , tflApiPresentationEntitiesBikePointOccupancyBikesCount :: Maybe Int -- ^ Total bike counts
  , tflApiPresentationEntitiesBikePointOccupancyEmptyDocks :: Maybe Int -- ^ Empty docks
  , tflApiPresentationEntitiesBikePointOccupancyTotalDocks :: Maybe Int -- ^ Total docks available
  , tflApiPresentationEntitiesBikePointOccupancyStandardBikesCount :: Maybe Int -- ^ Total standard bikes count
  , tflApiPresentationEntitiesBikePointOccupancyEBikesCount :: Maybe Int -- ^ Total ebikes count
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesBikePointOccupancy where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesBikePointOccupancy
instance ToJSON TflApiPresentationEntitiesBikePointOccupancy where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesBikePointOccupancy
instance ToSchema TflApiPresentationEntitiesBikePointOccupancy where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesBikePointOccupancy

optionsTflApiPresentationEntitiesBikePointOccupancy :: Options
optionsTflApiPresentationEntitiesBikePointOccupancy =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesBikePointOccupancyId", "id")
      , ("tflApiPresentationEntitiesBikePointOccupancyName", "name")
      , ("tflApiPresentationEntitiesBikePointOccupancyBikesCount", "bikesCount")
      , ("tflApiPresentationEntitiesBikePointOccupancyEmptyDocks", "emptyDocks")
      , ("tflApiPresentationEntitiesBikePointOccupancyTotalDocks", "totalDocks")
      , ("tflApiPresentationEntitiesBikePointOccupancyStandardBikesCount", "standardBikesCount")
      , ("tflApiPresentationEntitiesBikePointOccupancyEBikesCount", "eBikesCount")
      ]


-- | 
data TflApiPresentationEntitiesCarParkOccupancy = TflApiPresentationEntitiesCarParkOccupancy
  { tflApiPresentationEntitiesCarParkOccupancyId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesCarParkOccupancyBays :: Maybe [TflApiPresentationEntitiesBay] -- ^ 
  , tflApiPresentationEntitiesCarParkOccupancyName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesCarParkOccupancyCarParkDetailsUrl :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesCarParkOccupancy where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesCarParkOccupancy
instance ToJSON TflApiPresentationEntitiesCarParkOccupancy where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesCarParkOccupancy
instance ToSchema TflApiPresentationEntitiesCarParkOccupancy where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesCarParkOccupancy

optionsTflApiPresentationEntitiesCarParkOccupancy :: Options
optionsTflApiPresentationEntitiesCarParkOccupancy =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesCarParkOccupancyId", "id")
      , ("tflApiPresentationEntitiesCarParkOccupancyBays", "bays")
      , ("tflApiPresentationEntitiesCarParkOccupancyName", "name")
      , ("tflApiPresentationEntitiesCarParkOccupancyCarParkDetailsUrl", "carParkDetailsUrl")
      ]


-- | 
data TflApiPresentationEntitiesChargeConnectorOccupancy = TflApiPresentationEntitiesChargeConnectorOccupancy
  { tflApiPresentationEntitiesChargeConnectorOccupancyId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesChargeConnectorOccupancySourceSystemPlaceId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesChargeConnectorOccupancyStatus :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesChargeConnectorOccupancy where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesChargeConnectorOccupancy
instance ToJSON TflApiPresentationEntitiesChargeConnectorOccupancy where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesChargeConnectorOccupancy
instance ToSchema TflApiPresentationEntitiesChargeConnectorOccupancy where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesChargeConnectorOccupancy

optionsTflApiPresentationEntitiesChargeConnectorOccupancy :: Options
optionsTflApiPresentationEntitiesChargeConnectorOccupancy =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesChargeConnectorOccupancyId", "id")
      , ("tflApiPresentationEntitiesChargeConnectorOccupancySourceSystemPlaceId", "sourceSystemPlaceId")
      , ("tflApiPresentationEntitiesChargeConnectorOccupancyStatus", "status")
      ]


-- | 
data TflApiPresentationEntitiesCoordinate = TflApiPresentationEntitiesCoordinate
  { tflApiPresentationEntitiesCoordinateLongitude :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesCoordinateLatitude :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesCoordinateEasting :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesCoordinateNorthing :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesCoordinateXCoord :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesCoordinateYCoord :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesCoordinate where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesCoordinate
instance ToJSON TflApiPresentationEntitiesCoordinate where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesCoordinate
instance ToSchema TflApiPresentationEntitiesCoordinate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesCoordinate

optionsTflApiPresentationEntitiesCoordinate :: Options
optionsTflApiPresentationEntitiesCoordinate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesCoordinateLongitude", "longitude")
      , ("tflApiPresentationEntitiesCoordinateLatitude", "latitude")
      , ("tflApiPresentationEntitiesCoordinateEasting", "easting")
      , ("tflApiPresentationEntitiesCoordinateNorthing", "northing")
      , ("tflApiPresentationEntitiesCoordinateXCoord", "xCoord")
      , ("tflApiPresentationEntitiesCoordinateYCoord", "yCoord")
      ]


-- | 
data TflApiPresentationEntitiesCrowding = TflApiPresentationEntitiesCrowding
  { tflApiPresentationEntitiesCrowdingPassengerFlows :: Maybe [TflApiPresentationEntitiesPassengerFlow] -- ^ Busiest times at a station (static information)
  , tflApiPresentationEntitiesCrowdingTrainLoadings :: Maybe [TflApiPresentationEntitiesTrainLoading] -- ^ Train Loading on a scale 1-6, 1 being \"Very quiet\" and 6 being \"Exceptionally busy\" (static information)
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesCrowding where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesCrowding
instance ToJSON TflApiPresentationEntitiesCrowding where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesCrowding
instance ToSchema TflApiPresentationEntitiesCrowding where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesCrowding

optionsTflApiPresentationEntitiesCrowding :: Options
optionsTflApiPresentationEntitiesCrowding =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesCrowdingPassengerFlows", "passengerFlows")
      , ("tflApiPresentationEntitiesCrowdingTrainLoadings", "trainLoadings")
      ]


-- | 
data TflApiPresentationEntitiesCycleSuperhighway = TflApiPresentationEntitiesCycleSuperhighway
  { tflApiPresentationEntitiesCycleSuperhighwayId :: Maybe Text -- ^ The Id
  , tflApiPresentationEntitiesCycleSuperhighwayLabel :: Maybe Text -- ^ The long label to show on maps when zoomed in
  , tflApiPresentationEntitiesCycleSuperhighwayLabelShort :: Maybe Text -- ^ The short label to show on maps
  , tflApiPresentationEntitiesCycleSuperhighwayGeography :: Maybe SystemDataSpatialDbGeography -- ^ 
  , tflApiPresentationEntitiesCycleSuperhighwaySegmented :: Maybe Bool -- ^ True if the route is split into segments
  , tflApiPresentationEntitiesCycleSuperhighwayModified :: Maybe UTCTime -- ^ When the data was last updated
  , tflApiPresentationEntitiesCycleSuperhighwayStatus :: Maybe Text -- ^ Cycle route status i.e Proposed, Existing etc
  , tflApiPresentationEntitiesCycleSuperhighwayRouteType :: Maybe Text -- ^ Type of cycle route e.g CycleSuperhighways, Quietways, MiniHollands etc
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesCycleSuperhighway where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesCycleSuperhighway
instance ToJSON TflApiPresentationEntitiesCycleSuperhighway where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesCycleSuperhighway
instance ToSchema TflApiPresentationEntitiesCycleSuperhighway where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesCycleSuperhighway

optionsTflApiPresentationEntitiesCycleSuperhighway :: Options
optionsTflApiPresentationEntitiesCycleSuperhighway =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesCycleSuperhighwayId", "id")
      , ("tflApiPresentationEntitiesCycleSuperhighwayLabel", "label")
      , ("tflApiPresentationEntitiesCycleSuperhighwayLabelShort", "labelShort")
      , ("tflApiPresentationEntitiesCycleSuperhighwayGeography", "geography")
      , ("tflApiPresentationEntitiesCycleSuperhighwaySegmented", "segmented")
      , ("tflApiPresentationEntitiesCycleSuperhighwayModified", "modified")
      , ("tflApiPresentationEntitiesCycleSuperhighwayStatus", "status")
      , ("tflApiPresentationEntitiesCycleSuperhighwayRouteType", "routeType")
      ]


-- | 
data TflApiPresentationEntitiesDisruptedPoint = TflApiPresentationEntitiesDisruptedPoint
  { tflApiPresentationEntitiesDisruptedPointAtcoCode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesDisruptedPointFromDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesDisruptedPointToDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesDisruptedPointDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesDisruptedPointCommonName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesDisruptedPointType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesDisruptedPointMode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesDisruptedPointStationAtcoCode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesDisruptedPointAppearance :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesDisruptedPointAdditionalInformation :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesDisruptedPoint where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesDisruptedPoint
instance ToJSON TflApiPresentationEntitiesDisruptedPoint where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesDisruptedPoint
instance ToSchema TflApiPresentationEntitiesDisruptedPoint where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesDisruptedPoint

optionsTflApiPresentationEntitiesDisruptedPoint :: Options
optionsTflApiPresentationEntitiesDisruptedPoint =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesDisruptedPointAtcoCode", "atcoCode")
      , ("tflApiPresentationEntitiesDisruptedPointFromDate", "fromDate")
      , ("tflApiPresentationEntitiesDisruptedPointToDate", "toDate")
      , ("tflApiPresentationEntitiesDisruptedPointDescription", "description")
      , ("tflApiPresentationEntitiesDisruptedPointCommonName", "commonName")
      , ("tflApiPresentationEntitiesDisruptedPointType", "type")
      , ("tflApiPresentationEntitiesDisruptedPointMode", "mode")
      , ("tflApiPresentationEntitiesDisruptedPointStationAtcoCode", "stationAtcoCode")
      , ("tflApiPresentationEntitiesDisruptedPointAppearance", "appearance")
      , ("tflApiPresentationEntitiesDisruptedPointAdditionalInformation", "additionalInformation")
      ]


-- | keep old RouteSection name so as not to break contract
data TflApiPresentationEntitiesDisruptedRoute = TflApiPresentationEntitiesDisruptedRoute
  { tflApiPresentationEntitiesDisruptedRouteId :: Maybe Text -- ^ The Id of the route
  , tflApiPresentationEntitiesDisruptedRouteLineId :: Maybe Text -- ^ The Id of the Line
  , tflApiPresentationEntitiesDisruptedRouteRouteCode :: Maybe Text -- ^ The route code
  , tflApiPresentationEntitiesDisruptedRouteName :: Maybe Text -- ^ Name such as \"72\"
  , tflApiPresentationEntitiesDisruptedRouteLineString :: Maybe Text -- ^ The co-ordinates of the route's path as a geoJSON lineString
  , tflApiPresentationEntitiesDisruptedRouteDirection :: Maybe Text -- ^ Inbound or Outbound
  , tflApiPresentationEntitiesDisruptedRouteOriginationName :: Maybe Text -- ^ The name of the Origin StopPoint
  , tflApiPresentationEntitiesDisruptedRouteDestinationName :: Maybe Text -- ^ The name of the Destination StopPoint
  , tflApiPresentationEntitiesDisruptedRouteVia :: Maybe TflApiPresentationEntitiesRouteSectionNaptanEntrySequence -- ^ 
  , tflApiPresentationEntitiesDisruptedRouteIsEntireRouteSection :: Maybe Bool -- ^ Whether this represents the entire route section
  , tflApiPresentationEntitiesDisruptedRouteValidTo :: Maybe UTCTime -- ^ The DateTime that the Service containing this Route is valid until.
  , tflApiPresentationEntitiesDisruptedRouteValidFrom :: Maybe UTCTime -- ^ The DateTime that the Service containing this Route is valid from.
  , tflApiPresentationEntitiesDisruptedRouteRouteSectionNaptanEntrySequence :: Maybe [TflApiPresentationEntitiesRouteSectionNaptanEntrySequence] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesDisruptedRoute where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesDisruptedRoute
instance ToJSON TflApiPresentationEntitiesDisruptedRoute where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesDisruptedRoute
instance ToSchema TflApiPresentationEntitiesDisruptedRoute where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesDisruptedRoute

optionsTflApiPresentationEntitiesDisruptedRoute :: Options
optionsTflApiPresentationEntitiesDisruptedRoute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesDisruptedRouteId", "id")
      , ("tflApiPresentationEntitiesDisruptedRouteLineId", "lineId")
      , ("tflApiPresentationEntitiesDisruptedRouteRouteCode", "routeCode")
      , ("tflApiPresentationEntitiesDisruptedRouteName", "name")
      , ("tflApiPresentationEntitiesDisruptedRouteLineString", "lineString")
      , ("tflApiPresentationEntitiesDisruptedRouteDirection", "direction")
      , ("tflApiPresentationEntitiesDisruptedRouteOriginationName", "originationName")
      , ("tflApiPresentationEntitiesDisruptedRouteDestinationName", "destinationName")
      , ("tflApiPresentationEntitiesDisruptedRouteVia", "via")
      , ("tflApiPresentationEntitiesDisruptedRouteIsEntireRouteSection", "isEntireRouteSection")
      , ("tflApiPresentationEntitiesDisruptedRouteValidTo", "validTo")
      , ("tflApiPresentationEntitiesDisruptedRouteValidFrom", "validFrom")
      , ("tflApiPresentationEntitiesDisruptedRouteRouteSectionNaptanEntrySequence", "routeSectionNaptanEntrySequence")
      ]


-- | Represents a disruption to a route within the transport network.
data TflApiPresentationEntitiesDisruption = TflApiPresentationEntitiesDisruption
  { tflApiPresentationEntitiesDisruptionCategory :: Maybe Text -- ^ Gets or sets the category of this dispruption.
  , tflApiPresentationEntitiesDisruptionType :: Maybe Text -- ^ Gets or sets the disruption type of this dispruption.
  , tflApiPresentationEntitiesDisruptionCategoryDescription :: Maybe Text -- ^ Gets or sets the description of the category.
  , tflApiPresentationEntitiesDisruptionDescription :: Maybe Text -- ^ Gets or sets the description of this disruption.
  , tflApiPresentationEntitiesDisruptionSummary :: Maybe Text -- ^ Gets or sets the summary of this disruption.
  , tflApiPresentationEntitiesDisruptionAdditionalInfo :: Maybe Text -- ^ Gets or sets the additionaInfo of this disruption.
  , tflApiPresentationEntitiesDisruptionCreated :: Maybe UTCTime -- ^ Gets or sets the date/time when this disruption was created.
  , tflApiPresentationEntitiesDisruptionLastUpdate :: Maybe UTCTime -- ^ Gets or sets the date/time when this disruption was last updated.
  , tflApiPresentationEntitiesDisruptionAffectedRoutes :: Maybe [TflApiPresentationEntitiesDisruptedRoute] -- ^ Gets or sets the routes affected by this disruption
  , tflApiPresentationEntitiesDisruptionAffectedStops :: Maybe [TflApiPresentationEntitiesStopPoint] -- ^ Gets or sets the stops affected by this disruption
  , tflApiPresentationEntitiesDisruptionClosureText :: Maybe Text -- ^ Text describing the closure type
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesDisruption where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesDisruption
instance ToJSON TflApiPresentationEntitiesDisruption where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesDisruption
instance ToSchema TflApiPresentationEntitiesDisruption where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesDisruption

optionsTflApiPresentationEntitiesDisruption :: Options
optionsTflApiPresentationEntitiesDisruption =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesDisruptionCategory", "category")
      , ("tflApiPresentationEntitiesDisruptionType", "type")
      , ("tflApiPresentationEntitiesDisruptionCategoryDescription", "categoryDescription")
      , ("tflApiPresentationEntitiesDisruptionDescription", "description")
      , ("tflApiPresentationEntitiesDisruptionSummary", "summary")
      , ("tflApiPresentationEntitiesDisruptionAdditionalInfo", "additionalInfo")
      , ("tflApiPresentationEntitiesDisruptionCreated", "created")
      , ("tflApiPresentationEntitiesDisruptionLastUpdate", "lastUpdate")
      , ("tflApiPresentationEntitiesDisruptionAffectedRoutes", "affectedRoutes")
      , ("tflApiPresentationEntitiesDisruptionAffectedStops", "affectedStops")
      , ("tflApiPresentationEntitiesDisruptionClosureText", "closureText")
      ]


-- | 
data TflApiPresentationEntitiesFaresFare = TflApiPresentationEntitiesFaresFare
  { tflApiPresentationEntitiesFaresFareId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresFarePassengerType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareValidFrom :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesFaresFareValidUntil :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesFaresFareTicketTime :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareTicketType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareCost :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareCap :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesFaresFareDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareZone :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareMode :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresFare where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresFare
instance ToJSON TflApiPresentationEntitiesFaresFare where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresFare
instance ToSchema TflApiPresentationEntitiesFaresFare where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresFare

optionsTflApiPresentationEntitiesFaresFare :: Options
optionsTflApiPresentationEntitiesFaresFare =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresFareId", "id")
      , ("tflApiPresentationEntitiesFaresFarePassengerType", "passengerType")
      , ("tflApiPresentationEntitiesFaresFareValidFrom", "validFrom")
      , ("tflApiPresentationEntitiesFaresFareValidUntil", "validUntil")
      , ("tflApiPresentationEntitiesFaresFareTicketTime", "ticketTime")
      , ("tflApiPresentationEntitiesFaresFareTicketType", "ticketType")
      , ("tflApiPresentationEntitiesFaresFareCost", "cost")
      , ("tflApiPresentationEntitiesFaresFareCap", "cap")
      , ("tflApiPresentationEntitiesFaresFareDescription", "description")
      , ("tflApiPresentationEntitiesFaresFareZone", "zone")
      , ("tflApiPresentationEntitiesFaresFareMode", "mode")
      ]


-- | 
data TflApiPresentationEntitiesFaresFareBounds = TflApiPresentationEntitiesFaresFareBounds
  { tflApiPresentationEntitiesFaresFareBoundsId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsFrom :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsTo :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsVia :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsRouteCode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsDisplayName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsOperator :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsDisplayOrder :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsIsPopularFare :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsIsPopularTravelCard :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsIsTour :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesFaresFareBoundsMessages :: Maybe [TflApiPresentationEntitiesMessage] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresFareBounds where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresFareBounds
instance ToJSON TflApiPresentationEntitiesFaresFareBounds where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresFareBounds
instance ToSchema TflApiPresentationEntitiesFaresFareBounds where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresFareBounds

optionsTflApiPresentationEntitiesFaresFareBounds :: Options
optionsTflApiPresentationEntitiesFaresFareBounds =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresFareBoundsId", "id")
      , ("tflApiPresentationEntitiesFaresFareBoundsFrom", "from")
      , ("tflApiPresentationEntitiesFaresFareBoundsTo", "to")
      , ("tflApiPresentationEntitiesFaresFareBoundsVia", "via")
      , ("tflApiPresentationEntitiesFaresFareBoundsRouteCode", "routeCode")
      , ("tflApiPresentationEntitiesFaresFareBoundsDescription", "description")
      , ("tflApiPresentationEntitiesFaresFareBoundsDisplayName", "displayName")
      , ("tflApiPresentationEntitiesFaresFareBoundsOperator", "operator")
      , ("tflApiPresentationEntitiesFaresFareBoundsDisplayOrder", "displayOrder")
      , ("tflApiPresentationEntitiesFaresFareBoundsIsPopularFare", "isPopularFare")
      , ("tflApiPresentationEntitiesFaresFareBoundsIsPopularTravelCard", "isPopularTravelCard")
      , ("tflApiPresentationEntitiesFaresFareBoundsIsTour", "isTour")
      , ("tflApiPresentationEntitiesFaresFareBoundsMessages", "messages")
      ]


-- | 
data TflApiPresentationEntitiesFaresFareDetails = TflApiPresentationEntitiesFaresFareDetails
  { tflApiPresentationEntitiesFaresFareDetailsBoundsId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsStartDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsEndDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsMode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsPassengerType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsContactlessPAYGOnlyFare :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsFrom :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsTo :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsFromStation :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsToStation :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsVia :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsRouteCode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsDisplayName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsDisplayOrder :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsRouteDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsValidatorInformation :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsOperator :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsSpecialFare :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsThroughFare :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsIsTour :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsTicketsAvailable :: Maybe [TflApiPresentationEntitiesFaresTicket] -- ^ 
  , tflApiPresentationEntitiesFaresFareDetailsMessages :: Maybe [TflApiPresentationEntitiesMessage] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresFareDetails where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresFareDetails
instance ToJSON TflApiPresentationEntitiesFaresFareDetails where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresFareDetails
instance ToSchema TflApiPresentationEntitiesFaresFareDetails where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresFareDetails

optionsTflApiPresentationEntitiesFaresFareDetails :: Options
optionsTflApiPresentationEntitiesFaresFareDetails =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresFareDetailsBoundsId", "boundsId")
      , ("tflApiPresentationEntitiesFaresFareDetailsStartDate", "startDate")
      , ("tflApiPresentationEntitiesFaresFareDetailsEndDate", "endDate")
      , ("tflApiPresentationEntitiesFaresFareDetailsMode", "mode")
      , ("tflApiPresentationEntitiesFaresFareDetailsPassengerType", "passengerType")
      , ("tflApiPresentationEntitiesFaresFareDetailsContactlessPAYGOnlyFare", "contactlessPAYGOnlyFare")
      , ("tflApiPresentationEntitiesFaresFareDetailsFrom", "from")
      , ("tflApiPresentationEntitiesFaresFareDetailsTo", "to")
      , ("tflApiPresentationEntitiesFaresFareDetailsFromStation", "fromStation")
      , ("tflApiPresentationEntitiesFaresFareDetailsToStation", "toStation")
      , ("tflApiPresentationEntitiesFaresFareDetailsVia", "via")
      , ("tflApiPresentationEntitiesFaresFareDetailsRouteCode", "routeCode")
      , ("tflApiPresentationEntitiesFaresFareDetailsDisplayName", "displayName")
      , ("tflApiPresentationEntitiesFaresFareDetailsDisplayOrder", "displayOrder")
      , ("tflApiPresentationEntitiesFaresFareDetailsRouteDescription", "routeDescription")
      , ("tflApiPresentationEntitiesFaresFareDetailsValidatorInformation", "validatorInformation")
      , ("tflApiPresentationEntitiesFaresFareDetailsOperator", "operator")
      , ("tflApiPresentationEntitiesFaresFareDetailsSpecialFare", "specialFare")
      , ("tflApiPresentationEntitiesFaresFareDetailsThroughFare", "throughFare")
      , ("tflApiPresentationEntitiesFaresFareDetailsIsTour", "isTour")
      , ("tflApiPresentationEntitiesFaresFareDetailsTicketsAvailable", "ticketsAvailable")
      , ("tflApiPresentationEntitiesFaresFareDetailsMessages", "messages")
      ]


-- | 
data TflApiPresentationEntitiesFaresFareStation = TflApiPresentationEntitiesFaresFareStation
  { tflApiPresentationEntitiesFaresFareStationAtcoCode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareStationCommonName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFareStationFareCategory :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresFareStation where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresFareStation
instance ToJSON TflApiPresentationEntitiesFaresFareStation where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresFareStation
instance ToSchema TflApiPresentationEntitiesFaresFareStation where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresFareStation

optionsTflApiPresentationEntitiesFaresFareStation :: Options
optionsTflApiPresentationEntitiesFaresFareStation =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresFareStationAtcoCode", "atcoCode")
      , ("tflApiPresentationEntitiesFaresFareStationCommonName", "commonName")
      , ("tflApiPresentationEntitiesFaresFareStationFareCategory", "fareCategory")
      ]


-- | 
data TflApiPresentationEntitiesFaresFaresMode = TflApiPresentationEntitiesFaresFaresMode
  { tflApiPresentationEntitiesFaresFaresModeId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresFaresModeName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFaresModeDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresFaresMode where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresFaresMode
instance ToJSON TflApiPresentationEntitiesFaresFaresMode where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresFaresMode
instance ToSchema TflApiPresentationEntitiesFaresFaresMode where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresFaresMode

optionsTflApiPresentationEntitiesFaresFaresMode :: Options
optionsTflApiPresentationEntitiesFaresFaresMode =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresFaresModeId", "id")
      , ("tflApiPresentationEntitiesFaresFaresModeName", "name")
      , ("tflApiPresentationEntitiesFaresFaresModeDescription", "description")
      ]


-- | 
data TflApiPresentationEntitiesFaresFaresPeriod = TflApiPresentationEntitiesFaresFaresPeriod
  { tflApiPresentationEntitiesFaresFaresPeriodId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresFaresPeriodStartDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesFaresFaresPeriodViewableDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesFaresFaresPeriodEndDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesFaresFaresPeriodIsFuture :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresFaresPeriod where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresFaresPeriod
instance ToJSON TflApiPresentationEntitiesFaresFaresPeriod where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresFaresPeriod
instance ToSchema TflApiPresentationEntitiesFaresFaresPeriod where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresFaresPeriod

optionsTflApiPresentationEntitiesFaresFaresPeriod :: Options
optionsTflApiPresentationEntitiesFaresFaresPeriod =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresFaresPeriodId", "id")
      , ("tflApiPresentationEntitiesFaresFaresPeriodStartDate", "startDate")
      , ("tflApiPresentationEntitiesFaresFaresPeriodViewableDate", "viewableDate")
      , ("tflApiPresentationEntitiesFaresFaresPeriodEndDate", "endDate")
      , ("tflApiPresentationEntitiesFaresFaresPeriodIsFuture", "isFuture")
      ]


-- | 
data TflApiPresentationEntitiesFaresFaresSection = TflApiPresentationEntitiesFaresFaresSection
  { tflApiPresentationEntitiesFaresFaresSectionHeader :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresFaresSectionIndex :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresFaresSectionJourney :: Maybe TflApiPresentationEntitiesFaresJourney -- ^ 
  , tflApiPresentationEntitiesFaresFaresSectionRows :: Maybe [TflApiPresentationEntitiesFaresFareDetails] -- ^ 
  , tflApiPresentationEntitiesFaresFaresSectionMessages :: Maybe [TflApiPresentationEntitiesMessage] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresFaresSection where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresFaresSection
instance ToJSON TflApiPresentationEntitiesFaresFaresSection where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresFaresSection
instance ToSchema TflApiPresentationEntitiesFaresFaresSection where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresFaresSection

optionsTflApiPresentationEntitiesFaresFaresSection :: Options
optionsTflApiPresentationEntitiesFaresFaresSection =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresFaresSectionHeader", "header")
      , ("tflApiPresentationEntitiesFaresFaresSectionIndex", "index")
      , ("tflApiPresentationEntitiesFaresFaresSectionJourney", "journey")
      , ("tflApiPresentationEntitiesFaresFaresSectionRows", "rows")
      , ("tflApiPresentationEntitiesFaresFaresSectionMessages", "messages")
      ]


-- | 
data TflApiPresentationEntitiesFaresJourney = TflApiPresentationEntitiesFaresJourney
  { tflApiPresentationEntitiesFaresJourneyFromStation :: Maybe TflApiPresentationEntitiesFaresFareStation -- ^ 
  , tflApiPresentationEntitiesFaresJourneyToStation :: Maybe TflApiPresentationEntitiesFaresFareStation -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresJourney where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresJourney
instance ToJSON TflApiPresentationEntitiesFaresJourney where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresJourney
instance ToSchema TflApiPresentationEntitiesFaresJourney where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresJourney

optionsTflApiPresentationEntitiesFaresJourney :: Options
optionsTflApiPresentationEntitiesFaresJourney =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresJourneyFromStation", "fromStation")
      , ("tflApiPresentationEntitiesFaresJourneyToStation", "toStation")
      ]


-- | 
data TflApiPresentationEntitiesFaresPassengerType = TflApiPresentationEntitiesFaresPassengerType
  { tflApiPresentationEntitiesFaresPassengerTypeType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresPassengerTypeDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresPassengerTypeDisplayName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresPassengerTypeDisplayOrder :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresPassengerType where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresPassengerType
instance ToJSON TflApiPresentationEntitiesFaresPassengerType where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresPassengerType
instance ToSchema TflApiPresentationEntitiesFaresPassengerType where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresPassengerType

optionsTflApiPresentationEntitiesFaresPassengerType :: Options
optionsTflApiPresentationEntitiesFaresPassengerType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresPassengerTypeType", "type")
      , ("tflApiPresentationEntitiesFaresPassengerTypeDescription", "description")
      , ("tflApiPresentationEntitiesFaresPassengerTypeDisplayName", "displayName")
      , ("tflApiPresentationEntitiesFaresPassengerTypeDisplayOrder", "displayOrder")
      ]


-- | 
data TflApiPresentationEntitiesFaresRecommendation = TflApiPresentationEntitiesFaresRecommendation
  { tflApiPresentationEntitiesFaresRecommendationId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationRule :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationRank :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationFareType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationProduct :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationTicketType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationTicketTime :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationProductType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationDiscountCard :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationZones :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationCost :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationPriceDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationPriceComparison :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationRecommendedTopUp :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationNotes :: Maybe [TflApiPresentationEntitiesMessage] -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationKeyFeatures :: Maybe [TflApiPresentationEntitiesMessage] -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationGettingYourTicket :: Maybe [TflApiPresentationEntitiesMessage] -- ^ 
  , tflApiPresentationEntitiesFaresRecommendationSingleFare :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresRecommendation where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresRecommendation
instance ToJSON TflApiPresentationEntitiesFaresRecommendation where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresRecommendation
instance ToSchema TflApiPresentationEntitiesFaresRecommendation where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresRecommendation

optionsTflApiPresentationEntitiesFaresRecommendation :: Options
optionsTflApiPresentationEntitiesFaresRecommendation =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresRecommendationId", "id")
      , ("tflApiPresentationEntitiesFaresRecommendationRule", "rule")
      , ("tflApiPresentationEntitiesFaresRecommendationRank", "rank")
      , ("tflApiPresentationEntitiesFaresRecommendationFareType", "fareType")
      , ("tflApiPresentationEntitiesFaresRecommendationProduct", "product")
      , ("tflApiPresentationEntitiesFaresRecommendationTicketType", "ticketType")
      , ("tflApiPresentationEntitiesFaresRecommendationTicketTime", "ticketTime")
      , ("tflApiPresentationEntitiesFaresRecommendationProductType", "productType")
      , ("tflApiPresentationEntitiesFaresRecommendationDiscountCard", "discountCard")
      , ("tflApiPresentationEntitiesFaresRecommendationZones", "zones")
      , ("tflApiPresentationEntitiesFaresRecommendationCost", "cost")
      , ("tflApiPresentationEntitiesFaresRecommendationPriceDescription", "priceDescription")
      , ("tflApiPresentationEntitiesFaresRecommendationPriceComparison", "priceComparison")
      , ("tflApiPresentationEntitiesFaresRecommendationRecommendedTopUp", "recommendedTopUp")
      , ("tflApiPresentationEntitiesFaresRecommendationNotes", "notes")
      , ("tflApiPresentationEntitiesFaresRecommendationKeyFeatures", "keyFeatures")
      , ("tflApiPresentationEntitiesFaresRecommendationGettingYourTicket", "gettingYourTicket")
      , ("tflApiPresentationEntitiesFaresRecommendationSingleFare", "singleFare")
      ]


-- | 
data TflApiPresentationEntitiesFaresRecommendationResponse = TflApiPresentationEntitiesFaresRecommendationResponse
  { tflApiPresentationEntitiesFaresRecommendationResponseRecommendations :: Maybe [TflApiPresentationEntitiesFaresRecommendation] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresRecommendationResponse where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresRecommendationResponse
instance ToJSON TflApiPresentationEntitiesFaresRecommendationResponse where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresRecommendationResponse
instance ToSchema TflApiPresentationEntitiesFaresRecommendationResponse where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresRecommendationResponse

optionsTflApiPresentationEntitiesFaresRecommendationResponse :: Options
optionsTflApiPresentationEntitiesFaresRecommendationResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresRecommendationResponseRecommendations", "recommendations")
      ]


-- | 
data TflApiPresentationEntitiesFaresTicket = TflApiPresentationEntitiesFaresTicket
  { tflApiPresentationEntitiesFaresTicketPassengerType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresTicketTicketType :: Maybe TflApiPresentationEntitiesFaresTicketType -- ^ 
  , tflApiPresentationEntitiesFaresTicketTicketTime :: Maybe TflApiPresentationEntitiesFaresTicketTime -- ^ 
  , tflApiPresentationEntitiesFaresTicketCost :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresTicketDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresTicketMode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresTicketDisplayOrder :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesFaresTicketMessages :: Maybe [TflApiPresentationEntitiesMessage] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresTicket where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresTicket
instance ToJSON TflApiPresentationEntitiesFaresTicket where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresTicket
instance ToSchema TflApiPresentationEntitiesFaresTicket where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresTicket

optionsTflApiPresentationEntitiesFaresTicket :: Options
optionsTflApiPresentationEntitiesFaresTicket =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresTicketPassengerType", "passengerType")
      , ("tflApiPresentationEntitiesFaresTicketTicketType", "ticketType")
      , ("tflApiPresentationEntitiesFaresTicketTicketTime", "ticketTime")
      , ("tflApiPresentationEntitiesFaresTicketCost", "cost")
      , ("tflApiPresentationEntitiesFaresTicketDescription", "description")
      , ("tflApiPresentationEntitiesFaresTicketMode", "mode")
      , ("tflApiPresentationEntitiesFaresTicketDisplayOrder", "displayOrder")
      , ("tflApiPresentationEntitiesFaresTicketMessages", "messages")
      ]


-- | 
data TflApiPresentationEntitiesFaresTicketTime = TflApiPresentationEntitiesFaresTicketTime
  { tflApiPresentationEntitiesFaresTicketTimeType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresTicketTimeDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresTicketTime where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresTicketTime
instance ToJSON TflApiPresentationEntitiesFaresTicketTime where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresTicketTime
instance ToSchema TflApiPresentationEntitiesFaresTicketTime where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresTicketTime

optionsTflApiPresentationEntitiesFaresTicketTime :: Options
optionsTflApiPresentationEntitiesFaresTicketTime =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresTicketTimeType", "type")
      , ("tflApiPresentationEntitiesFaresTicketTimeDescription", "description")
      ]


-- | 
data TflApiPresentationEntitiesFaresTicketType = TflApiPresentationEntitiesFaresTicketType
  { tflApiPresentationEntitiesFaresTicketTypeType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesFaresTicketTypeDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesFaresTicketType where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesFaresTicketType
instance ToJSON TflApiPresentationEntitiesFaresTicketType where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesFaresTicketType
instance ToSchema TflApiPresentationEntitiesFaresTicketType where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesFaresTicketType

optionsTflApiPresentationEntitiesFaresTicketType :: Options
optionsTflApiPresentationEntitiesFaresTicketType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesFaresTicketTypeType", "type")
      , ("tflApiPresentationEntitiesFaresTicketTypeDescription", "description")
      ]


-- | 
data TflApiPresentationEntitiesGeoCodeSearchMatch = TflApiPresentationEntitiesGeoCodeSearchMatch
  { tflApiPresentationEntitiesGeoCodeSearchMatchTypes :: Maybe [Text] -- ^ The type of the place e.g. \"street_address\"
  , tflApiPresentationEntitiesGeoCodeSearchMatchAddress :: Maybe Text -- ^ A string describing the formatted address of the place. Adds additional context to the place's Name.
  , tflApiPresentationEntitiesGeoCodeSearchMatchId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesGeoCodeSearchMatchUrl :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesGeoCodeSearchMatchName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesGeoCodeSearchMatchLat :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesGeoCodeSearchMatchLon :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesGeoCodeSearchMatch where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesGeoCodeSearchMatch
instance ToJSON TflApiPresentationEntitiesGeoCodeSearchMatch where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesGeoCodeSearchMatch
instance ToSchema TflApiPresentationEntitiesGeoCodeSearchMatch where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesGeoCodeSearchMatch

optionsTflApiPresentationEntitiesGeoCodeSearchMatch :: Options
optionsTflApiPresentationEntitiesGeoCodeSearchMatch =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesGeoCodeSearchMatchTypes", "types")
      , ("tflApiPresentationEntitiesGeoCodeSearchMatchAddress", "address")
      , ("tflApiPresentationEntitiesGeoCodeSearchMatchId", "id")
      , ("tflApiPresentationEntitiesGeoCodeSearchMatchUrl", "url")
      , ("tflApiPresentationEntitiesGeoCodeSearchMatchName", "name")
      , ("tflApiPresentationEntitiesGeoCodeSearchMatchLat", "lat")
      , ("tflApiPresentationEntitiesGeoCodeSearchMatchLon", "lon")
      ]


-- | 
data TflApiPresentationEntitiesIdentifier = TflApiPresentationEntitiesIdentifier
  { tflApiPresentationEntitiesIdentifierId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesIdentifierName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesIdentifierUri :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesIdentifierFullName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesIdentifierType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesIdentifierCrowding :: Maybe TflApiPresentationEntitiesCrowding -- ^ 
  , tflApiPresentationEntitiesIdentifierRouteType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesIdentifierStatus :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesIdentifierMotType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesIdentifierNetwork :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesIdentifier where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesIdentifier
instance ToJSON TflApiPresentationEntitiesIdentifier where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesIdentifier
instance ToSchema TflApiPresentationEntitiesIdentifier where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesIdentifier

optionsTflApiPresentationEntitiesIdentifier :: Options
optionsTflApiPresentationEntitiesIdentifier =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesIdentifierId", "id")
      , ("tflApiPresentationEntitiesIdentifierName", "name")
      , ("tflApiPresentationEntitiesIdentifierUri", "uri")
      , ("tflApiPresentationEntitiesIdentifierFullName", "fullName")
      , ("tflApiPresentationEntitiesIdentifierType", "type")
      , ("tflApiPresentationEntitiesIdentifierCrowding", "crowding")
      , ("tflApiPresentationEntitiesIdentifierRouteType", "routeType")
      , ("tflApiPresentationEntitiesIdentifierStatus", "status")
      , ("tflApiPresentationEntitiesIdentifierMotType", "motType")
      , ("tflApiPresentationEntitiesIdentifierNetwork", "network")
      ]


-- | 
data TflApiPresentationEntitiesInstruction = TflApiPresentationEntitiesInstruction
  { tflApiPresentationEntitiesInstructionSummary :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesInstructionDetailed :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesInstructionSteps :: Maybe [TflApiPresentationEntitiesInstructionStep] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesInstruction where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesInstruction
instance ToJSON TflApiPresentationEntitiesInstruction where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesInstruction
instance ToSchema TflApiPresentationEntitiesInstruction where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesInstruction

optionsTflApiPresentationEntitiesInstruction :: Options
optionsTflApiPresentationEntitiesInstruction =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesInstructionSummary", "summary")
      , ("tflApiPresentationEntitiesInstructionDetailed", "detailed")
      , ("tflApiPresentationEntitiesInstructionSteps", "steps")
      ]


-- | 
data TflApiPresentationEntitiesInstructionStep = TflApiPresentationEntitiesInstructionStep
  { tflApiPresentationEntitiesInstructionStepDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesInstructionStepTurnDirection :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesInstructionStepStreetName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesInstructionStepDistance :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesInstructionStepCumulativeDistance :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesInstructionStepSkyDirection :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesInstructionStepSkyDirectionDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesInstructionStepCumulativeTravelTime :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesInstructionStepLatitude :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesInstructionStepLongitude :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesInstructionStepPathAttribute :: Maybe TflApiPresentationEntitiesPathAttribute -- ^ 
  , tflApiPresentationEntitiesInstructionStepDescriptionHeading :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesInstructionStepTrackType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesInstructionStepTravelTime :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesInstructionStepAtcoCode :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesInstructionStep where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesInstructionStep
instance ToJSON TflApiPresentationEntitiesInstructionStep where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesInstructionStep
instance ToSchema TflApiPresentationEntitiesInstructionStep where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesInstructionStep

optionsTflApiPresentationEntitiesInstructionStep :: Options
optionsTflApiPresentationEntitiesInstructionStep =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesInstructionStepDescription", "description")
      , ("tflApiPresentationEntitiesInstructionStepTurnDirection", "turnDirection")
      , ("tflApiPresentationEntitiesInstructionStepStreetName", "streetName")
      , ("tflApiPresentationEntitiesInstructionStepDistance", "distance")
      , ("tflApiPresentationEntitiesInstructionStepCumulativeDistance", "cumulativeDistance")
      , ("tflApiPresentationEntitiesInstructionStepSkyDirection", "skyDirection")
      , ("tflApiPresentationEntitiesInstructionStepSkyDirectionDescription", "skyDirectionDescription")
      , ("tflApiPresentationEntitiesInstructionStepCumulativeTravelTime", "cumulativeTravelTime")
      , ("tflApiPresentationEntitiesInstructionStepLatitude", "latitude")
      , ("tflApiPresentationEntitiesInstructionStepLongitude", "longitude")
      , ("tflApiPresentationEntitiesInstructionStepPathAttribute", "pathAttribute")
      , ("tflApiPresentationEntitiesInstructionStepDescriptionHeading", "descriptionHeading")
      , ("tflApiPresentationEntitiesInstructionStepTrackType", "trackType")
      , ("tflApiPresentationEntitiesInstructionStepTravelTime", "travelTime")
      , ("tflApiPresentationEntitiesInstructionStepAtcoCode", "atcoCode")
      ]


-- | 
data TflApiPresentationEntitiesInterval = TflApiPresentationEntitiesInterval
  { tflApiPresentationEntitiesIntervalStopId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesIntervalTimeToArrival :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesInterval where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesInterval
instance ToJSON TflApiPresentationEntitiesInterval where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesInterval
instance ToSchema TflApiPresentationEntitiesInterval where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesInterval

optionsTflApiPresentationEntitiesInterval :: Options
optionsTflApiPresentationEntitiesInterval =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesIntervalStopId", "stopId")
      , ("tflApiPresentationEntitiesIntervalTimeToArrival", "timeToArrival")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerFare = TflApiPresentationEntitiesJourneyPlannerFare
  { tflApiPresentationEntitiesJourneyPlannerFareLowZone :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareHighZone :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareCost :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareChargeProfileName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareIsHopperFare :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareChargeLevel :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFarePeak :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareOffPeak :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareTaps :: Maybe [TflApiPresentationEntitiesJourneyPlannerFareTap] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerFare where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerFare
instance ToJSON TflApiPresentationEntitiesJourneyPlannerFare where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerFare
instance ToSchema TflApiPresentationEntitiesJourneyPlannerFare where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerFare

optionsTflApiPresentationEntitiesJourneyPlannerFare :: Options
optionsTflApiPresentationEntitiesJourneyPlannerFare =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerFareLowZone", "lowZone")
      , ("tflApiPresentationEntitiesJourneyPlannerFareHighZone", "highZone")
      , ("tflApiPresentationEntitiesJourneyPlannerFareCost", "cost")
      , ("tflApiPresentationEntitiesJourneyPlannerFareChargeProfileName", "chargeProfileName")
      , ("tflApiPresentationEntitiesJourneyPlannerFareIsHopperFare", "isHopperFare")
      , ("tflApiPresentationEntitiesJourneyPlannerFareChargeLevel", "chargeLevel")
      , ("tflApiPresentationEntitiesJourneyPlannerFarePeak", "peak")
      , ("tflApiPresentationEntitiesJourneyPlannerFareOffPeak", "offPeak")
      , ("tflApiPresentationEntitiesJourneyPlannerFareTaps", "taps")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerFareCaveat = TflApiPresentationEntitiesJourneyPlannerFareCaveat
  { tflApiPresentationEntitiesJourneyPlannerFareCaveatText :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareCaveatType :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerFareCaveat where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerFareCaveat
instance ToJSON TflApiPresentationEntitiesJourneyPlannerFareCaveat where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerFareCaveat
instance ToSchema TflApiPresentationEntitiesJourneyPlannerFareCaveat where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerFareCaveat

optionsTflApiPresentationEntitiesJourneyPlannerFareCaveat :: Options
optionsTflApiPresentationEntitiesJourneyPlannerFareCaveat =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerFareCaveatText", "text")
      , ("tflApiPresentationEntitiesJourneyPlannerFareCaveatType", "type")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerFareTap = TflApiPresentationEntitiesJourneyPlannerFareTap
  { tflApiPresentationEntitiesJourneyPlannerFareTapAtcoCode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareTapTapDetails :: Maybe TflApiPresentationEntitiesJourneyPlannerFareTapDetails -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerFareTap where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerFareTap
instance ToJSON TflApiPresentationEntitiesJourneyPlannerFareTap where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerFareTap
instance ToSchema TflApiPresentationEntitiesJourneyPlannerFareTap where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerFareTap

optionsTflApiPresentationEntitiesJourneyPlannerFareTap :: Options
optionsTflApiPresentationEntitiesJourneyPlannerFareTap =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerFareTapAtcoCode", "atcoCode")
      , ("tflApiPresentationEntitiesJourneyPlannerFareTapTapDetails", "tapDetails")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerFareTapDetails = TflApiPresentationEntitiesJourneyPlannerFareTapDetails
  { tflApiPresentationEntitiesJourneyPlannerFareTapDetailsModeType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareTapDetailsValidationType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareTapDetailsHostDeviceType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareTapDetailsBusRouteId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareTapDetailsNationalLocationCode :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerFareTapDetailsTapTimestamp :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerFareTapDetails where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerFareTapDetails
instance ToJSON TflApiPresentationEntitiesJourneyPlannerFareTapDetails where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerFareTapDetails
instance ToSchema TflApiPresentationEntitiesJourneyPlannerFareTapDetails where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerFareTapDetails

optionsTflApiPresentationEntitiesJourneyPlannerFareTapDetails :: Options
optionsTflApiPresentationEntitiesJourneyPlannerFareTapDetails =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerFareTapDetailsModeType", "modeType")
      , ("tflApiPresentationEntitiesJourneyPlannerFareTapDetailsValidationType", "validationType")
      , ("tflApiPresentationEntitiesJourneyPlannerFareTapDetailsHostDeviceType", "hostDeviceType")
      , ("tflApiPresentationEntitiesJourneyPlannerFareTapDetailsBusRouteId", "busRouteId")
      , ("tflApiPresentationEntitiesJourneyPlannerFareTapDetailsNationalLocationCode", "nationalLocationCode")
      , ("tflApiPresentationEntitiesJourneyPlannerFareTapDetailsTapTimestamp", "tapTimestamp")
      ]


-- | A DTO representing a list of possible journeys.
data TflApiPresentationEntitiesJourneyPlannerItineraryResult = TflApiPresentationEntitiesJourneyPlannerItineraryResult
  { tflApiPresentationEntitiesJourneyPlannerItineraryResultJourneys :: Maybe [TflApiPresentationEntitiesJourneyPlannerJourney] -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerItineraryResultLines :: Maybe [TflApiPresentationEntitiesLine] -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerItineraryResultCycleHireDockingStationData :: Maybe TflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerItineraryResultStopMessages :: Maybe [Text] -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerItineraryResultRecommendedMaxAgeMinutes :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerItineraryResultSearchCriteria :: Maybe TflApiPresentationEntitiesJourneyPlannerSearchCriteria -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerItineraryResultJourneyVector :: Maybe TflApiPresentationEntitiesJourneyPlannerJourneyVector -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerItineraryResult where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerItineraryResult
instance ToJSON TflApiPresentationEntitiesJourneyPlannerItineraryResult where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerItineraryResult
instance ToSchema TflApiPresentationEntitiesJourneyPlannerItineraryResult where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerItineraryResult

optionsTflApiPresentationEntitiesJourneyPlannerItineraryResult :: Options
optionsTflApiPresentationEntitiesJourneyPlannerItineraryResult =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerItineraryResultJourneys", "journeys")
      , ("tflApiPresentationEntitiesJourneyPlannerItineraryResultLines", "lines")
      , ("tflApiPresentationEntitiesJourneyPlannerItineraryResultCycleHireDockingStationData", "cycleHireDockingStationData")
      , ("tflApiPresentationEntitiesJourneyPlannerItineraryResultStopMessages", "stopMessages")
      , ("tflApiPresentationEntitiesJourneyPlannerItineraryResultRecommendedMaxAgeMinutes", "recommendedMaxAgeMinutes")
      , ("tflApiPresentationEntitiesJourneyPlannerItineraryResultSearchCriteria", "searchCriteria")
      , ("tflApiPresentationEntitiesJourneyPlannerItineraryResultJourneyVector", "journeyVector")
      ]


-- | Object that represents an end to end journey (see schematic).
data TflApiPresentationEntitiesJourneyPlannerJourney = TflApiPresentationEntitiesJourneyPlannerJourney
  { tflApiPresentationEntitiesJourneyPlannerJourneyStartDateTime :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyDuration :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyArrivalDateTime :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyAlternativeRoute :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyLegs :: Maybe [TflApiPresentationEntitiesJourneyPlannerLeg] -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyFare :: Maybe TflApiPresentationEntitiesJourneyPlannerJourneyFare -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerJourney where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerJourney
instance ToJSON TflApiPresentationEntitiesJourneyPlannerJourney where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerJourney
instance ToSchema TflApiPresentationEntitiesJourneyPlannerJourney where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerJourney

optionsTflApiPresentationEntitiesJourneyPlannerJourney :: Options
optionsTflApiPresentationEntitiesJourneyPlannerJourney =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerJourneyStartDateTime", "startDateTime")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyDuration", "duration")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyArrivalDateTime", "arrivalDateTime")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyDescription", "description")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyAlternativeRoute", "alternativeRoute")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyLegs", "legs")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyFare", "fare")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerJourneyFare = TflApiPresentationEntitiesJourneyPlannerJourneyFare
  { tflApiPresentationEntitiesJourneyPlannerJourneyFareTotalCost :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyFareFares :: Maybe [TflApiPresentationEntitiesJourneyPlannerFare] -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyFareCaveats :: Maybe [TflApiPresentationEntitiesJourneyPlannerFareCaveat] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerJourneyFare where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerJourneyFare
instance ToJSON TflApiPresentationEntitiesJourneyPlannerJourneyFare where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerJourneyFare
instance ToSchema TflApiPresentationEntitiesJourneyPlannerJourneyFare where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerJourneyFare

optionsTflApiPresentationEntitiesJourneyPlannerJourneyFare :: Options
optionsTflApiPresentationEntitiesJourneyPlannerJourneyFare =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerJourneyFareTotalCost", "totalCost")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyFareFares", "fares")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyFareCaveats", "caveats")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData = TflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData
  { tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataOriginNumberOfBikes :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataDestinationNumberOfBikes :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataOriginNumberOfEmptySlots :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataDestinationNumberOfEmptySlots :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataOriginId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataDestinationId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData
instance ToJSON TflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData
instance ToSchema TflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData

optionsTflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData :: Options
optionsTflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataOriginNumberOfBikes", "originNumberOfBikes")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataDestinationNumberOfBikes", "destinationNumberOfBikes")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataOriginNumberOfEmptySlots", "originNumberOfEmptySlots")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataDestinationNumberOfEmptySlots", "destinationNumberOfEmptySlots")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataOriginId", "originId")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataDestinationId", "destinationId")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerJourneyVector = TflApiPresentationEntitiesJourneyPlannerJourneyVector
  { tflApiPresentationEntitiesJourneyPlannerJourneyVectorFrom :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyVectorTo :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyVectorVia :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerJourneyVectorUri :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerJourneyVector where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerJourneyVector
instance ToJSON TflApiPresentationEntitiesJourneyPlannerJourneyVector where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerJourneyVector
instance ToSchema TflApiPresentationEntitiesJourneyPlannerJourneyVector where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerJourneyVector

optionsTflApiPresentationEntitiesJourneyPlannerJourneyVector :: Options
optionsTflApiPresentationEntitiesJourneyPlannerJourneyVector =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerJourneyVectorFrom", "from")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyVectorTo", "to")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyVectorVia", "via")
      , ("tflApiPresentationEntitiesJourneyPlannerJourneyVectorUri", "uri")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerLeg = TflApiPresentationEntitiesJourneyPlannerLeg
  { tflApiPresentationEntitiesJourneyPlannerLegDuration :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegSpeed :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegInstruction :: Maybe TflApiPresentationEntitiesInstruction -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegObstacles :: Maybe [TflApiPresentationEntitiesJourneyPlannerObstacle] -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegDepartureTime :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegArrivalTime :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegDeparturePoint :: Maybe TflApiPresentationEntitiesPoint -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegArrivalPoint :: Maybe TflApiPresentationEntitiesPoint -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegPath :: Maybe TflApiPresentationEntitiesJourneyPlannerPath -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegRouteOptions :: Maybe [TflApiPresentationEntitiesJourneyPlannerRouteOption] -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegMode :: Maybe TflApiPresentationEntitiesIdentifier -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegDisruptions :: Maybe [TflApiPresentationEntitiesDisruption] -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegPlannedWorks :: Maybe [TflApiPresentationEntitiesJourneyPlannerPlannedWork] -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegDistance :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegIsDisrupted :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegHasFixedLocations :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegScheduledDepartureTime :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegScheduledArrivalTime :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegInterChangeDuration :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerLegInterChangePosition :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerLeg where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerLeg
instance ToJSON TflApiPresentationEntitiesJourneyPlannerLeg where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerLeg
instance ToSchema TflApiPresentationEntitiesJourneyPlannerLeg where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerLeg

optionsTflApiPresentationEntitiesJourneyPlannerLeg :: Options
optionsTflApiPresentationEntitiesJourneyPlannerLeg =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerLegDuration", "duration")
      , ("tflApiPresentationEntitiesJourneyPlannerLegSpeed", "speed")
      , ("tflApiPresentationEntitiesJourneyPlannerLegInstruction", "instruction")
      , ("tflApiPresentationEntitiesJourneyPlannerLegObstacles", "obstacles")
      , ("tflApiPresentationEntitiesJourneyPlannerLegDepartureTime", "departureTime")
      , ("tflApiPresentationEntitiesJourneyPlannerLegArrivalTime", "arrivalTime")
      , ("tflApiPresentationEntitiesJourneyPlannerLegDeparturePoint", "departurePoint")
      , ("tflApiPresentationEntitiesJourneyPlannerLegArrivalPoint", "arrivalPoint")
      , ("tflApiPresentationEntitiesJourneyPlannerLegPath", "path")
      , ("tflApiPresentationEntitiesJourneyPlannerLegRouteOptions", "routeOptions")
      , ("tflApiPresentationEntitiesJourneyPlannerLegMode", "mode")
      , ("tflApiPresentationEntitiesJourneyPlannerLegDisruptions", "disruptions")
      , ("tflApiPresentationEntitiesJourneyPlannerLegPlannedWorks", "plannedWorks")
      , ("tflApiPresentationEntitiesJourneyPlannerLegDistance", "distance")
      , ("tflApiPresentationEntitiesJourneyPlannerLegIsDisrupted", "isDisrupted")
      , ("tflApiPresentationEntitiesJourneyPlannerLegHasFixedLocations", "hasFixedLocations")
      , ("tflApiPresentationEntitiesJourneyPlannerLegScheduledDepartureTime", "scheduledDepartureTime")
      , ("tflApiPresentationEntitiesJourneyPlannerLegScheduledArrivalTime", "scheduledArrivalTime")
      , ("tflApiPresentationEntitiesJourneyPlannerLegInterChangeDuration", "interChangeDuration")
      , ("tflApiPresentationEntitiesJourneyPlannerLegInterChangePosition", "interChangePosition")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerObstacle = TflApiPresentationEntitiesJourneyPlannerObstacle
  { tflApiPresentationEntitiesJourneyPlannerObstacleType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerObstacleIncline :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerObstacleStopId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerObstaclePosition :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerObstacle where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerObstacle
instance ToJSON TflApiPresentationEntitiesJourneyPlannerObstacle where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerObstacle
instance ToSchema TflApiPresentationEntitiesJourneyPlannerObstacle where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerObstacle

optionsTflApiPresentationEntitiesJourneyPlannerObstacle :: Options
optionsTflApiPresentationEntitiesJourneyPlannerObstacle =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerObstacleType", "type")
      , ("tflApiPresentationEntitiesJourneyPlannerObstacleIncline", "incline")
      , ("tflApiPresentationEntitiesJourneyPlannerObstacleStopId", "stopId")
      , ("tflApiPresentationEntitiesJourneyPlannerObstaclePosition", "position")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerPath = TflApiPresentationEntitiesJourneyPlannerPath
  { tflApiPresentationEntitiesJourneyPlannerPathLineString :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerPathStopPoints :: Maybe [TflApiPresentationEntitiesIdentifier] -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerPathElevation :: Maybe [TflApiCommonJourneyPlannerJpElevation] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerPath where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerPath
instance ToJSON TflApiPresentationEntitiesJourneyPlannerPath where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerPath
instance ToSchema TflApiPresentationEntitiesJourneyPlannerPath where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerPath

optionsTflApiPresentationEntitiesJourneyPlannerPath :: Options
optionsTflApiPresentationEntitiesJourneyPlannerPath =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerPathLineString", "lineString")
      , ("tflApiPresentationEntitiesJourneyPlannerPathStopPoints", "stopPoints")
      , ("tflApiPresentationEntitiesJourneyPlannerPathElevation", "elevation")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerPlannedWork = TflApiPresentationEntitiesJourneyPlannerPlannedWork
  { tflApiPresentationEntitiesJourneyPlannerPlannedWorkId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerPlannedWorkDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerPlannedWorkCreatedDateTime :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerPlannedWorkLastUpdateDateTime :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerPlannedWork where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerPlannedWork
instance ToJSON TflApiPresentationEntitiesJourneyPlannerPlannedWork where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerPlannedWork
instance ToSchema TflApiPresentationEntitiesJourneyPlannerPlannedWork where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerPlannedWork

optionsTflApiPresentationEntitiesJourneyPlannerPlannedWork :: Options
optionsTflApiPresentationEntitiesJourneyPlannerPlannedWork =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerPlannedWorkId", "id")
      , ("tflApiPresentationEntitiesJourneyPlannerPlannedWorkDescription", "description")
      , ("tflApiPresentationEntitiesJourneyPlannerPlannedWorkCreatedDateTime", "createdDateTime")
      , ("tflApiPresentationEntitiesJourneyPlannerPlannedWorkLastUpdateDateTime", "lastUpdateDateTime")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerRouteOption = TflApiPresentationEntitiesJourneyPlannerRouteOption
  { tflApiPresentationEntitiesJourneyPlannerRouteOptionId :: Maybe Text -- ^ The Id of the route
  , tflApiPresentationEntitiesJourneyPlannerRouteOptionName :: Maybe Text -- ^ Name such as \"72\"
  , tflApiPresentationEntitiesJourneyPlannerRouteOptionDirections :: Maybe [Text] -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerRouteOptionLineIdentifier :: Maybe TflApiPresentationEntitiesIdentifier -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerRouteOptionDirection :: Maybe Text -- ^ The direction of the route, i.e. outbound or inbound.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerRouteOption where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerRouteOption
instance ToJSON TflApiPresentationEntitiesJourneyPlannerRouteOption where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerRouteOption
instance ToSchema TflApiPresentationEntitiesJourneyPlannerRouteOption where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerRouteOption

optionsTflApiPresentationEntitiesJourneyPlannerRouteOption :: Options
optionsTflApiPresentationEntitiesJourneyPlannerRouteOption =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerRouteOptionId", "id")
      , ("tflApiPresentationEntitiesJourneyPlannerRouteOptionName", "name")
      , ("tflApiPresentationEntitiesJourneyPlannerRouteOptionDirections", "directions")
      , ("tflApiPresentationEntitiesJourneyPlannerRouteOptionLineIdentifier", "lineIdentifier")
      , ("tflApiPresentationEntitiesJourneyPlannerRouteOptionDirection", "direction")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerSearchCriteria = TflApiPresentationEntitiesJourneyPlannerSearchCriteria
  { tflApiPresentationEntitiesJourneyPlannerSearchCriteriaDateTime :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerSearchCriteriaDateTimeType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerSearchCriteriaTimeAdjustments :: Maybe TflApiPresentationEntitiesJourneyPlannerTimeAdjustments -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerSearchCriteria where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerSearchCriteria
instance ToJSON TflApiPresentationEntitiesJourneyPlannerSearchCriteria where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerSearchCriteria
instance ToSchema TflApiPresentationEntitiesJourneyPlannerSearchCriteria where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerSearchCriteria

optionsTflApiPresentationEntitiesJourneyPlannerSearchCriteria :: Options
optionsTflApiPresentationEntitiesJourneyPlannerSearchCriteria =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerSearchCriteriaDateTime", "dateTime")
      , ("tflApiPresentationEntitiesJourneyPlannerSearchCriteriaDateTimeType", "dateTimeType")
      , ("tflApiPresentationEntitiesJourneyPlannerSearchCriteriaTimeAdjustments", "timeAdjustments")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerTimeAdjustment = TflApiPresentationEntitiesJourneyPlannerTimeAdjustment
  { tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentDate :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentTime :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentTimeIs :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentUri :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerTimeAdjustment where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerTimeAdjustment
instance ToJSON TflApiPresentationEntitiesJourneyPlannerTimeAdjustment where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerTimeAdjustment
instance ToSchema TflApiPresentationEntitiesJourneyPlannerTimeAdjustment where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerTimeAdjustment

optionsTflApiPresentationEntitiesJourneyPlannerTimeAdjustment :: Options
optionsTflApiPresentationEntitiesJourneyPlannerTimeAdjustment =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentDate", "date")
      , ("tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentTime", "time")
      , ("tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentTimeIs", "timeIs")
      , ("tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentUri", "uri")
      ]


-- | 
data TflApiPresentationEntitiesJourneyPlannerTimeAdjustments = TflApiPresentationEntitiesJourneyPlannerTimeAdjustments
  { tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsEarliest :: Maybe TflApiPresentationEntitiesJourneyPlannerTimeAdjustment -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsEarlier :: Maybe TflApiPresentationEntitiesJourneyPlannerTimeAdjustment -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsLater :: Maybe TflApiPresentationEntitiesJourneyPlannerTimeAdjustment -- ^ 
  , tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsLatest :: Maybe TflApiPresentationEntitiesJourneyPlannerTimeAdjustment -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesJourneyPlannerTimeAdjustments where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesJourneyPlannerTimeAdjustments
instance ToJSON TflApiPresentationEntitiesJourneyPlannerTimeAdjustments where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesJourneyPlannerTimeAdjustments
instance ToSchema TflApiPresentationEntitiesJourneyPlannerTimeAdjustments where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesJourneyPlannerTimeAdjustments

optionsTflApiPresentationEntitiesJourneyPlannerTimeAdjustments :: Options
optionsTflApiPresentationEntitiesJourneyPlannerTimeAdjustments =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsEarliest", "earliest")
      , ("tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsEarlier", "earlier")
      , ("tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsLater", "later")
      , ("tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsLatest", "latest")
      ]


-- | 
data TflApiPresentationEntitiesKnownJourney = TflApiPresentationEntitiesKnownJourney
  { tflApiPresentationEntitiesKnownJourneyHour :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesKnownJourneyMinute :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesKnownJourneyIntervalId :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesKnownJourney where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesKnownJourney
instance ToJSON TflApiPresentationEntitiesKnownJourney where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesKnownJourney
instance ToSchema TflApiPresentationEntitiesKnownJourney where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesKnownJourney

optionsTflApiPresentationEntitiesKnownJourney :: Options
optionsTflApiPresentationEntitiesKnownJourney =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesKnownJourneyHour", "hour")
      , ("tflApiPresentationEntitiesKnownJourneyMinute", "minute")
      , ("tflApiPresentationEntitiesKnownJourneyIntervalId", "intervalId")
      ]


-- | 
data TflApiPresentationEntitiesLine = TflApiPresentationEntitiesLine
  { tflApiPresentationEntitiesLineId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineModeName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineDisruptions :: Maybe [TflApiPresentationEntitiesDisruption] -- ^ 
  , tflApiPresentationEntitiesLineCreated :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesLineModified :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesLineLineStatuses :: Maybe [TflApiPresentationEntitiesLineStatus] -- ^ 
  , tflApiPresentationEntitiesLineRouteSections :: Maybe [TflApiPresentationEntitiesMatchedRoute] -- ^ 
  , tflApiPresentationEntitiesLineServiceTypes :: Maybe [TflApiPresentationEntitiesLineServiceTypeInfo] -- ^ 
  , tflApiPresentationEntitiesLineCrowding :: Maybe TflApiPresentationEntitiesCrowding -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesLine where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesLine
instance ToJSON TflApiPresentationEntitiesLine where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesLine
instance ToSchema TflApiPresentationEntitiesLine where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesLine

optionsTflApiPresentationEntitiesLine :: Options
optionsTflApiPresentationEntitiesLine =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesLineId", "id")
      , ("tflApiPresentationEntitiesLineName", "name")
      , ("tflApiPresentationEntitiesLineModeName", "modeName")
      , ("tflApiPresentationEntitiesLineDisruptions", "disruptions")
      , ("tflApiPresentationEntitiesLineCreated", "created")
      , ("tflApiPresentationEntitiesLineModified", "modified")
      , ("tflApiPresentationEntitiesLineLineStatuses", "lineStatuses")
      , ("tflApiPresentationEntitiesLineRouteSections", "routeSections")
      , ("tflApiPresentationEntitiesLineServiceTypes", "serviceTypes")
      , ("tflApiPresentationEntitiesLineCrowding", "crowding")
      ]


-- | 
data TflApiPresentationEntitiesLineGroup = TflApiPresentationEntitiesLineGroup
  { tflApiPresentationEntitiesLineGroupNaptanIdReference :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineGroupStationAtcoCode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineGroupLineIdentifier :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesLineGroup where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesLineGroup
instance ToJSON TflApiPresentationEntitiesLineGroup where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesLineGroup
instance ToSchema TflApiPresentationEntitiesLineGroup where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesLineGroup

optionsTflApiPresentationEntitiesLineGroup :: Options
optionsTflApiPresentationEntitiesLineGroup =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesLineGroupNaptanIdReference", "naptanIdReference")
      , ("tflApiPresentationEntitiesLineGroupStationAtcoCode", "stationAtcoCode")
      , ("tflApiPresentationEntitiesLineGroupLineIdentifier", "lineIdentifier")
      ]


-- | 
data TflApiPresentationEntitiesLineModeGroup = TflApiPresentationEntitiesLineModeGroup
  { tflApiPresentationEntitiesLineModeGroupModeName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineModeGroupLineIdentifier :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesLineModeGroup where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesLineModeGroup
instance ToJSON TflApiPresentationEntitiesLineModeGroup where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesLineModeGroup
instance ToSchema TflApiPresentationEntitiesLineModeGroup where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesLineModeGroup

optionsTflApiPresentationEntitiesLineModeGroup :: Options
optionsTflApiPresentationEntitiesLineModeGroup =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesLineModeGroupModeName", "modeName")
      , ("tflApiPresentationEntitiesLineModeGroupLineIdentifier", "lineIdentifier")
      ]


-- | 
data TflApiPresentationEntitiesLineRouteSection = TflApiPresentationEntitiesLineRouteSection
  { tflApiPresentationEntitiesLineRouteSectionRouteId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesLineRouteSectionDirection :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineRouteSectionDestination :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineRouteSectionFromStation :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineRouteSectionToStation :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineRouteSectionServiceType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineRouteSectionVehicleDestinationText :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesLineRouteSection where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesLineRouteSection
instance ToJSON TflApiPresentationEntitiesLineRouteSection where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesLineRouteSection
instance ToSchema TflApiPresentationEntitiesLineRouteSection where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesLineRouteSection

optionsTflApiPresentationEntitiesLineRouteSection :: Options
optionsTflApiPresentationEntitiesLineRouteSection =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesLineRouteSectionRouteId", "routeId")
      , ("tflApiPresentationEntitiesLineRouteSectionDirection", "direction")
      , ("tflApiPresentationEntitiesLineRouteSectionDestination", "destination")
      , ("tflApiPresentationEntitiesLineRouteSectionFromStation", "fromStation")
      , ("tflApiPresentationEntitiesLineRouteSectionToStation", "toStation")
      , ("tflApiPresentationEntitiesLineRouteSectionServiceType", "serviceType")
      , ("tflApiPresentationEntitiesLineRouteSectionVehicleDestinationText", "vehicleDestinationText")
      ]


-- | 
data TflApiPresentationEntitiesLineServiceType = TflApiPresentationEntitiesLineServiceType
  { tflApiPresentationEntitiesLineServiceTypeLineName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineServiceTypeLineSpecificServiceTypes :: Maybe [TflApiPresentationEntitiesLineSpecificServiceType] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesLineServiceType where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesLineServiceType
instance ToJSON TflApiPresentationEntitiesLineServiceType where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesLineServiceType
instance ToSchema TflApiPresentationEntitiesLineServiceType where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesLineServiceType

optionsTflApiPresentationEntitiesLineServiceType :: Options
optionsTflApiPresentationEntitiesLineServiceType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesLineServiceTypeLineName", "lineName")
      , ("tflApiPresentationEntitiesLineServiceTypeLineSpecificServiceTypes", "lineSpecificServiceTypes")
      ]


-- | 
data TflApiPresentationEntitiesLineServiceTypeInfo = TflApiPresentationEntitiesLineServiceTypeInfo
  { tflApiPresentationEntitiesLineServiceTypeInfoName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineServiceTypeInfoUri :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesLineServiceTypeInfo where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesLineServiceTypeInfo
instance ToJSON TflApiPresentationEntitiesLineServiceTypeInfo where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesLineServiceTypeInfo
instance ToSchema TflApiPresentationEntitiesLineServiceTypeInfo where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesLineServiceTypeInfo

optionsTflApiPresentationEntitiesLineServiceTypeInfo :: Options
optionsTflApiPresentationEntitiesLineServiceTypeInfo =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesLineServiceTypeInfoName", "name")
      , ("tflApiPresentationEntitiesLineServiceTypeInfoUri", "uri")
      ]


-- | 
data TflApiPresentationEntitiesLineSpecificServiceType = TflApiPresentationEntitiesLineSpecificServiceType
  { tflApiPresentationEntitiesLineSpecificServiceTypeServiceType :: Maybe TflApiPresentationEntitiesLineServiceTypeInfo -- ^ 
  , tflApiPresentationEntitiesLineSpecificServiceTypeStopServesServiceType :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesLineSpecificServiceType where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesLineSpecificServiceType
instance ToJSON TflApiPresentationEntitiesLineSpecificServiceType where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesLineSpecificServiceType
instance ToSchema TflApiPresentationEntitiesLineSpecificServiceType where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesLineSpecificServiceType

optionsTflApiPresentationEntitiesLineSpecificServiceType :: Options
optionsTflApiPresentationEntitiesLineSpecificServiceType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesLineSpecificServiceTypeServiceType", "serviceType")
      , ("tflApiPresentationEntitiesLineSpecificServiceTypeStopServesServiceType", "stopServesServiceType")
      ]


-- | 
data TflApiPresentationEntitiesLineStatus = TflApiPresentationEntitiesLineStatus
  { tflApiPresentationEntitiesLineStatusId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesLineStatusLineId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineStatusStatusSeverity :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesLineStatusStatusSeverityDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineStatusReason :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesLineStatusCreated :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesLineStatusModified :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesLineStatusValidityPeriods :: Maybe [TflApiPresentationEntitiesValidityPeriod] -- ^ 
  , tflApiPresentationEntitiesLineStatusDisruption :: Maybe TflApiPresentationEntitiesDisruption -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesLineStatus where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesLineStatus
instance ToJSON TflApiPresentationEntitiesLineStatus where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesLineStatus
instance ToSchema TflApiPresentationEntitiesLineStatus where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesLineStatus

optionsTflApiPresentationEntitiesLineStatus :: Options
optionsTflApiPresentationEntitiesLineStatus =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesLineStatusId", "id")
      , ("tflApiPresentationEntitiesLineStatusLineId", "lineId")
      , ("tflApiPresentationEntitiesLineStatusStatusSeverity", "statusSeverity")
      , ("tflApiPresentationEntitiesLineStatusStatusSeverityDescription", "statusSeverityDescription")
      , ("tflApiPresentationEntitiesLineStatusReason", "reason")
      , ("tflApiPresentationEntitiesLineStatusCreated", "created")
      , ("tflApiPresentationEntitiesLineStatusModified", "modified")
      , ("tflApiPresentationEntitiesLineStatusValidityPeriods", "validityPeriods")
      , ("tflApiPresentationEntitiesLineStatusDisruption", "disruption")
      ]


-- | Description of a Route used in Route search results.
data TflApiPresentationEntitiesMatchedRoute = TflApiPresentationEntitiesMatchedRoute
  { tflApiPresentationEntitiesMatchedRouteRouteCode :: Maybe Text -- ^ The route code
  , tflApiPresentationEntitiesMatchedRouteName :: Maybe Text -- ^ Name such as \"72\"
  , tflApiPresentationEntitiesMatchedRouteDirection :: Maybe Text -- ^ Inbound or Outbound
  , tflApiPresentationEntitiesMatchedRouteOriginationName :: Maybe Text -- ^ The name of the Origin StopPoint
  , tflApiPresentationEntitiesMatchedRouteDestinationName :: Maybe Text -- ^ The name of the Destination StopPoint
  , tflApiPresentationEntitiesMatchedRouteOriginator :: Maybe Text -- ^ The Id (NaPTAN code) of the Origin StopPoint
  , tflApiPresentationEntitiesMatchedRouteDestination :: Maybe Text -- ^ The Id (NaPTAN code) or the Destination StopPoint
  , tflApiPresentationEntitiesMatchedRouteServiceType :: Maybe Text -- ^ Regular or Night
  , tflApiPresentationEntitiesMatchedRouteValidTo :: Maybe UTCTime -- ^ The DateTime that the Service containing this Route is valid until.
  , tflApiPresentationEntitiesMatchedRouteValidFrom :: Maybe UTCTime -- ^ The DateTime that the Service containing this Route is valid from.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesMatchedRoute where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesMatchedRoute
instance ToJSON TflApiPresentationEntitiesMatchedRoute where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesMatchedRoute
instance ToSchema TflApiPresentationEntitiesMatchedRoute where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesMatchedRoute

optionsTflApiPresentationEntitiesMatchedRoute :: Options
optionsTflApiPresentationEntitiesMatchedRoute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesMatchedRouteRouteCode", "routeCode")
      , ("tflApiPresentationEntitiesMatchedRouteName", "name")
      , ("tflApiPresentationEntitiesMatchedRouteDirection", "direction")
      , ("tflApiPresentationEntitiesMatchedRouteOriginationName", "originationName")
      , ("tflApiPresentationEntitiesMatchedRouteDestinationName", "destinationName")
      , ("tflApiPresentationEntitiesMatchedRouteOriginator", "originator")
      , ("tflApiPresentationEntitiesMatchedRouteDestination", "destination")
      , ("tflApiPresentationEntitiesMatchedRouteServiceType", "serviceType")
      , ("tflApiPresentationEntitiesMatchedRouteValidTo", "validTo")
      , ("tflApiPresentationEntitiesMatchedRouteValidFrom", "validFrom")
      ]


-- | 
data TflApiPresentationEntitiesMatchedRouteSections = TflApiPresentationEntitiesMatchedRouteSections
  { tflApiPresentationEntitiesMatchedRouteSectionsId :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesMatchedRouteSections where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesMatchedRouteSections
instance ToJSON TflApiPresentationEntitiesMatchedRouteSections where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesMatchedRouteSections
instance ToSchema TflApiPresentationEntitiesMatchedRouteSections where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesMatchedRouteSections

optionsTflApiPresentationEntitiesMatchedRouteSections :: Options
optionsTflApiPresentationEntitiesMatchedRouteSections =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesMatchedRouteSectionsId", "id")
      ]


-- | 
data TflApiPresentationEntitiesMatchedStop = TflApiPresentationEntitiesMatchedStop
  { tflApiPresentationEntitiesMatchedStopRouteId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesMatchedStopParentId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopStationId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopIcsId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopTopMostParentId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopDirection :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopTowards :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopModes :: Maybe [Text] -- ^ 
  , tflApiPresentationEntitiesMatchedStopStopType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopStopLetter :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopZone :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopAccessibilitySummary :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopHasDisruption :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesMatchedStopLines :: Maybe [TflApiPresentationEntitiesIdentifier] -- ^ 
  , tflApiPresentationEntitiesMatchedStopStatus :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesMatchedStopId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopUrl :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMatchedStopLat :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesMatchedStopLon :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesMatchedStop where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesMatchedStop
instance ToJSON TflApiPresentationEntitiesMatchedStop where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesMatchedStop
instance ToSchema TflApiPresentationEntitiesMatchedStop where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesMatchedStop

optionsTflApiPresentationEntitiesMatchedStop :: Options
optionsTflApiPresentationEntitiesMatchedStop =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesMatchedStopRouteId", "routeId")
      , ("tflApiPresentationEntitiesMatchedStopParentId", "parentId")
      , ("tflApiPresentationEntitiesMatchedStopStationId", "stationId")
      , ("tflApiPresentationEntitiesMatchedStopIcsId", "icsId")
      , ("tflApiPresentationEntitiesMatchedStopTopMostParentId", "topMostParentId")
      , ("tflApiPresentationEntitiesMatchedStopDirection", "direction")
      , ("tflApiPresentationEntitiesMatchedStopTowards", "towards")
      , ("tflApiPresentationEntitiesMatchedStopModes", "modes")
      , ("tflApiPresentationEntitiesMatchedStopStopType", "stopType")
      , ("tflApiPresentationEntitiesMatchedStopStopLetter", "stopLetter")
      , ("tflApiPresentationEntitiesMatchedStopZone", "zone")
      , ("tflApiPresentationEntitiesMatchedStopAccessibilitySummary", "accessibilitySummary")
      , ("tflApiPresentationEntitiesMatchedStopHasDisruption", "hasDisruption")
      , ("tflApiPresentationEntitiesMatchedStopLines", "lines")
      , ("tflApiPresentationEntitiesMatchedStopStatus", "status")
      , ("tflApiPresentationEntitiesMatchedStopId", "id")
      , ("tflApiPresentationEntitiesMatchedStopUrl", "url")
      , ("tflApiPresentationEntitiesMatchedStopName", "name")
      , ("tflApiPresentationEntitiesMatchedStopLat", "lat")
      , ("tflApiPresentationEntitiesMatchedStopLon", "lon")
      ]


-- | 
data TflApiPresentationEntitiesMessage = TflApiPresentationEntitiesMessage
  { tflApiPresentationEntitiesMessageBulletOrder :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesMessageHeader :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesMessageMessageText :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMessageLinkText :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesMessageUrl :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesMessage where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesMessage
instance ToJSON TflApiPresentationEntitiesMessage where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesMessage
instance ToSchema TflApiPresentationEntitiesMessage where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesMessage

optionsTflApiPresentationEntitiesMessage :: Options
optionsTflApiPresentationEntitiesMessage =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesMessageBulletOrder", "bulletOrder")
      , ("tflApiPresentationEntitiesMessageHeader", "header")
      , ("tflApiPresentationEntitiesMessageMessageText", "messageText")
      , ("tflApiPresentationEntitiesMessageLinkText", "linkText")
      , ("tflApiPresentationEntitiesMessageUrl", "url")
      ]


-- | 
data TflApiPresentationEntitiesMode = TflApiPresentationEntitiesMode
  { tflApiPresentationEntitiesModeIsTflService :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesModeIsFarePaying :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesModeIsScheduledService :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesModeModeName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesModeMotType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesModeNetwork :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesMode where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesMode
instance ToJSON TflApiPresentationEntitiesMode where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesMode
instance ToSchema TflApiPresentationEntitiesMode where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesMode

optionsTflApiPresentationEntitiesMode :: Options
optionsTflApiPresentationEntitiesMode =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesModeIsTflService", "isTflService")
      , ("tflApiPresentationEntitiesModeIsFarePaying", "isFarePaying")
      , ("tflApiPresentationEntitiesModeIsScheduledService", "isScheduledService")
      , ("tflApiPresentationEntitiesModeModeName", "modeName")
      , ("tflApiPresentationEntitiesModeMotType", "motType")
      , ("tflApiPresentationEntitiesModeNetwork", "network")
      ]


-- | Represent travel network status
data TflApiPresentationEntitiesNetworkStatus = TflApiPresentationEntitiesNetworkStatus
  { tflApiPresentationEntitiesNetworkStatusOperator :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesNetworkStatusStatus :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesNetworkStatusMessage :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesNetworkStatusStatusLevel :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesNetworkStatus where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesNetworkStatus
instance ToJSON TflApiPresentationEntitiesNetworkStatus where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesNetworkStatus
instance ToSchema TflApiPresentationEntitiesNetworkStatus where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesNetworkStatus

optionsTflApiPresentationEntitiesNetworkStatus :: Options
optionsTflApiPresentationEntitiesNetworkStatus =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesNetworkStatusOperator", "operator")
      , ("tflApiPresentationEntitiesNetworkStatusStatus", "status")
      , ("tflApiPresentationEntitiesNetworkStatusMessage", "message")
      , ("tflApiPresentationEntitiesNetworkStatusStatusLevel", "statusLevel")
      ]


-- | 
data TflApiPresentationEntitiesOrderedRoute = TflApiPresentationEntitiesOrderedRoute
  { tflApiPresentationEntitiesOrderedRouteName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesOrderedRouteNaptanIds :: Maybe [Text] -- ^ 
  , tflApiPresentationEntitiesOrderedRouteServiceType :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesOrderedRoute where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesOrderedRoute
instance ToJSON TflApiPresentationEntitiesOrderedRoute where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesOrderedRoute
instance ToSchema TflApiPresentationEntitiesOrderedRoute where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesOrderedRoute

optionsTflApiPresentationEntitiesOrderedRoute :: Options
optionsTflApiPresentationEntitiesOrderedRoute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesOrderedRouteName", "name")
      , ("tflApiPresentationEntitiesOrderedRouteNaptanIds", "naptanIds")
      , ("tflApiPresentationEntitiesOrderedRouteServiceType", "serviceType")
      ]


-- | 
data TflApiPresentationEntitiesPassengerFlow = TflApiPresentationEntitiesPassengerFlow
  { tflApiPresentationEntitiesPassengerFlowTimeSlice :: Maybe Text -- ^ Time in 24hr format with 15 minute intervals e.g. 0500-0515, 0515-0530 etc.
  , tflApiPresentationEntitiesPassengerFlowValue :: Maybe Int -- ^ Count of passenger flow towards a platform
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesPassengerFlow where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesPassengerFlow
instance ToJSON TflApiPresentationEntitiesPassengerFlow where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesPassengerFlow
instance ToSchema TflApiPresentationEntitiesPassengerFlow where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesPassengerFlow

optionsTflApiPresentationEntitiesPassengerFlow :: Options
optionsTflApiPresentationEntitiesPassengerFlow =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesPassengerFlowTimeSlice", "timeSlice")
      , ("tflApiPresentationEntitiesPassengerFlowValue", "value")
      ]


-- | 
data TflApiPresentationEntitiesPathAttribute = TflApiPresentationEntitiesPathAttribute
  { tflApiPresentationEntitiesPathAttributeName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesPathAttributeValue :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesPathAttribute where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesPathAttribute
instance ToJSON TflApiPresentationEntitiesPathAttribute where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesPathAttribute
instance ToSchema TflApiPresentationEntitiesPathAttribute where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesPathAttribute

optionsTflApiPresentationEntitiesPathAttribute :: Options
optionsTflApiPresentationEntitiesPathAttribute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesPathAttributeName", "name")
      , ("tflApiPresentationEntitiesPathAttributeValue", "value")
      ]


-- | 
data TflApiPresentationEntitiesPeriod = TflApiPresentationEntitiesPeriod
  { tflApiPresentationEntitiesPeriodType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesPeriodFromTime :: Maybe TflApiPresentationEntitiesTwentyFourHourClockTime -- ^ 
  , tflApiPresentationEntitiesPeriodToTime :: Maybe TflApiPresentationEntitiesTwentyFourHourClockTime -- ^ 
  , tflApiPresentationEntitiesPeriodFrequency :: Maybe TflApiPresentationEntitiesServiceFrequency -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesPeriod where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesPeriod
instance ToJSON TflApiPresentationEntitiesPeriod where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesPeriod
instance ToSchema TflApiPresentationEntitiesPeriod where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesPeriod

optionsTflApiPresentationEntitiesPeriod :: Options
optionsTflApiPresentationEntitiesPeriod =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesPeriodType", "type")
      , ("tflApiPresentationEntitiesPeriodFromTime", "fromTime")
      , ("tflApiPresentationEntitiesPeriodToTime", "toTime")
      , ("tflApiPresentationEntitiesPeriodFrequency", "frequency")
      ]


-- | 
data TflApiPresentationEntitiesPlace = TflApiPresentationEntitiesPlace
  { tflApiPresentationEntitiesPlaceId :: Maybe Text -- ^ A unique identifier.
  , tflApiPresentationEntitiesPlaceUrl :: Maybe Text -- ^ The unique location of this resource.
  , tflApiPresentationEntitiesPlaceCommonName :: Maybe Text -- ^ A human readable name.
  , tflApiPresentationEntitiesPlaceDistance :: Maybe Double -- ^ The distance of the place from its search point, if this is the result              of a geographical search, otherwise zero.
  , tflApiPresentationEntitiesPlacePlaceType :: Maybe Text -- ^ The type of Place. See /Place/Meta/placeTypes for possible values.
  , tflApiPresentationEntitiesPlaceAdditionalProperties :: Maybe [TflApiPresentationEntitiesAdditionalProperties] -- ^ A bag of additional key/value pairs with extra information about this place.
  , tflApiPresentationEntitiesPlaceChildren :: Maybe [TflApiPresentationEntitiesPlace] -- ^ 
  , tflApiPresentationEntitiesPlaceChildrenUrls :: Maybe [Text] -- ^ 
  , tflApiPresentationEntitiesPlaceLat :: Maybe Double -- ^ WGS84 latitude of the location.
  , tflApiPresentationEntitiesPlaceLon :: Maybe Double -- ^ WGS84 longitude of the location.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesPlace where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesPlace
instance ToJSON TflApiPresentationEntitiesPlace where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesPlace
instance ToSchema TflApiPresentationEntitiesPlace where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesPlace

optionsTflApiPresentationEntitiesPlace :: Options
optionsTflApiPresentationEntitiesPlace =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesPlaceId", "id")
      , ("tflApiPresentationEntitiesPlaceUrl", "url")
      , ("tflApiPresentationEntitiesPlaceCommonName", "commonName")
      , ("tflApiPresentationEntitiesPlaceDistance", "distance")
      , ("tflApiPresentationEntitiesPlacePlaceType", "placeType")
      , ("tflApiPresentationEntitiesPlaceAdditionalProperties", "additionalProperties")
      , ("tflApiPresentationEntitiesPlaceChildren", "children")
      , ("tflApiPresentationEntitiesPlaceChildrenUrls", "childrenUrls")
      , ("tflApiPresentationEntitiesPlaceLat", "lat")
      , ("tflApiPresentationEntitiesPlaceLon", "lon")
      ]


-- | 
data TflApiPresentationEntitiesPlaceCategory = TflApiPresentationEntitiesPlaceCategory
  { tflApiPresentationEntitiesPlaceCategoryCategory :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesPlaceCategoryAvailableKeys :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesPlaceCategory where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesPlaceCategory
instance ToJSON TflApiPresentationEntitiesPlaceCategory where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesPlaceCategory
instance ToSchema TflApiPresentationEntitiesPlaceCategory where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesPlaceCategory

optionsTflApiPresentationEntitiesPlaceCategory :: Options
optionsTflApiPresentationEntitiesPlaceCategory =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesPlaceCategoryCategory", "category")
      , ("tflApiPresentationEntitiesPlaceCategoryAvailableKeys", "availableKeys")
      ]


-- | 
data TflApiPresentationEntitiesPlacePolygon = TflApiPresentationEntitiesPlacePolygon
  { tflApiPresentationEntitiesPlacePolygonGeoPoints :: Maybe [TflApiCommonGeoPoint] -- ^ 
  , tflApiPresentationEntitiesPlacePolygonCommonName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesPlacePolygon where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesPlacePolygon
instance ToJSON TflApiPresentationEntitiesPlacePolygon where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesPlacePolygon
instance ToSchema TflApiPresentationEntitiesPlacePolygon where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesPlacePolygon

optionsTflApiPresentationEntitiesPlacePolygon :: Options
optionsTflApiPresentationEntitiesPlacePolygon =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesPlacePolygonGeoPoints", "geoPoints")
      , ("tflApiPresentationEntitiesPlacePolygonCommonName", "commonName")
      ]


-- | Represents a point located at a latitude and longitude using the WGS84 co-ordinate system.
data TflApiPresentationEntitiesPoint = TflApiPresentationEntitiesPoint
  { tflApiPresentationEntitiesPointLat :: Maybe Double -- ^ WGS84 latitude of the location.
  , tflApiPresentationEntitiesPointLon :: Maybe Double -- ^ WGS84 longitude of the location.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesPoint where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesPoint
instance ToJSON TflApiPresentationEntitiesPoint where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesPoint
instance ToSchema TflApiPresentationEntitiesPoint where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesPoint

optionsTflApiPresentationEntitiesPoint :: Options
optionsTflApiPresentationEntitiesPoint =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesPointLat", "lat")
      , ("tflApiPresentationEntitiesPointLon", "lon")
      ]


-- | DTO to capture the prediction details
data TflApiPresentationEntitiesPrediction = TflApiPresentationEntitiesPrediction
  { tflApiPresentationEntitiesPredictionId :: Maybe Text -- ^ The identitier for the prediction
  , tflApiPresentationEntitiesPredictionOperationType :: Maybe Int -- ^ The type of the operation (1: is new or has been updated, 2: should be deleted from any client cache)
  , tflApiPresentationEntitiesPredictionVehicleId :: Maybe Text -- ^ The actual vehicle in transit (for train modes, the leading car of the rolling set)
  , tflApiPresentationEntitiesPredictionNaptanId :: Maybe Text -- ^ Identifier for the prediction
  , tflApiPresentationEntitiesPredictionStationName :: Maybe Text -- ^ Station name
  , tflApiPresentationEntitiesPredictionLineId :: Maybe Text -- ^ Unique identifier for the Line
  , tflApiPresentationEntitiesPredictionLineName :: Maybe Text -- ^ Line Name
  , tflApiPresentationEntitiesPredictionPlatformName :: Maybe Text -- ^ Platform name (for bus, this is the stop letter)
  , tflApiPresentationEntitiesPredictionDirection :: Maybe Text -- ^ Direction (unified to inbound/outbound)
  , tflApiPresentationEntitiesPredictionBearing :: Maybe Text -- ^ Bearing (between 0 to 359)
  , tflApiPresentationEntitiesPredictionTripId :: Maybe Text -- ^ TripId is used to assemble the primary key
  , tflApiPresentationEntitiesPredictionBaseVersion :: Maybe Text -- ^ Data base version
  , tflApiPresentationEntitiesPredictionDestinationNaptanId :: Maybe Text -- ^ Naptan Identifier for the prediction's destination
  , tflApiPresentationEntitiesPredictionDestinationName :: Maybe Text -- ^ Name of the destination
  , tflApiPresentationEntitiesPredictionTimestamp :: Maybe UTCTime -- ^ Timestamp for when the prediction was inserted/modified (source column drives what objects are broadcast on each iteration)
  , tflApiPresentationEntitiesPredictionTimeToStation :: Maybe Int -- ^ Prediction of the Time to station in seconds
  , tflApiPresentationEntitiesPredictionCurrentLocation :: Maybe Text -- ^ The current location of the vehicle.
  , tflApiPresentationEntitiesPredictionTowards :: Maybe Text -- ^ Routing information or other descriptive text about the path of the vehicle towards the destination
  , tflApiPresentationEntitiesPredictionExpectedArrival :: Maybe UTCTime -- ^ The expected arrival time of the vehicle at the stop/station
  , tflApiPresentationEntitiesPredictionTimeToLive :: Maybe UTCTime -- ^ The expiry time for the prediction
  , tflApiPresentationEntitiesPredictionModeName :: Maybe Text -- ^ The mode name of the station/line the prediction relates to
  , tflApiPresentationEntitiesPredictionTiming :: Maybe TflApiPresentationEntitiesPredictionTiming -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesPrediction where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesPrediction
instance ToJSON TflApiPresentationEntitiesPrediction where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesPrediction
instance ToSchema TflApiPresentationEntitiesPrediction where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesPrediction

optionsTflApiPresentationEntitiesPrediction :: Options
optionsTflApiPresentationEntitiesPrediction =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesPredictionId", "id")
      , ("tflApiPresentationEntitiesPredictionOperationType", "operationType")
      , ("tflApiPresentationEntitiesPredictionVehicleId", "vehicleId")
      , ("tflApiPresentationEntitiesPredictionNaptanId", "naptanId")
      , ("tflApiPresentationEntitiesPredictionStationName", "stationName")
      , ("tflApiPresentationEntitiesPredictionLineId", "lineId")
      , ("tflApiPresentationEntitiesPredictionLineName", "lineName")
      , ("tflApiPresentationEntitiesPredictionPlatformName", "platformName")
      , ("tflApiPresentationEntitiesPredictionDirection", "direction")
      , ("tflApiPresentationEntitiesPredictionBearing", "bearing")
      , ("tflApiPresentationEntitiesPredictionTripId", "tripId")
      , ("tflApiPresentationEntitiesPredictionBaseVersion", "baseVersion")
      , ("tflApiPresentationEntitiesPredictionDestinationNaptanId", "destinationNaptanId")
      , ("tflApiPresentationEntitiesPredictionDestinationName", "destinationName")
      , ("tflApiPresentationEntitiesPredictionTimestamp", "timestamp")
      , ("tflApiPresentationEntitiesPredictionTimeToStation", "timeToStation")
      , ("tflApiPresentationEntitiesPredictionCurrentLocation", "currentLocation")
      , ("tflApiPresentationEntitiesPredictionTowards", "towards")
      , ("tflApiPresentationEntitiesPredictionExpectedArrival", "expectedArrival")
      , ("tflApiPresentationEntitiesPredictionTimeToLive", "timeToLive")
      , ("tflApiPresentationEntitiesPredictionModeName", "modeName")
      , ("tflApiPresentationEntitiesPredictionTiming", "timing")
      ]


-- | 
data TflApiPresentationEntitiesPredictionTiming = TflApiPresentationEntitiesPredictionTiming
  { tflApiPresentationEntitiesPredictionTimingCountdownServerAdjustment :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesPredictionTimingSource :: Maybe UTCTime' -- ^ 
  , tflApiPresentationEntitiesPredictionTimingInsert :: Maybe UTCTime' -- ^ 
  , tflApiPresentationEntitiesPredictionTimingRead :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesPredictionTimingSent :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesPredictionTimingReceived :: Maybe UTCTime' -- ^ 
  } deriving (Show, Eq, Generic, Data)

newtype UTCTime' = UTCTime' UTCTime
  deriving newtype (Show, Eq, ToSchema, ToJSON)
  deriving stock (Data)
instance FromJSON UTCTime' where
  parseJSON = \case
    String "0001-01-01T00:00:00" -> pure $ UTCTime' $ UTCTime (fromOrdinalDate 0 0) 0
    x -> UTCTime' <$> parseJSON x

instance FromJSON TflApiPresentationEntitiesPredictionTiming where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesPredictionTiming
instance ToJSON TflApiPresentationEntitiesPredictionTiming where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesPredictionTiming
instance ToSchema TflApiPresentationEntitiesPredictionTiming where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesPredictionTiming

optionsTflApiPresentationEntitiesPredictionTiming :: Options
optionsTflApiPresentationEntitiesPredictionTiming =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesPredictionTimingCountdownServerAdjustment", "countdownServerAdjustment")
      , ("tflApiPresentationEntitiesPredictionTimingSource", "source")
      , ("tflApiPresentationEntitiesPredictionTimingInsert", "insert")
      , ("tflApiPresentationEntitiesPredictionTimingRead", "read")
      , ("tflApiPresentationEntitiesPredictionTimingSent", "sent")
      , ("tflApiPresentationEntitiesPredictionTimingReceived", "received")
      ]


-- | 
data TflApiPresentationEntitiesRedirect = TflApiPresentationEntitiesRedirect
  { tflApiPresentationEntitiesRedirectShortUrl :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRedirectLongUrl :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRedirectActive :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesRedirect where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesRedirect
instance ToJSON TflApiPresentationEntitiesRedirect where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesRedirect
instance ToSchema TflApiPresentationEntitiesRedirect where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesRedirect

optionsTflApiPresentationEntitiesRedirect :: Options
optionsTflApiPresentationEntitiesRedirect =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesRedirectShortUrl", "shortUrl")
      , ("tflApiPresentationEntitiesRedirectLongUrl", "longUrl")
      , ("tflApiPresentationEntitiesRedirectActive", "active")
      ]


-- | 
data TflApiPresentationEntitiesRoadCorridor = TflApiPresentationEntitiesRoadCorridor
  { tflApiPresentationEntitiesRoadCorridorId :: Maybe Text -- ^ The Id of the Corridor e.g. \"A406\"
  , tflApiPresentationEntitiesRoadCorridorDisplayName :: Maybe Text -- ^ The display name of the Corridor e.g. \"North Circular (A406)\". This              may be identical to the Id.
  , tflApiPresentationEntitiesRoadCorridorGroup :: Maybe Text -- ^ The group name of the Corridor e.g. \"Central London\". Most corridors are not grouped, in which case this field can be null.
  , tflApiPresentationEntitiesRoadCorridorStatusSeverity :: Maybe Text -- ^ Standard multi-mode status severity code
  , tflApiPresentationEntitiesRoadCorridorStatusSeverityDescription :: Maybe Text -- ^ Description of the status severity as applied to RoadCorridors
  , tflApiPresentationEntitiesRoadCorridorBounds :: Maybe Text -- ^ The Bounds of the Corridor, given by the south-east followed by the north-west co-ordinate              pair in geoJSON format e.g. \"[[-1.241531,51.242151],[1.641223,53.765721]]\"
  , tflApiPresentationEntitiesRoadCorridorEnvelope :: Maybe Text -- ^ The Envelope of the Corridor, given by the corner co-ordinates of a rectangular (four-point) polygon              in geoJSON format e.g. \"[[-1.241531,51.242151],[-1.241531,53.765721],[1.641223,53.765721],[1.641223,51.242151]]\"
  , tflApiPresentationEntitiesRoadCorridorStatusAggregationStartDate :: Maybe UTCTime -- ^ The start of the period over which status has been aggregated, or null if this is the current corridor status.
  , tflApiPresentationEntitiesRoadCorridorStatusAggregationEndDate :: Maybe UTCTime -- ^ The end of the period over which status has been aggregated, or null if this is the current corridor status.
  , tflApiPresentationEntitiesRoadCorridorUrl :: Maybe Text -- ^ URL to retrieve this Corridor.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesRoadCorridor where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesRoadCorridor
instance ToJSON TflApiPresentationEntitiesRoadCorridor where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesRoadCorridor
instance ToSchema TflApiPresentationEntitiesRoadCorridor where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesRoadCorridor

optionsTflApiPresentationEntitiesRoadCorridor :: Options
optionsTflApiPresentationEntitiesRoadCorridor =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesRoadCorridorId", "id")
      , ("tflApiPresentationEntitiesRoadCorridorDisplayName", "displayName")
      , ("tflApiPresentationEntitiesRoadCorridorGroup", "group")
      , ("tflApiPresentationEntitiesRoadCorridorStatusSeverity", "statusSeverity")
      , ("tflApiPresentationEntitiesRoadCorridorStatusSeverityDescription", "statusSeverityDescription")
      , ("tflApiPresentationEntitiesRoadCorridorBounds", "bounds")
      , ("tflApiPresentationEntitiesRoadCorridorEnvelope", "envelope")
      , ("tflApiPresentationEntitiesRoadCorridorStatusAggregationStartDate", "statusAggregationStartDate")
      , ("tflApiPresentationEntitiesRoadCorridorStatusAggregationEndDate", "statusAggregationEndDate")
      , ("tflApiPresentationEntitiesRoadCorridorUrl", "url")
      ]


-- | 
data TflApiPresentationEntitiesRoadDisruption = TflApiPresentationEntitiesRoadDisruption
  { tflApiPresentationEntitiesRoadDisruptionId :: Maybe Text -- ^ Unique identifier for the road disruption
  , tflApiPresentationEntitiesRoadDisruptionUrl :: Maybe Text -- ^ URL to retrieve this road disruption
  , tflApiPresentationEntitiesRoadDisruptionPoint :: Maybe Text -- ^ Latitude and longitude (WGS84) of the centroid of the disruption, stored in a geoJSON-formatted string.
  , tflApiPresentationEntitiesRoadDisruptionSeverity :: Maybe Text -- ^ A description of the severity of the disruption.
  , tflApiPresentationEntitiesRoadDisruptionOrdinal :: Maybe Int -- ^ An ordinal of the disruption based on severity, level of interest and corridor.
  , tflApiPresentationEntitiesRoadDisruptionCategory :: Maybe Text -- ^ Describes the nature of disruption e.g. Traffic Incidents, Works
  , tflApiPresentationEntitiesRoadDisruptionSubCategory :: Maybe Text -- ^ Describes the sub-category of disruption e.g. Collapsed Manhole, Abnormal Load
  , tflApiPresentationEntitiesRoadDisruptionComments :: Maybe Text -- ^ Full text of comments describing the disruption, including details of any road closures and diversions, where appropriate.
  , tflApiPresentationEntitiesRoadDisruptionCurrentUpdate :: Maybe Text -- ^ Text of the most recent update from the LSTCC on the state of the               disruption, including the current traffic impact and any advice to               road users.
  , tflApiPresentationEntitiesRoadDisruptionCurrentUpdateDateTime :: Maybe UTCTime -- ^ The time when the last CurrentUpdate description was recorded,               or null if no CurrentUpdate has been applied.
  , tflApiPresentationEntitiesRoadDisruptionCorridorIds :: Maybe [Text] -- ^ The Ids of affected corridors, if any.
  , tflApiPresentationEntitiesRoadDisruptionStartDateTime :: Maybe UTCTime -- ^ The date and time which the disruption started. For a planned disruption (i.e. planned road works) this date will be in the future.              For unplanned disruptions, this will default to the date on which the disruption was first recorded, but may be adjusted by the operator.
  , tflApiPresentationEntitiesRoadDisruptionEndDateTime :: Maybe UTCTime -- ^ The date and time on which the disruption ended. For planned disruptions, this date will have a valid value. For unplanned               disruptions in progress, this field will be omitted.
  , tflApiPresentationEntitiesRoadDisruptionLastModifiedTime :: Maybe UTCTime -- ^ The date and time on which the disruption was last modified in the system. This information can reliably be used by a developer to quickly              compare two instances of the same disruption to determine if it has been changed.
  , tflApiPresentationEntitiesRoadDisruptionLevelOfInterest :: Maybe Text -- ^ This describes the level of potential impact on traffic operations of the disruption.               High = e.g. a one-off disruption on a major or high profile route which will require a high level of operational attention               Medium = This is the default value               Low = e.g. a frequently occurring disruption which is well known
  , tflApiPresentationEntitiesRoadDisruptionLocation :: Maybe Text -- ^ Main road name / number (borough) or preset area name where the disruption is located. This might be useful for a map popup where space is limited.
  , tflApiPresentationEntitiesRoadDisruptionStatus :: Maybe Text -- ^ This describes the status of the disruption.                Active = currently in progress               Active Long Term = currently in progress and long term              Scheduled = scheduled to start within the next 180 days              Recurring Works = planned maintenance works that follow a regular routine or pattern and whose next occurrence is to start within the next 180 days.              Recently Cleared = recently cleared in the last 24 hours              Note that the status of Scheduled or Recurring Works disruptions will change to Active when they start, and will change status again when they end.
  , tflApiPresentationEntitiesRoadDisruptionGeography :: Maybe SystemDataSpatialDbGeography -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionGeometry :: Maybe SystemDataSpatialDbGeography -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionStreets :: Maybe [TflApiPresentationEntitiesStreet] -- ^ A collection of zero or more streets affected by the disruption.
  , tflApiPresentationEntitiesRoadDisruptionIsProvisional :: Maybe Bool -- ^ True if the disruption is planned on a future date that is open to change
  , tflApiPresentationEntitiesRoadDisruptionHasClosures :: Maybe Bool -- ^ True if any of the affected Streets have a \"Full Closure\" status, false otherwise. A RoadDisruption that has HasClosures is considered a               Severe or Serious disruption for severity filtering purposes.
  , tflApiPresentationEntitiesRoadDisruptionLinkText :: Maybe Text -- ^ The text of any associated link
  , tflApiPresentationEntitiesRoadDisruptionLinkUrl :: Maybe Text -- ^ The url of any associated link
  , tflApiPresentationEntitiesRoadDisruptionRoadProject :: Maybe TflApiPresentationEntitiesRoadProject -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionPublishStartDate :: Maybe UTCTime -- ^ TDM Additional properties
  , tflApiPresentationEntitiesRoadDisruptionPublishEndDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionTimeFrame :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionRoadDisruptionLines :: Maybe [TflApiPresentationEntitiesRoadDisruptionLine] -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionRoadDisruptionImpactAreas :: Maybe [TflApiPresentationEntitiesRoadDisruptionImpactArea] -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionRecurringSchedules :: Maybe [TflApiPresentationEntitiesRoadDisruptionSchedule] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesRoadDisruption where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesRoadDisruption
instance ToJSON TflApiPresentationEntitiesRoadDisruption where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesRoadDisruption
instance ToSchema TflApiPresentationEntitiesRoadDisruption where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesRoadDisruption

optionsTflApiPresentationEntitiesRoadDisruption :: Options
optionsTflApiPresentationEntitiesRoadDisruption =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesRoadDisruptionId", "id")
      , ("tflApiPresentationEntitiesRoadDisruptionUrl", "url")
      , ("tflApiPresentationEntitiesRoadDisruptionPoint", "point")
      , ("tflApiPresentationEntitiesRoadDisruptionSeverity", "severity")
      , ("tflApiPresentationEntitiesRoadDisruptionOrdinal", "ordinal")
      , ("tflApiPresentationEntitiesRoadDisruptionCategory", "category")
      , ("tflApiPresentationEntitiesRoadDisruptionSubCategory", "subCategory")
      , ("tflApiPresentationEntitiesRoadDisruptionComments", "comments")
      , ("tflApiPresentationEntitiesRoadDisruptionCurrentUpdate", "currentUpdate")
      , ("tflApiPresentationEntitiesRoadDisruptionCurrentUpdateDateTime", "currentUpdateDateTime")
      , ("tflApiPresentationEntitiesRoadDisruptionCorridorIds", "corridorIds")
      , ("tflApiPresentationEntitiesRoadDisruptionStartDateTime", "startDateTime")
      , ("tflApiPresentationEntitiesRoadDisruptionEndDateTime", "endDateTime")
      , ("tflApiPresentationEntitiesRoadDisruptionLastModifiedTime", "lastModifiedTime")
      , ("tflApiPresentationEntitiesRoadDisruptionLevelOfInterest", "levelOfInterest")
      , ("tflApiPresentationEntitiesRoadDisruptionLocation", "location")
      , ("tflApiPresentationEntitiesRoadDisruptionStatus", "status")
      , ("tflApiPresentationEntitiesRoadDisruptionGeography", "geography")
      , ("tflApiPresentationEntitiesRoadDisruptionGeometry", "geometry")
      , ("tflApiPresentationEntitiesRoadDisruptionStreets", "streets")
      , ("tflApiPresentationEntitiesRoadDisruptionIsProvisional", "isProvisional")
      , ("tflApiPresentationEntitiesRoadDisruptionHasClosures", "hasClosures")
      , ("tflApiPresentationEntitiesRoadDisruptionLinkText", "linkText")
      , ("tflApiPresentationEntitiesRoadDisruptionLinkUrl", "linkUrl")
      , ("tflApiPresentationEntitiesRoadDisruptionRoadProject", "roadProject")
      , ("tflApiPresentationEntitiesRoadDisruptionPublishStartDate", "publishStartDate")
      , ("tflApiPresentationEntitiesRoadDisruptionPublishEndDate", "publishEndDate")
      , ("tflApiPresentationEntitiesRoadDisruptionTimeFrame", "timeFrame")
      , ("tflApiPresentationEntitiesRoadDisruptionRoadDisruptionLines", "roadDisruptionLines")
      , ("tflApiPresentationEntitiesRoadDisruptionRoadDisruptionImpactAreas", "roadDisruptionImpactAreas")
      , ("tflApiPresentationEntitiesRoadDisruptionRecurringSchedules", "recurringSchedules")
      ]


-- | 
data TflApiPresentationEntitiesRoadDisruptionImpactArea = TflApiPresentationEntitiesRoadDisruptionImpactArea
  { tflApiPresentationEntitiesRoadDisruptionImpactAreaId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionImpactAreaRoadDisruptionId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionImpactAreaPolygon :: Maybe SystemDataSpatialDbGeography -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionImpactAreaStartDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionImpactAreaEndDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionImpactAreaStartTime :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionImpactAreaEndTime :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesRoadDisruptionImpactArea where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesRoadDisruptionImpactArea
instance ToJSON TflApiPresentationEntitiesRoadDisruptionImpactArea where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesRoadDisruptionImpactArea
instance ToSchema TflApiPresentationEntitiesRoadDisruptionImpactArea where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesRoadDisruptionImpactArea

optionsTflApiPresentationEntitiesRoadDisruptionImpactArea :: Options
optionsTflApiPresentationEntitiesRoadDisruptionImpactArea =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesRoadDisruptionImpactAreaId", "id")
      , ("tflApiPresentationEntitiesRoadDisruptionImpactAreaRoadDisruptionId", "roadDisruptionId")
      , ("tflApiPresentationEntitiesRoadDisruptionImpactAreaPolygon", "polygon")
      , ("tflApiPresentationEntitiesRoadDisruptionImpactAreaStartDate", "startDate")
      , ("tflApiPresentationEntitiesRoadDisruptionImpactAreaEndDate", "endDate")
      , ("tflApiPresentationEntitiesRoadDisruptionImpactAreaStartTime", "startTime")
      , ("tflApiPresentationEntitiesRoadDisruptionImpactAreaEndTime", "endTime")
      ]


-- | 
data TflApiPresentationEntitiesRoadDisruptionLine = TflApiPresentationEntitiesRoadDisruptionLine
  { tflApiPresentationEntitiesRoadDisruptionLineId :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionLineRoadDisruptionId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionLineIsDiversion :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionLineMultiLineString :: Maybe SystemDataSpatialDbGeography -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionLineStartDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionLineEndDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionLineStartTime :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionLineEndTime :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesRoadDisruptionLine where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesRoadDisruptionLine
instance ToJSON TflApiPresentationEntitiesRoadDisruptionLine where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesRoadDisruptionLine
instance ToSchema TflApiPresentationEntitiesRoadDisruptionLine where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesRoadDisruptionLine

optionsTflApiPresentationEntitiesRoadDisruptionLine :: Options
optionsTflApiPresentationEntitiesRoadDisruptionLine =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesRoadDisruptionLineId", "id")
      , ("tflApiPresentationEntitiesRoadDisruptionLineRoadDisruptionId", "roadDisruptionId")
      , ("tflApiPresentationEntitiesRoadDisruptionLineIsDiversion", "isDiversion")
      , ("tflApiPresentationEntitiesRoadDisruptionLineMultiLineString", "multiLineString")
      , ("tflApiPresentationEntitiesRoadDisruptionLineStartDate", "startDate")
      , ("tflApiPresentationEntitiesRoadDisruptionLineEndDate", "endDate")
      , ("tflApiPresentationEntitiesRoadDisruptionLineStartTime", "startTime")
      , ("tflApiPresentationEntitiesRoadDisruptionLineEndTime", "endTime")
      ]


-- | 
data TflApiPresentationEntitiesRoadDisruptionSchedule = TflApiPresentationEntitiesRoadDisruptionSchedule
  { tflApiPresentationEntitiesRoadDisruptionScheduleStartTime :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesRoadDisruptionScheduleEndTime :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesRoadDisruptionSchedule where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesRoadDisruptionSchedule
instance ToJSON TflApiPresentationEntitiesRoadDisruptionSchedule where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesRoadDisruptionSchedule
instance ToSchema TflApiPresentationEntitiesRoadDisruptionSchedule where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesRoadDisruptionSchedule

optionsTflApiPresentationEntitiesRoadDisruptionSchedule :: Options
optionsTflApiPresentationEntitiesRoadDisruptionSchedule =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesRoadDisruptionScheduleStartTime", "startTime")
      , ("tflApiPresentationEntitiesRoadDisruptionScheduleEndTime", "endTime")
      ]


-- | 
data TflApiPresentationEntitiesRoadProject = TflApiPresentationEntitiesRoadProject
  { tflApiPresentationEntitiesRoadProjectProjectId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadProjectSchemeName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadProjectProjectName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadProjectProjectDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadProjectProjectPageUrl :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadProjectConsultationPageUrl :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadProjectConsultationStartDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesRoadProjectConsultationEndDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesRoadProjectConstructionStartDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesRoadProjectConstructionEndDate :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesRoadProjectBoroughsBenefited :: Maybe [Text] -- ^ 
  , tflApiPresentationEntitiesRoadProjectCycleSuperhighwayId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadProjectPhase :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadProjectContactName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadProjectContactEmail :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadProjectExternalPageUrl :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRoadProjectProjectSummaryPageUrl :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesRoadProject where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesRoadProject
instance ToJSON TflApiPresentationEntitiesRoadProject where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesRoadProject
instance ToSchema TflApiPresentationEntitiesRoadProject where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesRoadProject

optionsTflApiPresentationEntitiesRoadProject :: Options
optionsTflApiPresentationEntitiesRoadProject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesRoadProjectProjectId", "projectId")
      , ("tflApiPresentationEntitiesRoadProjectSchemeName", "schemeName")
      , ("tflApiPresentationEntitiesRoadProjectProjectName", "projectName")
      , ("tflApiPresentationEntitiesRoadProjectProjectDescription", "projectDescription")
      , ("tflApiPresentationEntitiesRoadProjectProjectPageUrl", "projectPageUrl")
      , ("tflApiPresentationEntitiesRoadProjectConsultationPageUrl", "consultationPageUrl")
      , ("tflApiPresentationEntitiesRoadProjectConsultationStartDate", "consultationStartDate")
      , ("tflApiPresentationEntitiesRoadProjectConsultationEndDate", "consultationEndDate")
      , ("tflApiPresentationEntitiesRoadProjectConstructionStartDate", "constructionStartDate")
      , ("tflApiPresentationEntitiesRoadProjectConstructionEndDate", "constructionEndDate")
      , ("tflApiPresentationEntitiesRoadProjectBoroughsBenefited", "boroughsBenefited")
      , ("tflApiPresentationEntitiesRoadProjectCycleSuperhighwayId", "cycleSuperhighwayId")
      , ("tflApiPresentationEntitiesRoadProjectPhase", "phase")
      , ("tflApiPresentationEntitiesRoadProjectContactName", "contactName")
      , ("tflApiPresentationEntitiesRoadProjectContactEmail", "contactEmail")
      , ("tflApiPresentationEntitiesRoadProjectExternalPageUrl", "externalPageUrl")
      , ("tflApiPresentationEntitiesRoadProjectProjectSummaryPageUrl", "projectSummaryPageUrl")
      ]


-- | 
data TflApiPresentationEntitiesRouteSearchMatch = TflApiPresentationEntitiesRouteSearchMatch
  { tflApiPresentationEntitiesRouteSearchMatchLineId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRouteSearchMatchMode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRouteSearchMatchLineName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRouteSearchMatchLineRouteSection :: Maybe [TflApiPresentationEntitiesLineRouteSection] -- ^ 
  , tflApiPresentationEntitiesRouteSearchMatchMatchedRouteSections :: Maybe [TflApiPresentationEntitiesMatchedRouteSections] -- ^ 
  , tflApiPresentationEntitiesRouteSearchMatchMatchedStops :: Maybe [TflApiPresentationEntitiesMatchedStop] -- ^ 
  , tflApiPresentationEntitiesRouteSearchMatchId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRouteSearchMatchUrl :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRouteSearchMatchName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRouteSearchMatchLat :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesRouteSearchMatchLon :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesRouteSearchMatch where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesRouteSearchMatch
instance ToJSON TflApiPresentationEntitiesRouteSearchMatch where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesRouteSearchMatch
instance ToSchema TflApiPresentationEntitiesRouteSearchMatch where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesRouteSearchMatch

optionsTflApiPresentationEntitiesRouteSearchMatch :: Options
optionsTflApiPresentationEntitiesRouteSearchMatch =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesRouteSearchMatchLineId", "lineId")
      , ("tflApiPresentationEntitiesRouteSearchMatchMode", "mode")
      , ("tflApiPresentationEntitiesRouteSearchMatchLineName", "lineName")
      , ("tflApiPresentationEntitiesRouteSearchMatchLineRouteSection", "lineRouteSection")
      , ("tflApiPresentationEntitiesRouteSearchMatchMatchedRouteSections", "matchedRouteSections")
      , ("tflApiPresentationEntitiesRouteSearchMatchMatchedStops", "matchedStops")
      , ("tflApiPresentationEntitiesRouteSearchMatchId", "id")
      , ("tflApiPresentationEntitiesRouteSearchMatchUrl", "url")
      , ("tflApiPresentationEntitiesRouteSearchMatchName", "name")
      , ("tflApiPresentationEntitiesRouteSearchMatchLat", "lat")
      , ("tflApiPresentationEntitiesRouteSearchMatchLon", "lon")
      ]


-- | 
data TflApiPresentationEntitiesRouteSearchResponse = TflApiPresentationEntitiesRouteSearchResponse
  { tflApiPresentationEntitiesRouteSearchResponseInput :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRouteSearchResponseSearchMatches :: Maybe [TflApiPresentationEntitiesRouteSearchMatch] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesRouteSearchResponse where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesRouteSearchResponse
instance ToJSON TflApiPresentationEntitiesRouteSearchResponse where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesRouteSearchResponse
instance ToSchema TflApiPresentationEntitiesRouteSearchResponse where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesRouteSearchResponse

optionsTflApiPresentationEntitiesRouteSearchResponse :: Options
optionsTflApiPresentationEntitiesRouteSearchResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesRouteSearchResponseInput", "input")
      , ("tflApiPresentationEntitiesRouteSearchResponseSearchMatches", "searchMatches")
      ]


-- | 
data TflApiPresentationEntitiesRouteSectionNaptanEntrySequence = TflApiPresentationEntitiesRouteSectionNaptanEntrySequence
  { tflApiPresentationEntitiesRouteSectionNaptanEntrySequenceOrdinal :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesRouteSectionNaptanEntrySequenceStopPoint :: Maybe TflApiPresentationEntitiesStopPoint -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesRouteSectionNaptanEntrySequence where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesRouteSectionNaptanEntrySequence
instance ToJSON TflApiPresentationEntitiesRouteSectionNaptanEntrySequence where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesRouteSectionNaptanEntrySequence
instance ToSchema TflApiPresentationEntitiesRouteSectionNaptanEntrySequence where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesRouteSectionNaptanEntrySequence

optionsTflApiPresentationEntitiesRouteSectionNaptanEntrySequence :: Options
optionsTflApiPresentationEntitiesRouteSectionNaptanEntrySequence =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesRouteSectionNaptanEntrySequenceOrdinal", "ordinal")
      , ("tflApiPresentationEntitiesRouteSectionNaptanEntrySequenceStopPoint", "stopPoint")
      ]


-- | 
data TflApiPresentationEntitiesRouteSequence = TflApiPresentationEntitiesRouteSequence
  { tflApiPresentationEntitiesRouteSequenceLineId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRouteSequenceLineName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRouteSequenceDirection :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRouteSequenceIsOutboundOnly :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesRouteSequenceMode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesRouteSequenceLineStrings :: Maybe [Text] -- ^ 
  , tflApiPresentationEntitiesRouteSequenceStations :: Maybe [TflApiPresentationEntitiesMatchedStop] -- ^ 
  , tflApiPresentationEntitiesRouteSequenceStopPointSequences :: Maybe [TflApiPresentationEntitiesStopPointSequence] -- ^ 
  , tflApiPresentationEntitiesRouteSequenceOrderedLineRoutes :: Maybe [TflApiPresentationEntitiesOrderedRoute] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesRouteSequence where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesRouteSequence
instance ToJSON TflApiPresentationEntitiesRouteSequence where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesRouteSequence
instance ToSchema TflApiPresentationEntitiesRouteSequence where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesRouteSequence

optionsTflApiPresentationEntitiesRouteSequence :: Options
optionsTflApiPresentationEntitiesRouteSequence =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesRouteSequenceLineId", "lineId")
      , ("tflApiPresentationEntitiesRouteSequenceLineName", "lineName")
      , ("tflApiPresentationEntitiesRouteSequenceDirection", "direction")
      , ("tflApiPresentationEntitiesRouteSequenceIsOutboundOnly", "isOutboundOnly")
      , ("tflApiPresentationEntitiesRouteSequenceMode", "mode")
      , ("tflApiPresentationEntitiesRouteSequenceLineStrings", "lineStrings")
      , ("tflApiPresentationEntitiesRouteSequenceStations", "stations")
      , ("tflApiPresentationEntitiesRouteSequenceStopPointSequences", "stopPointSequences")
      , ("tflApiPresentationEntitiesRouteSequenceOrderedLineRoutes", "orderedLineRoutes")
      ]


-- | 
data TflApiPresentationEntitiesSchedule = TflApiPresentationEntitiesSchedule
  { tflApiPresentationEntitiesScheduleName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesScheduleKnownJourneys :: Maybe [TflApiPresentationEntitiesKnownJourney] -- ^ 
  , tflApiPresentationEntitiesScheduleFirstJourney :: Maybe TflApiPresentationEntitiesKnownJourney -- ^ 
  , tflApiPresentationEntitiesScheduleLastJourney :: Maybe TflApiPresentationEntitiesKnownJourney -- ^ 
  , tflApiPresentationEntitiesSchedulePeriods :: Maybe [TflApiPresentationEntitiesPeriod] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesSchedule where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesSchedule
instance ToJSON TflApiPresentationEntitiesSchedule where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesSchedule
instance ToSchema TflApiPresentationEntitiesSchedule where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesSchedule

optionsTflApiPresentationEntitiesSchedule :: Options
optionsTflApiPresentationEntitiesSchedule =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesScheduleName", "name")
      , ("tflApiPresentationEntitiesScheduleKnownJourneys", "knownJourneys")
      , ("tflApiPresentationEntitiesScheduleFirstJourney", "firstJourney")
      , ("tflApiPresentationEntitiesScheduleLastJourney", "lastJourney")
      , ("tflApiPresentationEntitiesSchedulePeriods", "periods")
      ]


-- | 
data TflApiPresentationEntitiesSearchMatch = TflApiPresentationEntitiesSearchMatch
  { tflApiPresentationEntitiesSearchMatchId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesSearchMatchUrl :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesSearchMatchName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesSearchMatchLat :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesSearchMatchLon :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesSearchMatch where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesSearchMatch
instance ToJSON TflApiPresentationEntitiesSearchMatch where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesSearchMatch
instance ToSchema TflApiPresentationEntitiesSearchMatch where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesSearchMatch

optionsTflApiPresentationEntitiesSearchMatch :: Options
optionsTflApiPresentationEntitiesSearchMatch =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesSearchMatchId", "id")
      , ("tflApiPresentationEntitiesSearchMatchUrl", "url")
      , ("tflApiPresentationEntitiesSearchMatchName", "name")
      , ("tflApiPresentationEntitiesSearchMatchLat", "lat")
      , ("tflApiPresentationEntitiesSearchMatchLon", "lon")
      ]


-- | 
data TflApiPresentationEntitiesSearchResponse = TflApiPresentationEntitiesSearchResponse
  { tflApiPresentationEntitiesSearchResponseQuery :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesSearchResponseFrom :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesSearchResponsePage :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesSearchResponsePageSize :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesSearchResponseProvider :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesSearchResponseTotal :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesSearchResponseMatches :: Maybe [TflApiPresentationEntitiesSearchMatch] -- ^ 
  , tflApiPresentationEntitiesSearchResponseMaxScore :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesSearchResponse where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesSearchResponse
instance ToJSON TflApiPresentationEntitiesSearchResponse where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesSearchResponse
instance ToSchema TflApiPresentationEntitiesSearchResponse where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesSearchResponse

optionsTflApiPresentationEntitiesSearchResponse :: Options
optionsTflApiPresentationEntitiesSearchResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesSearchResponseQuery", "query")
      , ("tflApiPresentationEntitiesSearchResponseFrom", "from")
      , ("tflApiPresentationEntitiesSearchResponsePage", "page")
      , ("tflApiPresentationEntitiesSearchResponsePageSize", "pageSize")
      , ("tflApiPresentationEntitiesSearchResponseProvider", "provider")
      , ("tflApiPresentationEntitiesSearchResponseTotal", "total")
      , ("tflApiPresentationEntitiesSearchResponseMatches", "matches")
      , ("tflApiPresentationEntitiesSearchResponseMaxScore", "maxScore")
      ]


-- | 
data TflApiPresentationEntitiesServiceFrequency = TflApiPresentationEntitiesServiceFrequency
  { tflApiPresentationEntitiesServiceFrequencyLowestFrequency :: Maybe Double -- ^ 
  , tflApiPresentationEntitiesServiceFrequencyHighestFrequency :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesServiceFrequency where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesServiceFrequency
instance ToJSON TflApiPresentationEntitiesServiceFrequency where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesServiceFrequency
instance ToSchema TflApiPresentationEntitiesServiceFrequency where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesServiceFrequency

optionsTflApiPresentationEntitiesServiceFrequency :: Options
optionsTflApiPresentationEntitiesServiceFrequency =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesServiceFrequencyLowestFrequency", "lowestFrequency")
      , ("tflApiPresentationEntitiesServiceFrequencyHighestFrequency", "highestFrequency")
      ]


-- | 
data TflApiPresentationEntitiesStationInterval = TflApiPresentationEntitiesStationInterval
  { tflApiPresentationEntitiesStationIntervalId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStationIntervalIntervals :: Maybe [TflApiPresentationEntitiesInterval] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesStationInterval where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesStationInterval
instance ToJSON TflApiPresentationEntitiesStationInterval where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesStationInterval
instance ToSchema TflApiPresentationEntitiesStationInterval where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesStationInterval

optionsTflApiPresentationEntitiesStationInterval :: Options
optionsTflApiPresentationEntitiesStationInterval =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesStationIntervalId", "id")
      , ("tflApiPresentationEntitiesStationIntervalIntervals", "intervals")
      ]


-- | 
data TflApiPresentationEntitiesStatusSeverity = TflApiPresentationEntitiesStatusSeverity
  { tflApiPresentationEntitiesStatusSeverityModeName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStatusSeveritySeverityLevel :: Maybe Int -- ^ 
  , tflApiPresentationEntitiesStatusSeverityDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesStatusSeverity where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesStatusSeverity
instance ToJSON TflApiPresentationEntitiesStatusSeverity where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesStatusSeverity
instance ToSchema TflApiPresentationEntitiesStatusSeverity where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesStatusSeverity

optionsTflApiPresentationEntitiesStatusSeverity :: Options
optionsTflApiPresentationEntitiesStatusSeverity =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesStatusSeverityModeName", "modeName")
      , ("tflApiPresentationEntitiesStatusSeveritySeverityLevel", "severityLevel")
      , ("tflApiPresentationEntitiesStatusSeverityDescription", "description")
      ]


-- | 
data TflApiPresentationEntitiesStopPoint = TflApiPresentationEntitiesStopPoint
  { tflApiPresentationEntitiesStopPointNaptanId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointPlatformName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointIndicator :: Maybe Text -- ^ The indicator of the stop point e.g. \"Stop K\"
  , tflApiPresentationEntitiesStopPointStopLetter :: Maybe Text -- ^ The stop letter, if it could be cleansed from the Indicator e.g. \"K\"
  , tflApiPresentationEntitiesStopPointModes :: Maybe [Text] -- ^ 
  , tflApiPresentationEntitiesStopPointIcsCode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointSmsCode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointStopType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointStationNaptan :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointAccessibilitySummary :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointHubNaptanCode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointLines :: Maybe [TflApiPresentationEntitiesIdentifier] -- ^ 
  , tflApiPresentationEntitiesStopPointLineGroup :: Maybe [TflApiPresentationEntitiesLineGroup] -- ^ 
  , tflApiPresentationEntitiesStopPointLineModeGroups :: Maybe [TflApiPresentationEntitiesLineModeGroup] -- ^ 
  , tflApiPresentationEntitiesStopPointFullName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointNaptanMode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointStatus :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesStopPointIndividualStopId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointId :: Maybe Text -- ^ A unique identifier.
  , tflApiPresentationEntitiesStopPointUrl :: Maybe Text -- ^ The unique location of this resource.
  , tflApiPresentationEntitiesStopPointCommonName :: Maybe Text -- ^ A human readable name.
  , tflApiPresentationEntitiesStopPointDistance :: Maybe Double -- ^ The distance of the place from its search point, if this is the result              of a geographical search, otherwise zero.
  , tflApiPresentationEntitiesStopPointPlaceType :: Maybe Text -- ^ The type of Place. See /Place/Meta/placeTypes for possible values.
  , tflApiPresentationEntitiesStopPointAdditionalProperties :: Maybe [TflApiPresentationEntitiesAdditionalProperties] -- ^ A bag of additional key/value pairs with extra information about this place.
  , tflApiPresentationEntitiesStopPointChildren :: Maybe [TflApiPresentationEntitiesPlace] -- ^ 
  , tflApiPresentationEntitiesStopPointChildrenUrls :: Maybe [Text] -- ^ 
  , tflApiPresentationEntitiesStopPointLat :: Maybe Double -- ^ WGS84 latitude of the location.
  , tflApiPresentationEntitiesStopPointLon :: Maybe Double -- ^ WGS84 longitude of the location.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesStopPoint where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesStopPoint
instance ToJSON TflApiPresentationEntitiesStopPoint where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesStopPoint
instance ToSchema TflApiPresentationEntitiesStopPoint where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesStopPoint

optionsTflApiPresentationEntitiesStopPoint :: Options
optionsTflApiPresentationEntitiesStopPoint =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesStopPointNaptanId", "naptanId")
      , ("tflApiPresentationEntitiesStopPointPlatformName", "platformName")
      , ("tflApiPresentationEntitiesStopPointIndicator", "indicator")
      , ("tflApiPresentationEntitiesStopPointStopLetter", "stopLetter")
      , ("tflApiPresentationEntitiesStopPointModes", "modes")
      , ("tflApiPresentationEntitiesStopPointIcsCode", "icsCode")
      , ("tflApiPresentationEntitiesStopPointSmsCode", "smsCode")
      , ("tflApiPresentationEntitiesStopPointStopType", "stopType")
      , ("tflApiPresentationEntitiesStopPointStationNaptan", "stationNaptan")
      , ("tflApiPresentationEntitiesStopPointAccessibilitySummary", "accessibilitySummary")
      , ("tflApiPresentationEntitiesStopPointHubNaptanCode", "hubNaptanCode")
      , ("tflApiPresentationEntitiesStopPointLines", "lines")
      , ("tflApiPresentationEntitiesStopPointLineGroup", "lineGroup")
      , ("tflApiPresentationEntitiesStopPointLineModeGroups", "lineModeGroups")
      , ("tflApiPresentationEntitiesStopPointFullName", "fullName")
      , ("tflApiPresentationEntitiesStopPointNaptanMode", "naptanMode")
      , ("tflApiPresentationEntitiesStopPointStatus", "status")
      , ("tflApiPresentationEntitiesStopPointIndividualStopId", "individualStopId")
      , ("tflApiPresentationEntitiesStopPointId", "id")
      , ("tflApiPresentationEntitiesStopPointUrl", "url")
      , ("tflApiPresentationEntitiesStopPointCommonName", "commonName")
      , ("tflApiPresentationEntitiesStopPointDistance", "distance")
      , ("tflApiPresentationEntitiesStopPointPlaceType", "placeType")
      , ("tflApiPresentationEntitiesStopPointAdditionalProperties", "additionalProperties")
      , ("tflApiPresentationEntitiesStopPointChildren", "children")
      , ("tflApiPresentationEntitiesStopPointChildrenUrls", "childrenUrls")
      , ("tflApiPresentationEntitiesStopPointLat", "lat")
      , ("tflApiPresentationEntitiesStopPointLon", "lon")
      ]


-- | 
data TflApiPresentationEntitiesStopPointCategory = TflApiPresentationEntitiesStopPointCategory
  { tflApiPresentationEntitiesStopPointCategoryCategory :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointCategoryAvailableKeys :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesStopPointCategory where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesStopPointCategory
instance ToJSON TflApiPresentationEntitiesStopPointCategory where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesStopPointCategory
instance ToSchema TflApiPresentationEntitiesStopPointCategory where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesStopPointCategory

optionsTflApiPresentationEntitiesStopPointCategory :: Options
optionsTflApiPresentationEntitiesStopPointCategory =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesStopPointCategoryCategory", "category")
      , ("tflApiPresentationEntitiesStopPointCategoryAvailableKeys", "availableKeys")
      ]


-- | 
data TflApiPresentationEntitiesStopPointRouteSection = TflApiPresentationEntitiesStopPointRouteSection
  { tflApiPresentationEntitiesStopPointRouteSectionNaptanId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointRouteSectionLineId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointRouteSectionMode :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointRouteSectionValidFrom :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesStopPointRouteSectionValidTo :: Maybe UTCTime -- ^ 
  , tflApiPresentationEntitiesStopPointRouteSectionDirection :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointRouteSectionRouteSectionName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointRouteSectionLineString :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointRouteSectionIsActive :: Maybe Bool -- ^ 
  , tflApiPresentationEntitiesStopPointRouteSectionServiceType :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointRouteSectionVehicleDestinationText :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointRouteSectionDestinationName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesStopPointRouteSection where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesStopPointRouteSection
instance ToJSON TflApiPresentationEntitiesStopPointRouteSection where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesStopPointRouteSection
instance ToSchema TflApiPresentationEntitiesStopPointRouteSection where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesStopPointRouteSection

optionsTflApiPresentationEntitiesStopPointRouteSection :: Options
optionsTflApiPresentationEntitiesStopPointRouteSection =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesStopPointRouteSectionNaptanId", "naptanId")
      , ("tflApiPresentationEntitiesStopPointRouteSectionLineId", "lineId")
      , ("tflApiPresentationEntitiesStopPointRouteSectionMode", "mode")
      , ("tflApiPresentationEntitiesStopPointRouteSectionValidFrom", "validFrom")
      , ("tflApiPresentationEntitiesStopPointRouteSectionValidTo", "validTo")
      , ("tflApiPresentationEntitiesStopPointRouteSectionDirection", "direction")
      , ("tflApiPresentationEntitiesStopPointRouteSectionRouteSectionName", "routeSectionName")
      , ("tflApiPresentationEntitiesStopPointRouteSectionLineString", "lineString")
      , ("tflApiPresentationEntitiesStopPointRouteSectionIsActive", "isActive")
      , ("tflApiPresentationEntitiesStopPointRouteSectionServiceType", "serviceType")
      , ("tflApiPresentationEntitiesStopPointRouteSectionVehicleDestinationText", "vehicleDestinationText")
      , ("tflApiPresentationEntitiesStopPointRouteSectionDestinationName", "destinationName")
      ]


-- | 
data TflApiPresentationEntitiesStopPointSequence = TflApiPresentationEntitiesStopPointSequence
  { tflApiPresentationEntitiesStopPointSequenceLineId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointSequenceLineName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointSequenceDirection :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesStopPointSequenceBranchId :: Maybe Int -- ^ The id of this branch.
  , tflApiPresentationEntitiesStopPointSequenceNextBranchIds :: Maybe [Int] -- ^ The ids of the next branch(es) in the sequence. Note that the next and previous branch id can be              identical in the case of a looped route e.g. the Circle line.
  , tflApiPresentationEntitiesStopPointSequencePrevBranchIds :: Maybe [Int] -- ^ The ids of the previous branch(es) in the sequence. Note that the next and previous branch id can be              identical in the case of a looped route e.g. the Circle line.
  , tflApiPresentationEntitiesStopPointSequenceStopPoint :: Maybe [TflApiPresentationEntitiesMatchedStop] -- ^ 
  , tflApiPresentationEntitiesStopPointSequenceServiceType :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesStopPointSequence where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesStopPointSequence
instance ToJSON TflApiPresentationEntitiesStopPointSequence where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesStopPointSequence
instance ToSchema TflApiPresentationEntitiesStopPointSequence where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesStopPointSequence

optionsTflApiPresentationEntitiesStopPointSequence :: Options
optionsTflApiPresentationEntitiesStopPointSequence =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesStopPointSequenceLineId", "lineId")
      , ("tflApiPresentationEntitiesStopPointSequenceLineName", "lineName")
      , ("tflApiPresentationEntitiesStopPointSequenceDirection", "direction")
      , ("tflApiPresentationEntitiesStopPointSequenceBranchId", "branchId")
      , ("tflApiPresentationEntitiesStopPointSequenceNextBranchIds", "nextBranchIds")
      , ("tflApiPresentationEntitiesStopPointSequencePrevBranchIds", "prevBranchIds")
      , ("tflApiPresentationEntitiesStopPointSequenceStopPoint", "stopPoint")
      , ("tflApiPresentationEntitiesStopPointSequenceServiceType", "serviceType")
      ]


-- | A paged response containing StopPoints
data TflApiPresentationEntitiesStopPointsResponse = TflApiPresentationEntitiesStopPointsResponse
  { tflApiPresentationEntitiesStopPointsResponseCentrePoint :: Maybe [Double] -- ^ The centre latitude/longitude of this list of StopPoints
  , tflApiPresentationEntitiesStopPointsResponseStopPoints :: Maybe [TflApiPresentationEntitiesStopPoint] -- ^ Collection of stop points
  , tflApiPresentationEntitiesStopPointsResponsePageSize :: Maybe Int -- ^ The maximum size of the page in this response i.e. the maximum number of StopPoints
  , tflApiPresentationEntitiesStopPointsResponseTotal :: Maybe Int -- ^ The total number of StopPoints available across all pages
  , tflApiPresentationEntitiesStopPointsResponsePage :: Maybe Int -- ^ The index of this page
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesStopPointsResponse where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesStopPointsResponse
instance ToJSON TflApiPresentationEntitiesStopPointsResponse where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesStopPointsResponse
instance ToSchema TflApiPresentationEntitiesStopPointsResponse where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesStopPointsResponse

optionsTflApiPresentationEntitiesStopPointsResponse :: Options
optionsTflApiPresentationEntitiesStopPointsResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesStopPointsResponseCentrePoint", "centrePoint")
      , ("tflApiPresentationEntitiesStopPointsResponseStopPoints", "stopPoints")
      , ("tflApiPresentationEntitiesStopPointsResponsePageSize", "pageSize")
      , ("tflApiPresentationEntitiesStopPointsResponseTotal", "total")
      , ("tflApiPresentationEntitiesStopPointsResponsePage", "page")
      ]


-- | 
data TflApiPresentationEntitiesStreet = TflApiPresentationEntitiesStreet
  { tflApiPresentationEntitiesStreetName :: Maybe Text -- ^ Street name
  , tflApiPresentationEntitiesStreetClosure :: Maybe Text -- ^ Type of road closure. Some example values:              Open = road is open, not blocked, not closed, not restricted. It maybe that the disruption has been moved out of the carriageway.              Partial Closure = road is partially blocked, closed or restricted.               Full Closure = road is fully blocked or closed.
  , tflApiPresentationEntitiesStreetDirections :: Maybe Text -- ^ The direction of the disruption on the street. Some example values:              All Directions              All Approaches              Clockwise              Anti-Clockwise              Northbound              Eastbound              Southbound              Westbound              Both Directions
  , tflApiPresentationEntitiesStreetSegments :: Maybe [TflApiPresentationEntitiesStreetSegment] -- ^ Geographic description of the sections of this street that are affected.
  , tflApiPresentationEntitiesStreetSourceSystemId :: Maybe Integer -- ^ The ID from the source system of the disruption that this street belongs to.
  , tflApiPresentationEntitiesStreetSourceSystemKey :: Maybe Text -- ^ The key of the source system of the disruption that this street belongs to.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesStreet where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesStreet
instance ToJSON TflApiPresentationEntitiesStreet where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesStreet
instance ToSchema TflApiPresentationEntitiesStreet where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesStreet

optionsTflApiPresentationEntitiesStreet :: Options
optionsTflApiPresentationEntitiesStreet =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesStreetName", "name")
      , ("tflApiPresentationEntitiesStreetClosure", "closure")
      , ("tflApiPresentationEntitiesStreetDirections", "directions")
      , ("tflApiPresentationEntitiesStreetSegments", "segments")
      , ("tflApiPresentationEntitiesStreetSourceSystemId", "sourceSystemId")
      , ("tflApiPresentationEntitiesStreetSourceSystemKey", "sourceSystemKey")
      ]


-- | 
data TflApiPresentationEntitiesStreetSegment = TflApiPresentationEntitiesStreetSegment
  { tflApiPresentationEntitiesStreetSegmentToid :: Maybe Text -- ^ A 16 digit unique integer identifying a OS ITN (Ordnance Survey Integrated Transport Network) road link.
  , tflApiPresentationEntitiesStreetSegmentLineString :: Maybe Text -- ^ geoJSON formatted LineString containing two latitude/longitude (WGS84) pairs that identify the start and end points of the street segment.
  , tflApiPresentationEntitiesStreetSegmentSourceSystemId :: Maybe Integer -- ^ The ID from the source system of the disruption that this street belongs to.
  , tflApiPresentationEntitiesStreetSegmentSourceSystemKey :: Maybe Text -- ^ The key of the source system of the disruption that this street belongs to.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesStreetSegment where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesStreetSegment
instance ToJSON TflApiPresentationEntitiesStreetSegment where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesStreetSegment
instance ToSchema TflApiPresentationEntitiesStreetSegment where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesStreetSegment

optionsTflApiPresentationEntitiesStreetSegment :: Options
optionsTflApiPresentationEntitiesStreetSegment =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesStreetSegmentToid", "toid")
      , ("tflApiPresentationEntitiesStreetSegmentLineString", "lineString")
      , ("tflApiPresentationEntitiesStreetSegmentSourceSystemId", "sourceSystemId")
      , ("tflApiPresentationEntitiesStreetSegmentSourceSystemKey", "sourceSystemKey")
      ]


-- | 
data TflApiPresentationEntitiesTimetable = TflApiPresentationEntitiesTimetable
  { tflApiPresentationEntitiesTimetableDepartureStopId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesTimetableRoutes :: Maybe [TflApiPresentationEntitiesTimetableRoute] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesTimetable where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesTimetable
instance ToJSON TflApiPresentationEntitiesTimetable where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesTimetable
instance ToSchema TflApiPresentationEntitiesTimetable where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesTimetable

optionsTflApiPresentationEntitiesTimetable :: Options
optionsTflApiPresentationEntitiesTimetable =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesTimetableDepartureStopId", "departureStopId")
      , ("tflApiPresentationEntitiesTimetableRoutes", "routes")
      ]


-- | 
data TflApiPresentationEntitiesTimetableResponse = TflApiPresentationEntitiesTimetableResponse
  { tflApiPresentationEntitiesTimetableResponseLineId :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesTimetableResponseLineName :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesTimetableResponseDirection :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesTimetableResponsePdfUrl :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesTimetableResponseStations :: Maybe [TflApiPresentationEntitiesMatchedStop] -- ^ 
  , tflApiPresentationEntitiesTimetableResponseStops :: Maybe [TflApiPresentationEntitiesMatchedStop] -- ^ 
  , tflApiPresentationEntitiesTimetableResponseTimetable :: Maybe TflApiPresentationEntitiesTimetable -- ^ 
  , tflApiPresentationEntitiesTimetableResponseDisambiguation :: Maybe TflApiPresentationEntitiesTimetablesDisambiguation -- ^ 
  , tflApiPresentationEntitiesTimetableResponseStatusErrorMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesTimetableResponse where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesTimetableResponse
instance ToJSON TflApiPresentationEntitiesTimetableResponse where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesTimetableResponse
instance ToSchema TflApiPresentationEntitiesTimetableResponse where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesTimetableResponse

optionsTflApiPresentationEntitiesTimetableResponse :: Options
optionsTflApiPresentationEntitiesTimetableResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesTimetableResponseLineId", "lineId")
      , ("tflApiPresentationEntitiesTimetableResponseLineName", "lineName")
      , ("tflApiPresentationEntitiesTimetableResponseDirection", "direction")
      , ("tflApiPresentationEntitiesTimetableResponsePdfUrl", "pdfUrl")
      , ("tflApiPresentationEntitiesTimetableResponseStations", "stations")
      , ("tflApiPresentationEntitiesTimetableResponseStops", "stops")
      , ("tflApiPresentationEntitiesTimetableResponseTimetable", "timetable")
      , ("tflApiPresentationEntitiesTimetableResponseDisambiguation", "disambiguation")
      , ("tflApiPresentationEntitiesTimetableResponseStatusErrorMessage", "statusErrorMessage")
      ]


-- | 
data TflApiPresentationEntitiesTimetableRoute = TflApiPresentationEntitiesTimetableRoute
  { tflApiPresentationEntitiesTimetableRouteStationIntervals :: Maybe [TflApiPresentationEntitiesStationInterval] -- ^ 
  , tflApiPresentationEntitiesTimetableRouteSchedules :: Maybe [TflApiPresentationEntitiesSchedule] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesTimetableRoute where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesTimetableRoute
instance ToJSON TflApiPresentationEntitiesTimetableRoute where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesTimetableRoute
instance ToSchema TflApiPresentationEntitiesTimetableRoute where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesTimetableRoute

optionsTflApiPresentationEntitiesTimetableRoute :: Options
optionsTflApiPresentationEntitiesTimetableRoute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesTimetableRouteStationIntervals", "stationIntervals")
      , ("tflApiPresentationEntitiesTimetableRouteSchedules", "schedules")
      ]


-- | 
data TflApiPresentationEntitiesTimetablesDisambiguation = TflApiPresentationEntitiesTimetablesDisambiguation
  { tflApiPresentationEntitiesTimetablesDisambiguationDisambiguationOptions :: Maybe [TflApiPresentationEntitiesTimetablesDisambiguationOption] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesTimetablesDisambiguation where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesTimetablesDisambiguation
instance ToJSON TflApiPresentationEntitiesTimetablesDisambiguation where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesTimetablesDisambiguation
instance ToSchema TflApiPresentationEntitiesTimetablesDisambiguation where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesTimetablesDisambiguation

optionsTflApiPresentationEntitiesTimetablesDisambiguation :: Options
optionsTflApiPresentationEntitiesTimetablesDisambiguation =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesTimetablesDisambiguationDisambiguationOptions", "disambiguationOptions")
      ]


-- | 
data TflApiPresentationEntitiesTimetablesDisambiguationOption = TflApiPresentationEntitiesTimetablesDisambiguationOption
  { tflApiPresentationEntitiesTimetablesDisambiguationOptionDescription :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesTimetablesDisambiguationOptionUri :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesTimetablesDisambiguationOption where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesTimetablesDisambiguationOption
instance ToJSON TflApiPresentationEntitiesTimetablesDisambiguationOption where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesTimetablesDisambiguationOption
instance ToSchema TflApiPresentationEntitiesTimetablesDisambiguationOption where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesTimetablesDisambiguationOption

optionsTflApiPresentationEntitiesTimetablesDisambiguationOption :: Options
optionsTflApiPresentationEntitiesTimetablesDisambiguationOption =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesTimetablesDisambiguationOptionDescription", "description")
      , ("tflApiPresentationEntitiesTimetablesDisambiguationOptionUri", "uri")
      ]


-- | 
data TflApiPresentationEntitiesTrainLoading = TflApiPresentationEntitiesTrainLoading
  { tflApiPresentationEntitiesTrainLoadingLine :: Maybe Text -- ^ The Line Name e.g. \"Victoria\"
  , tflApiPresentationEntitiesTrainLoadingLineDirection :: Maybe Text -- ^ Direction of the Line e.g. NB, SB, WB etc.
  , tflApiPresentationEntitiesTrainLoadingPlatformDirection :: Maybe Text -- ^ Direction displayed on the platform e.g. NB, SB, WB etc.
  , tflApiPresentationEntitiesTrainLoadingDirection :: Maybe Text -- ^ Direction in regards to Journey Planner i.e. inbound or outbound
  , tflApiPresentationEntitiesTrainLoadingNaptanTo :: Maybe Text -- ^ Naptan of the adjacent station
  , tflApiPresentationEntitiesTrainLoadingTimeSlice :: Maybe Text -- ^ Time in 24hr format with 15 minute intervals e.g. 0500-0515, 0515-0530 etc.
  , tflApiPresentationEntitiesTrainLoadingValue :: Maybe Int -- ^ Scale between 1-6,                1 = Very quiet, 2 = Quiet, 3 = Fairly busy, 4 = Busy, 5 = Very busy, 6 = Exceptionally busy
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesTrainLoading where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesTrainLoading
instance ToJSON TflApiPresentationEntitiesTrainLoading where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesTrainLoading
instance ToSchema TflApiPresentationEntitiesTrainLoading where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesTrainLoading

optionsTflApiPresentationEntitiesTrainLoading :: Options
optionsTflApiPresentationEntitiesTrainLoading =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesTrainLoadingLine", "line")
      , ("tflApiPresentationEntitiesTrainLoadingLineDirection", "lineDirection")
      , ("tflApiPresentationEntitiesTrainLoadingPlatformDirection", "platformDirection")
      , ("tflApiPresentationEntitiesTrainLoadingDirection", "direction")
      , ("tflApiPresentationEntitiesTrainLoadingNaptanTo", "naptanTo")
      , ("tflApiPresentationEntitiesTrainLoadingTimeSlice", "timeSlice")
      , ("tflApiPresentationEntitiesTrainLoadingValue", "value")
      ]


-- | 
data TflApiPresentationEntitiesTwentyFourHourClockTime = TflApiPresentationEntitiesTwentyFourHourClockTime
  { tflApiPresentationEntitiesTwentyFourHourClockTimeHour :: Maybe Text -- ^ 
  , tflApiPresentationEntitiesTwentyFourHourClockTimeMinute :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesTwentyFourHourClockTime where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesTwentyFourHourClockTime
instance ToJSON TflApiPresentationEntitiesTwentyFourHourClockTime where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesTwentyFourHourClockTime
instance ToSchema TflApiPresentationEntitiesTwentyFourHourClockTime where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesTwentyFourHourClockTime

optionsTflApiPresentationEntitiesTwentyFourHourClockTime :: Options
optionsTflApiPresentationEntitiesTwentyFourHourClockTime =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesTwentyFourHourClockTimeHour", "hour")
      , ("tflApiPresentationEntitiesTwentyFourHourClockTimeMinute", "minute")
      ]


-- | Represents a period for which a planned works is valid.
data TflApiPresentationEntitiesValidityPeriod = TflApiPresentationEntitiesValidityPeriod
  { tflApiPresentationEntitiesValidityPeriodFromDate :: Maybe UTCTime -- ^ Gets or sets the start date.
  , tflApiPresentationEntitiesValidityPeriodToDate :: Maybe UTCTime -- ^ Gets or sets the end date.
  , tflApiPresentationEntitiesValidityPeriodIsNow :: Maybe Bool -- ^ If true is a realtime status rather than planned or info
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TflApiPresentationEntitiesValidityPeriod where
  parseJSON = genericParseJSON optionsTflApiPresentationEntitiesValidityPeriod
instance ToJSON TflApiPresentationEntitiesValidityPeriod where
  toJSON = genericToJSON optionsTflApiPresentationEntitiesValidityPeriod
instance ToSchema TflApiPresentationEntitiesValidityPeriod where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTflApiPresentationEntitiesValidityPeriod

optionsTflApiPresentationEntitiesValidityPeriod :: Options
optionsTflApiPresentationEntitiesValidityPeriod =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tflApiPresentationEntitiesValidityPeriodFromDate", "fromDate")
      , ("tflApiPresentationEntitiesValidityPeriodToDate", "toDate")
      , ("tflApiPresentationEntitiesValidityPeriodIsNow", "isNow")
      ]

