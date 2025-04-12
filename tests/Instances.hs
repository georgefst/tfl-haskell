{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import TransportForLondonUnified.Model
import TransportForLondonUnified.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

-- * Models
 
instance Arbitrary SystemDataSpatialDbGeography where
  arbitrary =
    SystemDataSpatialDbGeography
      <$> arbitrary -- systemDataSpatialDbGeographyGeography :: Maybe SystemDataSpatialDbGeographyWellKnownValue
    
instance Arbitrary SystemDataSpatialDbGeographyWellKnownValue where
  arbitrary =
    SystemDataSpatialDbGeographyWellKnownValue
      <$> arbitrary -- systemDataSpatialDbGeographyWellKnownValueCoordinateSystemId :: Maybe Int
      <*> arbitrary -- systemDataSpatialDbGeographyWellKnownValueWellKnownText :: Maybe Text
      <*> arbitrary -- systemDataSpatialDbGeographyWellKnownValueWellKnownBinary :: Maybe ByteArray
    
instance Arbitrary SystemObject where
  arbitrary =
    
    pure SystemObject
     
instance Arbitrary TflApiCommonApiVersionInfo where
  arbitrary =
    TflApiCommonApiVersionInfo
      <$> arbitrary -- tflApiCommonApiVersionInfoLabel :: Maybe Text
      <*> arbitrary -- tflApiCommonApiVersionInfoTimestamp :: Maybe DateTime
      <*> arbitrary -- tflApiCommonApiVersionInfoVersion :: Maybe Text
      <*> arbitrary -- tflApiCommonApiVersionInfoAssemblies :: Maybe [Text]
    
instance Arbitrary TflApiCommonDateRange where
  arbitrary =
    TflApiCommonDateRange
      <$> arbitrary -- tflApiCommonDateRangeStartDate :: Maybe DateTime
      <*> arbitrary -- tflApiCommonDateRangeEndDate :: Maybe DateTime
    
instance Arbitrary TflApiCommonDateRangeNullable where
  arbitrary =
    TflApiCommonDateRangeNullable
      <$> arbitrary -- tflApiCommonDateRangeNullableStartDate :: Maybe DateTime
      <*> arbitrary -- tflApiCommonDateRangeNullableEndDate :: Maybe DateTime
    
instance Arbitrary TflApiCommonGeoPoint where
  arbitrary =
    TflApiCommonGeoPoint
      <$> arbitrary -- tflApiCommonGeoPointLat :: Double
      <*> arbitrary -- tflApiCommonGeoPointLon :: Double
    
instance Arbitrary TflApiCommonJourneyPlannerJpElevation where
  arbitrary =
    TflApiCommonJourneyPlannerJpElevation
      <$> arbitrary -- tflApiCommonJourneyPlannerJpElevationDistance :: Maybe Int
      <*> arbitrary -- tflApiCommonJourneyPlannerJpElevationStartLat :: Maybe Double
      <*> arbitrary -- tflApiCommonJourneyPlannerJpElevationStartLon :: Maybe Double
      <*> arbitrary -- tflApiCommonJourneyPlannerJpElevationEndLat :: Maybe Double
      <*> arbitrary -- tflApiCommonJourneyPlannerJpElevationEndLon :: Maybe Double
      <*> arbitrary -- tflApiCommonJourneyPlannerJpElevationHeightFromPreviousPoint :: Maybe Int
      <*> arbitrary -- tflApiCommonJourneyPlannerJpElevationGradient :: Maybe Double
    
instance Arbitrary TflApiCommonPlaceGeo where
  arbitrary =
    TflApiCommonPlaceGeo
      <$> arbitrary -- tflApiCommonPlaceGeoSwLat :: Maybe Double
      <*> arbitrary -- tflApiCommonPlaceGeoSwLon :: Maybe Double
      <*> arbitrary -- tflApiCommonPlaceGeoNeLat :: Maybe Double
      <*> arbitrary -- tflApiCommonPlaceGeoNeLon :: Maybe Double
      <*> arbitrary -- tflApiCommonPlaceGeoLat :: Maybe Double
      <*> arbitrary -- tflApiCommonPlaceGeoLon :: Maybe Double
    
instance Arbitrary TflApiCommonPostcodeInput where
  arbitrary =
    TflApiCommonPostcodeInput
      <$> arbitrary -- tflApiCommonPostcodeInputPostcode :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesAccidentStatsAccidentDetail where
  arbitrary =
    TflApiPresentationEntitiesAccidentStatsAccidentDetail
      <$> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentDetailId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentDetailLat :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentDetailLon :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentDetailLocation :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentDetailDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentDetailSeverity :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentDetailBorough :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentDetailCasualties :: Maybe [TflApiPresentationEntitiesAccidentStatsCasualty]
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentDetailVehicles :: Maybe [TflApiPresentationEntitiesAccidentStatsVehicle]
    
instance Arbitrary TflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary where
  arbitrary =
    TflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummary
      <$> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummaryYear :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummaryBorough :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsAccidentStatsOrderedSummaryAccidents :: Maybe Int
    
instance Arbitrary TflApiPresentationEntitiesAccidentStatsCasualty where
  arbitrary =
    TflApiPresentationEntitiesAccidentStatsCasualty
      <$> arbitrary -- tflApiPresentationEntitiesAccidentStatsCasualtyAge :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsCasualtyClass :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsCasualtySeverity :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsCasualtyMode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesAccidentStatsCasualtyAgeBand :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesAccidentStatsVehicle where
  arbitrary =
    TflApiPresentationEntitiesAccidentStatsVehicle
      <$> arbitrary -- tflApiPresentationEntitiesAccidentStatsVehicleType :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesActiveServiceType where
  arbitrary =
    TflApiPresentationEntitiesActiveServiceType
      <$> arbitrary -- tflApiPresentationEntitiesActiveServiceTypeMode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesActiveServiceTypeServiceType :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesAdditionalProperties where
  arbitrary =
    TflApiPresentationEntitiesAdditionalProperties
      <$> arbitrary -- tflApiPresentationEntitiesAdditionalPropertiesCategory :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesAdditionalPropertiesKey :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesAdditionalPropertiesSourceSystemKey :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesAdditionalPropertiesValue :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesAdditionalPropertiesModified :: Maybe DateTime
    
instance Arbitrary TflApiPresentationEntitiesArrivalDeparture where
  arbitrary =
    TflApiPresentationEntitiesArrivalDeparture
      <$> arbitrary -- tflApiPresentationEntitiesArrivalDeparturePlatformName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureDestinationNaptanId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureDestinationName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureNaptanId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureStationName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureEstimatedTimeOfArrival :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureScheduledTimeOfArrival :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureEstimatedTimeOfDeparture :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureScheduledTimeOfDeparture :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureMinutesAndSecondsToArrival :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureMinutesAndSecondsToDeparture :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureCause :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureDepartureStatus :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureTiming :: Maybe TflApiPresentationEntitiesPredictionTiming
    
instance Arbitrary TflApiPresentationEntitiesArrivalDepartureWithLine where
  arbitrary =
    TflApiPresentationEntitiesArrivalDepartureWithLine
      <$> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineLineId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineLineName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineVehicleId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLinePlatformName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineDestinationNaptanId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineDestinationName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineNaptanId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineStationName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineEstimatedTimeOfArrival :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineScheduledTimeOfArrival :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineEstimatedTimeOfDeparture :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineScheduledTimeOfDeparture :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineMinutesAndSecondsToArrival :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineMinutesAndSecondsToDeparture :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineCause :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineDepartureStatus :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesArrivalDepartureWithLineTiming :: Maybe TflApiPresentationEntitiesPredictionTiming
    
instance Arbitrary TflApiPresentationEntitiesBay where
  arbitrary =
    TflApiPresentationEntitiesBay
      <$> arbitrary -- tflApiPresentationEntitiesBayBayType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesBayBayCount :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesBayFree :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesBayOccupied :: Maybe Int
    
instance Arbitrary TflApiPresentationEntitiesBikePointOccupancy where
  arbitrary =
    TflApiPresentationEntitiesBikePointOccupancy
      <$> arbitrary -- tflApiPresentationEntitiesBikePointOccupancyId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesBikePointOccupancyName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesBikePointOccupancyBikesCount :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesBikePointOccupancyEmptyDocks :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesBikePointOccupancyTotalDocks :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesBikePointOccupancyStandardBikesCount :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesBikePointOccupancyEBikesCount :: Maybe Int
    
instance Arbitrary TflApiPresentationEntitiesCarParkOccupancy where
  arbitrary =
    TflApiPresentationEntitiesCarParkOccupancy
      <$> arbitrary -- tflApiPresentationEntitiesCarParkOccupancyId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesCarParkOccupancyBays :: Maybe [TflApiPresentationEntitiesBay]
      <*> arbitrary -- tflApiPresentationEntitiesCarParkOccupancyName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesCarParkOccupancyCarParkDetailsUrl :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesChargeConnectorOccupancy where
  arbitrary =
    TflApiPresentationEntitiesChargeConnectorOccupancy
      <$> arbitrary -- tflApiPresentationEntitiesChargeConnectorOccupancyId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesChargeConnectorOccupancySourceSystemPlaceId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesChargeConnectorOccupancyStatus :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesCoordinate where
  arbitrary =
    TflApiPresentationEntitiesCoordinate
      <$> arbitrary -- tflApiPresentationEntitiesCoordinateLongitude :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesCoordinateLatitude :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesCoordinateEasting :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesCoordinateNorthing :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesCoordinateXCoord :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesCoordinateYCoord :: Maybe Int
    
instance Arbitrary TflApiPresentationEntitiesCrowding where
  arbitrary =
    TflApiPresentationEntitiesCrowding
      <$> arbitrary -- tflApiPresentationEntitiesCrowdingPassengerFlows :: Maybe [TflApiPresentationEntitiesPassengerFlow]
      <*> arbitrary -- tflApiPresentationEntitiesCrowdingTrainLoadings :: Maybe [TflApiPresentationEntitiesTrainLoading]
    
instance Arbitrary TflApiPresentationEntitiesCycleSuperhighway where
  arbitrary =
    TflApiPresentationEntitiesCycleSuperhighway
      <$> arbitrary -- tflApiPresentationEntitiesCycleSuperhighwayId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesCycleSuperhighwayLabel :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesCycleSuperhighwayLabelShort :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesCycleSuperhighwayGeography :: Maybe SystemDataSpatialDbGeography
      <*> arbitrary -- tflApiPresentationEntitiesCycleSuperhighwaySegmented :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesCycleSuperhighwayModified :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesCycleSuperhighwayStatus :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesCycleSuperhighwayRouteType :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesDisruptedPoint where
  arbitrary =
    TflApiPresentationEntitiesDisruptedPoint
      <$> arbitrary -- tflApiPresentationEntitiesDisruptedPointAtcoCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedPointFromDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedPointToDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedPointDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedPointCommonName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedPointType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedPointMode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedPointStationAtcoCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedPointAppearance :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedPointAdditionalInformation :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesDisruptedRoute where
  arbitrary =
    TflApiPresentationEntitiesDisruptedRoute
      <$> arbitrary -- tflApiPresentationEntitiesDisruptedRouteId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteLineId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteRouteCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteLineString :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteOriginationName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteDestinationName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteVia :: Maybe TflApiPresentationEntitiesRouteSectionNaptanEntrySequence
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteIsEntireRouteSection :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteValidTo :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteValidFrom :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesDisruptedRouteRouteSectionNaptanEntrySequence :: Maybe [TflApiPresentationEntitiesRouteSectionNaptanEntrySequence]
    
instance Arbitrary TflApiPresentationEntitiesDisruption where
  arbitrary =
    TflApiPresentationEntitiesDisruption
      <$> arbitrary -- tflApiPresentationEntitiesDisruptionCategory :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptionType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptionCategoryDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptionDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptionSummary :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptionAdditionalInfo :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesDisruptionCreated :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesDisruptionLastUpdate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesDisruptionAffectedRoutes :: Maybe [TflApiPresentationEntitiesDisruptedRoute]
      <*> arbitrary -- tflApiPresentationEntitiesDisruptionAffectedStops :: Maybe [TflApiPresentationEntitiesStopPoint]
      <*> arbitrary -- tflApiPresentationEntitiesDisruptionClosureText :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesFaresFare where
  arbitrary =
    TflApiPresentationEntitiesFaresFare
      <$> arbitrary -- tflApiPresentationEntitiesFaresFareId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresFarePassengerType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareValidFrom :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareValidUntil :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareTicketTime :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareTicketType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareCost :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareCap :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareZone :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareMode :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesFaresFareBounds where
  arbitrary =
    TflApiPresentationEntitiesFaresFareBounds
      <$> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsFrom :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsTo :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsVia :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsRouteCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsDisplayName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsOperator :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsDisplayOrder :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsIsPopularFare :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsIsPopularTravelCard :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsIsTour :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareBoundsMessages :: Maybe [TflApiPresentationEntitiesMessage]
    
instance Arbitrary TflApiPresentationEntitiesFaresFareDetails where
  arbitrary =
    TflApiPresentationEntitiesFaresFareDetails
      <$> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsBoundsId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsStartDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsEndDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsMode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsPassengerType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsContactlessPaygOnlyFare :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsFrom :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsTo :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsFromStation :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsToStation :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsVia :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsRouteCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsDisplayName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsDisplayOrder :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsRouteDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsValidatorInformation :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsOperator :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsSpecialFare :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsThroughFare :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsIsTour :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsTicketsAvailable :: Maybe [TflApiPresentationEntitiesFaresTicket]
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareDetailsMessages :: Maybe [TflApiPresentationEntitiesMessage]
    
instance Arbitrary TflApiPresentationEntitiesFaresFareStation where
  arbitrary =
    TflApiPresentationEntitiesFaresFareStation
      <$> arbitrary -- tflApiPresentationEntitiesFaresFareStationAtcoCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareStationCommonName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFareStationFareCategory :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesFaresFaresMode where
  arbitrary =
    TflApiPresentationEntitiesFaresFaresMode
      <$> arbitrary -- tflApiPresentationEntitiesFaresFaresModeId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresFaresModeName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFaresModeDescription :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesFaresFaresPeriod where
  arbitrary =
    TflApiPresentationEntitiesFaresFaresPeriod
      <$> arbitrary -- tflApiPresentationEntitiesFaresFaresPeriodId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresFaresPeriodStartDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesFaresFaresPeriodViewableDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesFaresFaresPeriodEndDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesFaresFaresPeriodIsFuture :: Maybe Bool
    
instance Arbitrary TflApiPresentationEntitiesFaresFaresSection where
  arbitrary =
    TflApiPresentationEntitiesFaresFaresSection
      <$> arbitrary -- tflApiPresentationEntitiesFaresFaresSectionHeader :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresFaresSectionIndex :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresFaresSectionJourney :: Maybe TflApiPresentationEntitiesFaresJourney
      <*> arbitrary -- tflApiPresentationEntitiesFaresFaresSectionRows :: Maybe [TflApiPresentationEntitiesFaresFareDetails]
      <*> arbitrary -- tflApiPresentationEntitiesFaresFaresSectionMessages :: Maybe [TflApiPresentationEntitiesMessage]
    
instance Arbitrary TflApiPresentationEntitiesFaresJourney where
  arbitrary =
    TflApiPresentationEntitiesFaresJourney
      <$> arbitrary -- tflApiPresentationEntitiesFaresJourneyFromStation :: Maybe TflApiPresentationEntitiesFaresFareStation
      <*> arbitrary -- tflApiPresentationEntitiesFaresJourneyToStation :: Maybe TflApiPresentationEntitiesFaresFareStation
    
instance Arbitrary TflApiPresentationEntitiesFaresPassengerType where
  arbitrary =
    TflApiPresentationEntitiesFaresPassengerType
      <$> arbitrary -- tflApiPresentationEntitiesFaresPassengerTypeType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresPassengerTypeDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresPassengerTypeDisplayName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresPassengerTypeDisplayOrder :: Maybe Int
    
instance Arbitrary TflApiPresentationEntitiesFaresRecommendation where
  arbitrary =
    TflApiPresentationEntitiesFaresRecommendation
      <$> arbitrary -- tflApiPresentationEntitiesFaresRecommendationId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationRule :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationRank :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationFareType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationProduct :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationTicketType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationTicketTime :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationProductType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationDiscountCard :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationZones :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationCost :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationPriceDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationPriceComparison :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationRecommendedTopUp :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationNotes :: Maybe [TflApiPresentationEntitiesMessage]
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationKeyFeatures :: Maybe [TflApiPresentationEntitiesMessage]
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationGettingYourTicket :: Maybe [TflApiPresentationEntitiesMessage]
      <*> arbitrary -- tflApiPresentationEntitiesFaresRecommendationSingleFare :: Maybe Double
    
instance Arbitrary TflApiPresentationEntitiesFaresRecommendationResponse where
  arbitrary =
    TflApiPresentationEntitiesFaresRecommendationResponse
      <$> arbitrary -- tflApiPresentationEntitiesFaresRecommendationResponseRecommendations :: Maybe [TflApiPresentationEntitiesFaresRecommendation]
    
instance Arbitrary TflApiPresentationEntitiesFaresTicket where
  arbitrary =
    TflApiPresentationEntitiesFaresTicket
      <$> arbitrary -- tflApiPresentationEntitiesFaresTicketPassengerType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresTicketTicketType :: Maybe TflApiPresentationEntitiesFaresTicketType
      <*> arbitrary -- tflApiPresentationEntitiesFaresTicketTicketTime :: Maybe TflApiPresentationEntitiesFaresTicketTime
      <*> arbitrary -- tflApiPresentationEntitiesFaresTicketCost :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresTicketDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresTicketMode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresTicketDisplayOrder :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesFaresTicketMessages :: Maybe [TflApiPresentationEntitiesMessage]
    
instance Arbitrary TflApiPresentationEntitiesFaresTicketTime where
  arbitrary =
    TflApiPresentationEntitiesFaresTicketTime
      <$> arbitrary -- tflApiPresentationEntitiesFaresTicketTimeType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresTicketTimeDescription :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesFaresTicketType where
  arbitrary =
    TflApiPresentationEntitiesFaresTicketType
      <$> arbitrary -- tflApiPresentationEntitiesFaresTicketTypeType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesFaresTicketTypeDescription :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesGeoCodeSearchMatch where
  arbitrary =
    TflApiPresentationEntitiesGeoCodeSearchMatch
      <$> arbitrary -- tflApiPresentationEntitiesGeoCodeSearchMatchTypes :: Maybe [Text]
      <*> arbitrary -- tflApiPresentationEntitiesGeoCodeSearchMatchAddress :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesGeoCodeSearchMatchId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesGeoCodeSearchMatchUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesGeoCodeSearchMatchName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesGeoCodeSearchMatchLat :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesGeoCodeSearchMatchLon :: Maybe Double
    
instance Arbitrary TflApiPresentationEntitiesIdentifier where
  arbitrary =
    TflApiPresentationEntitiesIdentifier
      <$> arbitrary -- tflApiPresentationEntitiesIdentifierId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesIdentifierName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesIdentifierUri :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesIdentifierFullName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesIdentifierType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesIdentifierCrowding :: Maybe TflApiPresentationEntitiesCrowding
      <*> arbitrary -- tflApiPresentationEntitiesIdentifierRouteType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesIdentifierStatus :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesIdentifierMotType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesIdentifierNetwork :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesInstruction where
  arbitrary =
    TflApiPresentationEntitiesInstruction
      <$> arbitrary -- tflApiPresentationEntitiesInstructionSummary :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesInstructionDetailed :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesInstructionSteps :: Maybe [TflApiPresentationEntitiesInstructionStep]
    
instance Arbitrary TflApiPresentationEntitiesInstructionStep where
  arbitrary =
    TflApiPresentationEntitiesInstructionStep
      <$> arbitrary -- tflApiPresentationEntitiesInstructionStepDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepTurnDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepStreetName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepDistance :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepCumulativeDistance :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepSkyDirection :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepSkyDirectionDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepCumulativeTravelTime :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepLatitude :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepLongitude :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepPathAttribute :: Maybe TflApiPresentationEntitiesPathAttribute
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepDescriptionHeading :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepTrackType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepTravelTime :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesInstructionStepAtcoCode :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesInterval where
  arbitrary =
    TflApiPresentationEntitiesInterval
      <$> arbitrary -- tflApiPresentationEntitiesIntervalStopId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesIntervalTimeToArrival :: Maybe Double
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerFare where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerFare
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareLowZone :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareHighZone :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareCost :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareChargeProfileName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareIsHopperFare :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareChargeLevel :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFarePeak :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareOffPeak :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareTaps :: Maybe [TflApiPresentationEntitiesJourneyPlannerFareTap]
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerFareCaveat where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerFareCaveat
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareCaveatText :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareCaveatType :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerFareTap where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerFareTap
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareTapAtcoCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareTapTapDetails :: Maybe TflApiPresentationEntitiesJourneyPlannerFareTapDetails
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerFareTapDetails where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerFareTapDetails
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareTapDetailsModeType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareTapDetailsValidationType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareTapDetailsHostDeviceType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareTapDetailsBusRouteId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareTapDetailsNationalLocationCode :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerFareTapDetailsTapTimestamp :: Maybe DateTime
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerItineraryResult where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerItineraryResult
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerItineraryResultJourneys :: Maybe [TflApiPresentationEntitiesJourneyPlannerJourney]
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerItineraryResultLines :: Maybe [TflApiPresentationEntitiesLine]
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerItineraryResultCycleHireDockingStationData :: Maybe TflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerItineraryResultStopMessages :: Maybe [Text]
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerItineraryResultRecommendedMaxAgeMinutes :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerItineraryResultSearchCriteria :: Maybe TflApiPresentationEntitiesJourneyPlannerSearchCriteria
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerItineraryResultJourneyVector :: Maybe TflApiPresentationEntitiesJourneyPlannerJourneyVector
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerJourney where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerJourney
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyStartDateTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyDuration :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyArrivalDateTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyAlternativeRoute :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyLegs :: Maybe [TflApiPresentationEntitiesJourneyPlannerLeg]
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyFare :: Maybe TflApiPresentationEntitiesJourneyPlannerJourneyFare
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerJourneyFare where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerJourneyFare
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyFareTotalCost :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyFareFares :: Maybe [TflApiPresentationEntitiesJourneyPlannerFare]
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyFareCaveats :: Maybe [TflApiPresentationEntitiesJourneyPlannerFareCaveat]
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationData
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataOriginNumberOfBikes :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataDestinationNumberOfBikes :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataOriginNumberOfEmptySlots :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataDestinationNumberOfEmptySlots :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataOriginId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyPlannerCycleHireDockingStationDataDestinationId :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerJourneyVector where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerJourneyVector
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyVectorFrom :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyVectorTo :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyVectorVia :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerJourneyVectorUri :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerLeg where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerLeg
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegDuration :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegSpeed :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegInstruction :: Maybe TflApiPresentationEntitiesInstruction
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegObstacles :: Maybe [TflApiPresentationEntitiesJourneyPlannerObstacle]
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegDepartureTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegArrivalTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegDeparturePoint :: Maybe TflApiPresentationEntitiesPoint
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegArrivalPoint :: Maybe TflApiPresentationEntitiesPoint
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegPath :: Maybe TflApiPresentationEntitiesJourneyPlannerPath
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegRouteOptions :: Maybe [TflApiPresentationEntitiesJourneyPlannerRouteOption]
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegMode :: Maybe TflApiPresentationEntitiesIdentifier
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegDisruptions :: Maybe [TflApiPresentationEntitiesDisruption]
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegPlannedWorks :: Maybe [TflApiPresentationEntitiesJourneyPlannerPlannedWork]
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegDistance :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegIsDisrupted :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegHasFixedLocations :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegScheduledDepartureTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegScheduledArrivalTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegInterChangeDuration :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerLegInterChangePosition :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerObstacle where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerObstacle
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerObstacleType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerObstacleIncline :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerObstacleStopId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerObstaclePosition :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerPath where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerPath
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerPathLineString :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerPathStopPoints :: Maybe [TflApiPresentationEntitiesIdentifier]
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerPathElevation :: Maybe [TflApiCommonJourneyPlannerJpElevation]
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerPlannedWork where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerPlannedWork
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerPlannedWorkId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerPlannedWorkDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerPlannedWorkCreatedDateTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerPlannedWorkLastUpdateDateTime :: Maybe DateTime
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerRouteOption where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerRouteOption
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerRouteOptionId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerRouteOptionName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerRouteOptionDirections :: Maybe [Text]
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerRouteOptionLineIdentifier :: Maybe TflApiPresentationEntitiesIdentifier
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerRouteOptionDirection :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerSearchCriteria where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerSearchCriteria
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerSearchCriteriaDateTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerSearchCriteriaDateTimeType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerSearchCriteriaTimeAdjustments :: Maybe TflApiPresentationEntitiesJourneyPlannerTimeAdjustments
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerTimeAdjustment where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerTimeAdjustment
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentDate :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentTime :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentTimeIs :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentUri :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesJourneyPlannerTimeAdjustments where
  arbitrary =
    TflApiPresentationEntitiesJourneyPlannerTimeAdjustments
      <$> arbitrary -- tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsEarliest :: Maybe TflApiPresentationEntitiesJourneyPlannerTimeAdjustment
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsEarlier :: Maybe TflApiPresentationEntitiesJourneyPlannerTimeAdjustment
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsLater :: Maybe TflApiPresentationEntitiesJourneyPlannerTimeAdjustment
      <*> arbitrary -- tflApiPresentationEntitiesJourneyPlannerTimeAdjustmentsLatest :: Maybe TflApiPresentationEntitiesJourneyPlannerTimeAdjustment
    
instance Arbitrary TflApiPresentationEntitiesKnownJourney where
  arbitrary =
    TflApiPresentationEntitiesKnownJourney
      <$> arbitrary -- tflApiPresentationEntitiesKnownJourneyHour :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesKnownJourneyMinute :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesKnownJourneyIntervalId :: Maybe Int
    
instance Arbitrary TflApiPresentationEntitiesLine where
  arbitrary =
    TflApiPresentationEntitiesLine
      <$> arbitrary -- tflApiPresentationEntitiesLineId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineModeName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineDisruptions :: Maybe [TflApiPresentationEntitiesDisruption]
      <*> arbitrary -- tflApiPresentationEntitiesLineCreated :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesLineModified :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesLineLineStatuses :: Maybe [TflApiPresentationEntitiesLineStatus]
      <*> arbitrary -- tflApiPresentationEntitiesLineRouteSections :: Maybe [TflApiPresentationEntitiesMatchedRoute]
      <*> arbitrary -- tflApiPresentationEntitiesLineServiceTypes :: Maybe [TflApiPresentationEntitiesLineServiceTypeInfo]
      <*> arbitrary -- tflApiPresentationEntitiesLineCrowding :: Maybe TflApiPresentationEntitiesCrowding
    
instance Arbitrary TflApiPresentationEntitiesLineGroup where
  arbitrary =
    TflApiPresentationEntitiesLineGroup
      <$> arbitrary -- tflApiPresentationEntitiesLineGroupNaptanIdReference :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineGroupStationAtcoCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineGroupLineIdentifier :: Maybe [Text]
    
instance Arbitrary TflApiPresentationEntitiesLineModeGroup where
  arbitrary =
    TflApiPresentationEntitiesLineModeGroup
      <$> arbitrary -- tflApiPresentationEntitiesLineModeGroupModeName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineModeGroupLineIdentifier :: Maybe [Text]
    
instance Arbitrary TflApiPresentationEntitiesLineRouteSection where
  arbitrary =
    TflApiPresentationEntitiesLineRouteSection
      <$> arbitrary -- tflApiPresentationEntitiesLineRouteSectionRouteId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesLineRouteSectionDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineRouteSectionDestination :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineRouteSectionFromStation :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineRouteSectionToStation :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineRouteSectionServiceType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineRouteSectionVehicleDestinationText :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesLineServiceType where
  arbitrary =
    TflApiPresentationEntitiesLineServiceType
      <$> arbitrary -- tflApiPresentationEntitiesLineServiceTypeLineName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineServiceTypeLineSpecificServiceTypes :: Maybe [TflApiPresentationEntitiesLineSpecificServiceType]
    
instance Arbitrary TflApiPresentationEntitiesLineServiceTypeInfo where
  arbitrary =
    TflApiPresentationEntitiesLineServiceTypeInfo
      <$> arbitrary -- tflApiPresentationEntitiesLineServiceTypeInfoName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineServiceTypeInfoUri :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesLineSpecificServiceType where
  arbitrary =
    TflApiPresentationEntitiesLineSpecificServiceType
      <$> arbitrary -- tflApiPresentationEntitiesLineSpecificServiceTypeServiceType :: Maybe TflApiPresentationEntitiesLineServiceTypeInfo
      <*> arbitrary -- tflApiPresentationEntitiesLineSpecificServiceTypeStopServesServiceType :: Maybe Bool
    
instance Arbitrary TflApiPresentationEntitiesLineStatus where
  arbitrary =
    TflApiPresentationEntitiesLineStatus
      <$> arbitrary -- tflApiPresentationEntitiesLineStatusId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesLineStatusLineId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineStatusStatusSeverity :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesLineStatusStatusSeverityDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineStatusReason :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesLineStatusCreated :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesLineStatusModified :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesLineStatusValidityPeriods :: Maybe [TflApiPresentationEntitiesValidityPeriod]
      <*> arbitrary -- tflApiPresentationEntitiesLineStatusDisruption :: Maybe TflApiPresentationEntitiesDisruption
    
instance Arbitrary TflApiPresentationEntitiesMatchedRoute where
  arbitrary =
    TflApiPresentationEntitiesMatchedRoute
      <$> arbitrary -- tflApiPresentationEntitiesMatchedRouteRouteCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedRouteName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedRouteDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedRouteOriginationName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedRouteDestinationName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedRouteOriginator :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedRouteDestination :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedRouteServiceType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedRouteValidTo :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesMatchedRouteValidFrom :: Maybe DateTime
    
instance Arbitrary TflApiPresentationEntitiesMatchedRouteSections where
  arbitrary =
    TflApiPresentationEntitiesMatchedRouteSections
      <$> arbitrary -- tflApiPresentationEntitiesMatchedRouteSectionsId :: Maybe Int
    
instance Arbitrary TflApiPresentationEntitiesMatchedStop where
  arbitrary =
    TflApiPresentationEntitiesMatchedStop
      <$> arbitrary -- tflApiPresentationEntitiesMatchedStopRouteId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopParentId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopStationId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopIcsId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopTopMostParentId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopTowards :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopModes :: Maybe [Text]
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopStopType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopStopLetter :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopZone :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopAccessibilitySummary :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopHasDisruption :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopLines :: Maybe [TflApiPresentationEntitiesIdentifier]
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopStatus :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopLat :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesMatchedStopLon :: Maybe Double
    
instance Arbitrary TflApiPresentationEntitiesMessage where
  arbitrary =
    TflApiPresentationEntitiesMessage
      <$> arbitrary -- tflApiPresentationEntitiesMessageBulletOrder :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesMessageHeader :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesMessageMessageText :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMessageLinkText :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesMessageUrl :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesMode where
  arbitrary =
    TflApiPresentationEntitiesMode
      <$> arbitrary -- tflApiPresentationEntitiesModeIsTflService :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesModeIsFarePaying :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesModeIsScheduledService :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesModeModeName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesModeMotType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesModeNetwork :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesNetworkStatus where
  arbitrary =
    TflApiPresentationEntitiesNetworkStatus
      <$> arbitrary -- tflApiPresentationEntitiesNetworkStatusOperator :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesNetworkStatusStatus :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesNetworkStatusMessage :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesNetworkStatusStatusLevel :: Maybe Int
    
instance Arbitrary TflApiPresentationEntitiesOrderedRoute where
  arbitrary =
    TflApiPresentationEntitiesOrderedRoute
      <$> arbitrary -- tflApiPresentationEntitiesOrderedRouteName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesOrderedRouteNaptanIds :: Maybe [Text]
      <*> arbitrary -- tflApiPresentationEntitiesOrderedRouteServiceType :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesPassengerFlow where
  arbitrary =
    TflApiPresentationEntitiesPassengerFlow
      <$> arbitrary -- tflApiPresentationEntitiesPassengerFlowTimeSlice :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPassengerFlowValue :: Maybe Int
    
instance Arbitrary TflApiPresentationEntitiesPathAttribute where
  arbitrary =
    TflApiPresentationEntitiesPathAttribute
      <$> arbitrary -- tflApiPresentationEntitiesPathAttributeName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPathAttributeValue :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesPeriod where
  arbitrary =
    TflApiPresentationEntitiesPeriod
      <$> arbitrary -- tflApiPresentationEntitiesPeriodType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPeriodFromTime :: Maybe TflApiPresentationEntitiesTwentyFourHourClockTime
      <*> arbitrary -- tflApiPresentationEntitiesPeriodToTime :: Maybe TflApiPresentationEntitiesTwentyFourHourClockTime
      <*> arbitrary -- tflApiPresentationEntitiesPeriodFrequency :: Maybe TflApiPresentationEntitiesServiceFrequency
    
instance Arbitrary TflApiPresentationEntitiesPlace where
  arbitrary =
    TflApiPresentationEntitiesPlace
      <$> arbitrary -- tflApiPresentationEntitiesPlaceId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPlaceUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPlaceCommonName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPlaceDistance :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesPlacePlaceType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPlaceAdditionalProperties :: Maybe [TflApiPresentationEntitiesAdditionalProperties]
      <*> arbitrary -- tflApiPresentationEntitiesPlaceChildren :: Maybe [TflApiPresentationEntitiesPlace]
      <*> arbitrary -- tflApiPresentationEntitiesPlaceChildrenUrls :: Maybe [Text]
      <*> arbitrary -- tflApiPresentationEntitiesPlaceLat :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesPlaceLon :: Maybe Double
    
instance Arbitrary TflApiPresentationEntitiesPlaceCategory where
  arbitrary =
    TflApiPresentationEntitiesPlaceCategory
      <$> arbitrary -- tflApiPresentationEntitiesPlaceCategoryCategory :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPlaceCategoryAvailableKeys :: Maybe [Text]
    
instance Arbitrary TflApiPresentationEntitiesPlacePolygon where
  arbitrary =
    TflApiPresentationEntitiesPlacePolygon
      <$> arbitrary -- tflApiPresentationEntitiesPlacePolygonGeoPoints :: Maybe [TflApiCommonGeoPoint]
      <*> arbitrary -- tflApiPresentationEntitiesPlacePolygonCommonName :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesPoint where
  arbitrary =
    TflApiPresentationEntitiesPoint
      <$> arbitrary -- tflApiPresentationEntitiesPointLat :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesPointLon :: Maybe Double
    
instance Arbitrary TflApiPresentationEntitiesPrediction where
  arbitrary =
    TflApiPresentationEntitiesPrediction
      <$> arbitrary -- tflApiPresentationEntitiesPredictionId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionOperationType :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesPredictionVehicleId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionNaptanId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionStationName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionLineId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionLineName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionPlatformName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionBearing :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionTripId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionBaseVersion :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionDestinationNaptanId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionDestinationName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionTimestamp :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesPredictionTimeToStation :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesPredictionCurrentLocation :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionTowards :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionExpectedArrival :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesPredictionTimeToLive :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesPredictionModeName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionTiming :: Maybe TflApiPresentationEntitiesPredictionTiming
    
instance Arbitrary TflApiPresentationEntitiesPredictionTiming where
  arbitrary =
    TflApiPresentationEntitiesPredictionTiming
      <$> arbitrary -- tflApiPresentationEntitiesPredictionTimingCountdownServerAdjustment :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesPredictionTimingSource :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesPredictionTimingInsert :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesPredictionTimingRead :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesPredictionTimingSent :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesPredictionTimingReceived :: Maybe DateTime
    
instance Arbitrary TflApiPresentationEntitiesRedirect where
  arbitrary =
    TflApiPresentationEntitiesRedirect
      <$> arbitrary -- tflApiPresentationEntitiesRedirectShortUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRedirectLongUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRedirectActive :: Maybe Bool
    
instance Arbitrary TflApiPresentationEntitiesRoadCorridor where
  arbitrary =
    TflApiPresentationEntitiesRoadCorridor
      <$> arbitrary -- tflApiPresentationEntitiesRoadCorridorId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadCorridorDisplayName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadCorridorGroup :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadCorridorStatusSeverity :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadCorridorStatusSeverityDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadCorridorBounds :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadCorridorEnvelope :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadCorridorStatusAggregationStartDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadCorridorStatusAggregationEndDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadCorridorUrl :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesRoadDisruption where
  arbitrary =
    TflApiPresentationEntitiesRoadDisruption
      <$> arbitrary -- tflApiPresentationEntitiesRoadDisruptionId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionPoint :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionSeverity :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionOrdinal :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionCategory :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionSubCategory :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionComments :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionCurrentUpdate :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionCurrentUpdateDateTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionCorridorIds :: Maybe [Text]
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionStartDateTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionEndDateTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLastModifiedTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLevelOfInterest :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLocation :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionStatus :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionGeography :: Maybe SystemDataSpatialDbGeography
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionGeometry :: Maybe SystemDataSpatialDbGeography
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionStreets :: Maybe [TflApiPresentationEntitiesStreet]
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionIsProvisional :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionHasClosures :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLinkText :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLinkUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionRoadProject :: Maybe TflApiPresentationEntitiesRoadProject
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionPublishStartDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionPublishEndDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionTimeFrame :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionRoadDisruptionLines :: Maybe [TflApiPresentationEntitiesRoadDisruptionLine]
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionRoadDisruptionImpactAreas :: Maybe [TflApiPresentationEntitiesRoadDisruptionImpactArea]
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionRecurringSchedules :: Maybe [TflApiPresentationEntitiesRoadDisruptionSchedule]
    
instance Arbitrary TflApiPresentationEntitiesRoadDisruptionImpactArea where
  arbitrary =
    TflApiPresentationEntitiesRoadDisruptionImpactArea
      <$> arbitrary -- tflApiPresentationEntitiesRoadDisruptionImpactAreaId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionImpactAreaRoadDisruptionId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionImpactAreaPolygon :: Maybe SystemDataSpatialDbGeography
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionImpactAreaStartDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionImpactAreaEndDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionImpactAreaStartTime :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionImpactAreaEndTime :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesRoadDisruptionLine where
  arbitrary =
    TflApiPresentationEntitiesRoadDisruptionLine
      <$> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLineId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLineRoadDisruptionId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLineIsDiversion :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLineMultiLineString :: Maybe SystemDataSpatialDbGeography
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLineStartDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLineEndDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLineStartTime :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionLineEndTime :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesRoadDisruptionSchedule where
  arbitrary =
    TflApiPresentationEntitiesRoadDisruptionSchedule
      <$> arbitrary -- tflApiPresentationEntitiesRoadDisruptionScheduleStartTime :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadDisruptionScheduleEndTime :: Maybe DateTime
    
instance Arbitrary TflApiPresentationEntitiesRoadProject where
  arbitrary =
    TflApiPresentationEntitiesRoadProject
      <$> arbitrary -- tflApiPresentationEntitiesRoadProjectProjectId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectSchemeName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectProjectName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectProjectDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectProjectPageUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectConsultationPageUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectConsultationStartDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectConsultationEndDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectConstructionStartDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectConstructionEndDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectBoroughsBenefited :: Maybe [Text]
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectCycleSuperhighwayId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectPhase :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectContactName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectContactEmail :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectExternalPageUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRoadProjectProjectSummaryPageUrl :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesRouteSearchMatch where
  arbitrary =
    TflApiPresentationEntitiesRouteSearchMatch
      <$> arbitrary -- tflApiPresentationEntitiesRouteSearchMatchLineId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRouteSearchMatchMode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRouteSearchMatchLineName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRouteSearchMatchLineRouteSection :: Maybe [TflApiPresentationEntitiesLineRouteSection]
      <*> arbitrary -- tflApiPresentationEntitiesRouteSearchMatchMatchedRouteSections :: Maybe [TflApiPresentationEntitiesMatchedRouteSections]
      <*> arbitrary -- tflApiPresentationEntitiesRouteSearchMatchMatchedStops :: Maybe [TflApiPresentationEntitiesMatchedStop]
      <*> arbitrary -- tflApiPresentationEntitiesRouteSearchMatchId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRouteSearchMatchUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRouteSearchMatchName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRouteSearchMatchLat :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesRouteSearchMatchLon :: Maybe Double
    
instance Arbitrary TflApiPresentationEntitiesRouteSearchResponse where
  arbitrary =
    TflApiPresentationEntitiesRouteSearchResponse
      <$> arbitrary -- tflApiPresentationEntitiesRouteSearchResponseInput :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRouteSearchResponseSearchMatches :: Maybe [TflApiPresentationEntitiesRouteSearchMatch]
    
instance Arbitrary TflApiPresentationEntitiesRouteSectionNaptanEntrySequence where
  arbitrary =
    TflApiPresentationEntitiesRouteSectionNaptanEntrySequence
      <$> arbitrary -- tflApiPresentationEntitiesRouteSectionNaptanEntrySequenceOrdinal :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesRouteSectionNaptanEntrySequenceStopPoint :: Maybe TflApiPresentationEntitiesStopPoint
    
instance Arbitrary TflApiPresentationEntitiesRouteSequence where
  arbitrary =
    TflApiPresentationEntitiesRouteSequence
      <$> arbitrary -- tflApiPresentationEntitiesRouteSequenceLineId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRouteSequenceLineName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRouteSequenceDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRouteSequenceIsOutboundOnly :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesRouteSequenceMode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesRouteSequenceLineStrings :: Maybe [Text]
      <*> arbitrary -- tflApiPresentationEntitiesRouteSequenceStations :: Maybe [TflApiPresentationEntitiesMatchedStop]
      <*> arbitrary -- tflApiPresentationEntitiesRouteSequenceStopPointSequences :: Maybe [TflApiPresentationEntitiesStopPointSequence]
      <*> arbitrary -- tflApiPresentationEntitiesRouteSequenceOrderedLineRoutes :: Maybe [TflApiPresentationEntitiesOrderedRoute]
    
instance Arbitrary TflApiPresentationEntitiesSchedule where
  arbitrary =
    TflApiPresentationEntitiesSchedule
      <$> arbitrary -- tflApiPresentationEntitiesScheduleName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesScheduleKnownJourneys :: Maybe [TflApiPresentationEntitiesKnownJourney]
      <*> arbitrary -- tflApiPresentationEntitiesScheduleFirstJourney :: Maybe TflApiPresentationEntitiesKnownJourney
      <*> arbitrary -- tflApiPresentationEntitiesScheduleLastJourney :: Maybe TflApiPresentationEntitiesKnownJourney
      <*> arbitrary -- tflApiPresentationEntitiesSchedulePeriods :: Maybe [TflApiPresentationEntitiesPeriod]
    
instance Arbitrary TflApiPresentationEntitiesSearchMatch where
  arbitrary =
    TflApiPresentationEntitiesSearchMatch
      <$> arbitrary -- tflApiPresentationEntitiesSearchMatchId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesSearchMatchUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesSearchMatchName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesSearchMatchLat :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesSearchMatchLon :: Maybe Double
    
instance Arbitrary TflApiPresentationEntitiesSearchResponse where
  arbitrary =
    TflApiPresentationEntitiesSearchResponse
      <$> arbitrary -- tflApiPresentationEntitiesSearchResponseQuery :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesSearchResponseFrom :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesSearchResponsePage :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesSearchResponsePageSize :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesSearchResponseProvider :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesSearchResponseTotal :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesSearchResponseMatches :: Maybe [TflApiPresentationEntitiesSearchMatch]
      <*> arbitrary -- tflApiPresentationEntitiesSearchResponseMaxScore :: Maybe Double
    
instance Arbitrary TflApiPresentationEntitiesServiceFrequency where
  arbitrary =
    TflApiPresentationEntitiesServiceFrequency
      <$> arbitrary -- tflApiPresentationEntitiesServiceFrequencyLowestFrequency :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesServiceFrequencyHighestFrequency :: Maybe Double
    
instance Arbitrary TflApiPresentationEntitiesStationInterval where
  arbitrary =
    TflApiPresentationEntitiesStationInterval
      <$> arbitrary -- tflApiPresentationEntitiesStationIntervalId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStationIntervalIntervals :: Maybe [TflApiPresentationEntitiesInterval]
    
instance Arbitrary TflApiPresentationEntitiesStatusSeverity where
  arbitrary =
    TflApiPresentationEntitiesStatusSeverity
      <$> arbitrary -- tflApiPresentationEntitiesStatusSeverityModeName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStatusSeveritySeverityLevel :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesStatusSeverityDescription :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesStopPoint where
  arbitrary =
    TflApiPresentationEntitiesStopPoint
      <$> arbitrary -- tflApiPresentationEntitiesStopPointNaptanId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointPlatformName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointIndicator :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointStopLetter :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointModes :: Maybe [Text]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointIcsCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointSmsCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointStopType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointStationNaptan :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointAccessibilitySummary :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointHubNaptanCode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointLines :: Maybe [TflApiPresentationEntitiesIdentifier]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointLineGroup :: Maybe [TflApiPresentationEntitiesLineGroup]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointLineModeGroups :: Maybe [TflApiPresentationEntitiesLineModeGroup]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointFullName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointNaptanMode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointStatus :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesStopPointIndividualStopId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointCommonName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointDistance :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesStopPointPlaceType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointAdditionalProperties :: Maybe [TflApiPresentationEntitiesAdditionalProperties]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointChildren :: Maybe [TflApiPresentationEntitiesPlace]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointChildrenUrls :: Maybe [Text]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointLat :: Maybe Double
      <*> arbitrary -- tflApiPresentationEntitiesStopPointLon :: Maybe Double
    
instance Arbitrary TflApiPresentationEntitiesStopPointCategory where
  arbitrary =
    TflApiPresentationEntitiesStopPointCategory
      <$> arbitrary -- tflApiPresentationEntitiesStopPointCategoryCategory :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointCategoryAvailableKeys :: Maybe [Text]
    
instance Arbitrary TflApiPresentationEntitiesStopPointRouteSection where
  arbitrary =
    TflApiPresentationEntitiesStopPointRouteSection
      <$> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionNaptanId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionLineId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionMode :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionValidFrom :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionValidTo :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionRouteSectionName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionLineString :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionIsActive :: Maybe Bool
      <*> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionServiceType :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionVehicleDestinationText :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointRouteSectionDestinationName :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesStopPointSequence where
  arbitrary =
    TflApiPresentationEntitiesStopPointSequence
      <$> arbitrary -- tflApiPresentationEntitiesStopPointSequenceLineId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointSequenceLineName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointSequenceDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStopPointSequenceBranchId :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesStopPointSequenceNextBranchIds :: Maybe [Int]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointSequencePrevBranchIds :: Maybe [Int]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointSequenceStopPoint :: Maybe [TflApiPresentationEntitiesMatchedStop]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointSequenceServiceType :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesStopPointsResponse where
  arbitrary =
    TflApiPresentationEntitiesStopPointsResponse
      <$> arbitrary -- tflApiPresentationEntitiesStopPointsResponseCentrePoint :: Maybe [Double]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointsResponseStopPoints :: Maybe [TflApiPresentationEntitiesStopPoint]
      <*> arbitrary -- tflApiPresentationEntitiesStopPointsResponsePageSize :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesStopPointsResponseTotal :: Maybe Int
      <*> arbitrary -- tflApiPresentationEntitiesStopPointsResponsePage :: Maybe Int
    
instance Arbitrary TflApiPresentationEntitiesStreet where
  arbitrary =
    TflApiPresentationEntitiesStreet
      <$> arbitrary -- tflApiPresentationEntitiesStreetName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStreetClosure :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStreetDirections :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStreetSegments :: Maybe [TflApiPresentationEntitiesStreetSegment]
      <*> arbitrary -- tflApiPresentationEntitiesStreetSourceSystemId :: Maybe Integer
      <*> arbitrary -- tflApiPresentationEntitiesStreetSourceSystemKey :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesStreetSegment where
  arbitrary =
    TflApiPresentationEntitiesStreetSegment
      <$> arbitrary -- tflApiPresentationEntitiesStreetSegmentToid :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStreetSegmentLineString :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesStreetSegmentSourceSystemId :: Maybe Integer
      <*> arbitrary -- tflApiPresentationEntitiesStreetSegmentSourceSystemKey :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesTimetable where
  arbitrary =
    TflApiPresentationEntitiesTimetable
      <$> arbitrary -- tflApiPresentationEntitiesTimetableDepartureStopId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTimetableRoutes :: Maybe [TflApiPresentationEntitiesTimetableRoute]
    
instance Arbitrary TflApiPresentationEntitiesTimetableResponse where
  arbitrary =
    TflApiPresentationEntitiesTimetableResponse
      <$> arbitrary -- tflApiPresentationEntitiesTimetableResponseLineId :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTimetableResponseLineName :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTimetableResponseDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTimetableResponsePdfUrl :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTimetableResponseStations :: Maybe [TflApiPresentationEntitiesMatchedStop]
      <*> arbitrary -- tflApiPresentationEntitiesTimetableResponseStops :: Maybe [TflApiPresentationEntitiesMatchedStop]
      <*> arbitrary -- tflApiPresentationEntitiesTimetableResponseTimetable :: Maybe TflApiPresentationEntitiesTimetable
      <*> arbitrary -- tflApiPresentationEntitiesTimetableResponseDisambiguation :: Maybe TflApiPresentationEntitiesTimetablesDisambiguation
      <*> arbitrary -- tflApiPresentationEntitiesTimetableResponseStatusErrorMessage :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesTimetableRoute where
  arbitrary =
    TflApiPresentationEntitiesTimetableRoute
      <$> arbitrary -- tflApiPresentationEntitiesTimetableRouteStationIntervals :: Maybe [TflApiPresentationEntitiesStationInterval]
      <*> arbitrary -- tflApiPresentationEntitiesTimetableRouteSchedules :: Maybe [TflApiPresentationEntitiesSchedule]
    
instance Arbitrary TflApiPresentationEntitiesTimetablesDisambiguation where
  arbitrary =
    TflApiPresentationEntitiesTimetablesDisambiguation
      <$> arbitrary -- tflApiPresentationEntitiesTimetablesDisambiguationDisambiguationOptions :: Maybe [TflApiPresentationEntitiesTimetablesDisambiguationOption]
    
instance Arbitrary TflApiPresentationEntitiesTimetablesDisambiguationOption where
  arbitrary =
    TflApiPresentationEntitiesTimetablesDisambiguationOption
      <$> arbitrary -- tflApiPresentationEntitiesTimetablesDisambiguationOptionDescription :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTimetablesDisambiguationOptionUri :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesTrainLoading where
  arbitrary =
    TflApiPresentationEntitiesTrainLoading
      <$> arbitrary -- tflApiPresentationEntitiesTrainLoadingLine :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTrainLoadingLineDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTrainLoadingPlatformDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTrainLoadingDirection :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTrainLoadingNaptanTo :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTrainLoadingTimeSlice :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTrainLoadingValue :: Maybe Int
    
instance Arbitrary TflApiPresentationEntitiesTwentyFourHourClockTime where
  arbitrary =
    TflApiPresentationEntitiesTwentyFourHourClockTime
      <$> arbitrary -- tflApiPresentationEntitiesTwentyFourHourClockTimeHour :: Maybe Text
      <*> arbitrary -- tflApiPresentationEntitiesTwentyFourHourClockTimeMinute :: Maybe Text
    
instance Arbitrary TflApiPresentationEntitiesValidityPeriod where
  arbitrary =
    TflApiPresentationEntitiesValidityPeriod
      <$> arbitrary -- tflApiPresentationEntitiesValidityPeriodFromDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesValidityPeriodToDate :: Maybe DateTime
      <*> arbitrary -- tflApiPresentationEntitiesValidityPeriodIsNow :: Maybe Bool
    



instance Arbitrary E'Category where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'CyclePreference where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'DateTimeType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'DepartureStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Direction where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Direction2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FareCategory where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'JourneyPreference where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Phase where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'RouteType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ServiceType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SkyDirectionDescription where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'TrackType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'WalkingSpeed where
  arbitrary = arbitraryBoundedEnum
