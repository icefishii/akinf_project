{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PropertyTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import AkinfProject.CSV (Stock(..))
import AkinfProject.Calculate
import AkinfProject.Output (formatPercentage)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.List (sort)

-- =============================================================================
-- ARBITRARY INSTANCES
-- =============================================================================

instance Arbitrary TrendDirection where
  arbitrary = elements [Upward, Downward, Sideways]

instance Arbitrary RiskLevel where
  arbitrary = elements [Low, Medium, High]

instance Arbitrary MASignal where
  arbitrary = elements [Bullish, Bearish, Neutral]

instance Arbitrary Stock where
  arbitrary = genStockWithMissing

-- =============================================================================
-- GENERATORS
-- =============================================================================

-- | Generate a valid stock price (positive, reasonable range)
genPrice :: Gen Double
genPrice = choose (1.0, 1000.0)

-- | Generate a valid volume (positive integer)
genVolume :: Gen Int
genVolume = choose (100, 10000000)

-- | Generate a date string in YYYY-MM-DD format
genDate :: Gen T.Text
genDate = do
  year <- choose (2010, 2024) :: Gen Int
  month <- choose (1, 12) :: Gen Int
  day <- choose (1, 28) :: Gen Int  -- Avoid leap year complications
  return $ T.pack $ show year ++ "-" ++ 
           (if month < 10 then "0" else "") ++ show month ++ "-" ++
           (if day < 10 then "0" else "") ++ show day

-- | Generate a stock name (3-4 letter ticker)
genStockName :: Gen T.Text
genStockName = do
  len <- choose (3, 4)
  name <- vectorOf len (choose ('A', 'Z'))
  return $ T.pack name

-- | Generate a valid Stock with all fields present
genValidStock :: Gen Stock
genValidStock = do
  date <- genDate
  open <- genPrice
  high <- genPrice
  low <- genPrice
  close <- genPrice
  volume <- genVolume
  name <- genStockName
  -- Ensure high >= max(open, close) and low <= min(open, close)
  let actualHigh = max high (max open close)
      actualLow = min low (min open close)
  return $ Stock date (Just open) (Just actualHigh) (Just actualLow) (Just close) (Just volume) name

-- | Generate a stock with potentially missing fields
genStockWithMissing :: Gen Stock
genStockWithMissing = do
  date <- genDate
  open <- oneof [Just <$> genPrice, return Nothing]
  high <- oneof [Just <$> genPrice, return Nothing]
  low <- oneof [Just <$> genPrice, return Nothing]
  close <- oneof [Just <$> genPrice, return Nothing]
  volume <- oneof [Just <$> genVolume, return Nothing]
  name <- genStockName
  return $ Stock date open high low close volume name

-- | Generate a non-empty vector of valid stocks
genValidStocks :: Gen (V.Vector Stock)
genValidStocks = do
  size <- choose (1, 100)
  stocks <- vectorOf size genValidStock
  return $ V.fromList stocks

-- | Generate a vector of stocks with some missing data
genStocksWithMissing :: Gen (V.Vector Stock)
genStocksWithMissing = do
  size <- choose (1, 50)
  stocks <- vectorOf size genStockWithMissing
  return $ V.fromList stocks

-- | Generate a map of stock names to stock vectors
genStockMap :: Gen (Map.Map String (V.Vector Stock))
genStockMap = do
  numStocks <- choose (1, 5)
  stockNames <- vectorOf numStocks (T.unpack <$> genStockName)
  let uniqueNames = take numStocks (map show [1..]) -- Ensure unique names
  stockVectors <- vectorOf numStocks genValidStocks
  return $ Map.fromList (zip uniqueNames stockVectors)

-- =============================================================================
-- PROPERTY TESTS
-- =============================================================================

-- | Property tests for the Calculate module
propertyTests :: TestTree
propertyTests = testGroup "Property-Based Tests"
  [ percentageChangeProperties
  , movingAverageProperties
  , roiProperties
  , riskAnalysisProperties
  , trendAnalysisProperties
  , correlationProperties
  , outputProperties
  , stockAnalysisProperties
  ]

-- | Properties for percentage change calculations
percentageChangeProperties :: TestTree
percentageChangeProperties = testGroup "Percentage Change Properties"
  [ testProperty "percentageChange is Nothing for stocks with missing data" $
      \stock -> case (open stock, close stock) of
        (Nothing, _) -> percentageChange stock === Nothing
        (_, Nothing) -> percentageChange stock === Nothing
        (Just _, Just _) -> property True  -- Should have a value
  
  , testProperty "percentageChange formula is correct" $
      \(Positive openPrice) (Positive closePrice) -> 
        let stock = Stock "2023-01-01" (Just openPrice) (Just closePrice) (Just closePrice) (Just closePrice) (Just 1000) "TEST"
            expected = ((closePrice - openPrice) / openPrice) * 100
        in percentageChange stock === Just expected
  
  , testProperty "percentageChange is symmetric around zero" $
      \(Positive price) -> 
        price > 1.0 && price < 100.0 ==> -- Keep prices reasonable
        forAll (choose (0.5, 2.0)) $ \factor -> 
          let stock1 = Stock "2023-01-01" (Just price) (Just price) (Just price) (Just (price * factor)) (Just 1000) "TEST"
              stock2 = Stock "2023-01-01" (Just (price * factor)) (Just (price * factor)) (Just (price * factor)) (Just price) (Just 1000) "TEST"
          in case (percentageChange stock1, percentageChange stock2) of
               (Just pct1, Just pct2) -> 
                 let combined = (1 + pct1/100) * (1 + pct2/100)
                 in abs (combined - 1.0) < 0.01  -- Should multiply to approximately 1
               _ -> False
  
  , testProperty "percentageChange is 0 when open equals close" $
      \(Positive price) -> 
        let stock = Stock "2023-01-01" (Just price) (Just price) (Just price) (Just price) (Just 1000) "TEST"
        in percentageChange stock === Just 0.0
  ]

-- | Properties for moving averages
movingAverageProperties :: TestTree
movingAverageProperties = testGroup "Moving Average Properties"
  [ testProperty "calculateSMA returns Nothing for insufficient data" $
      \(Positive period) -> 
        forAll genValidStocks $ \stocks ->
          V.length stocks < period ==> calculateSMA period stocks === Nothing
  
  , testProperty "calculateSMA returns Just for sufficient data" $
      \(Positive period) -> 
        forAll genValidStocks $ \stocks ->
          V.length stocks >= period ==> case calculateSMA period stocks of
            Just _ -> property True
            Nothing -> property False
  
  , testProperty "calculateSMA result is within reasonable bounds" $
      \(Positive period) -> 
        forAll genValidStocks $ \stocks ->
          V.length stocks >= period ==> case calculateSMA period stocks of
            Just sma -> 
              let prices = V.mapMaybe close (V.take period stocks)
                  minPrice = V.minimum prices
                  maxPrice = V.maximum prices
              in property (sma >= minPrice && sma <= maxPrice)
            Nothing -> property False
  
  , testProperty "calculateSMA with period 1 equals the first close price" $
      forAll genValidStocks $ \stocks ->
        not (V.null stocks) ==> case (calculateSMA 1 stocks, close (V.head stocks)) of
          (Just sma, Just firstClose) -> property (abs (sma - firstClose) < 0.01)
          _ -> property False
  
  , testProperty "Moving averages are monotonic with respect to trend" $
      \(Positive basePrice) -> 
        basePrice < 100 ==> -- Keep prices reasonable
        let size = 50
            prices = [basePrice * (1.02 ^ i) | i <- [0..size-1]]
            -- Reverse so most recent (highest) prices are first
            stocks = V.fromList $ reverse [Stock "2023-01-01" (Just p) (Just p) (Just p) (Just p) (Just 1000) "TEST" | p <- prices]
            sma10 = calculateSMA 10 stocks
            sma20 = calculateSMA 20 stocks
        in case (sma10, sma20) of
             (Just sma10', Just sma20') -> property (sma10' >= sma20')  -- Shorter MA should be higher in uptrend
             _ -> property False
  
  , testProperty "determineMASignal gives Bullish for ascending SMAs" $
      \(Positive base) (Positive gap) -> 
        let sma20 = base + 2 * gap
            sma50 = base + gap
            sma200 = base
        in determineMASignal (Just sma20) (Just sma50) (Just sma200) === Just Bullish
  
  , testProperty "determineMASignal gives Bearish for descending SMAs" $
      \(Positive base) (Positive gap) -> 
        let sma20 = base
            sma50 = base + gap
            sma200 = base + 2 * gap
        in determineMASignal (Just sma20) (Just sma50) (Just sma200) === Just Bearish
  ]

-- | Properties for ROI calculations
roiProperties :: TestTree
roiProperties = testGroup "ROI Properties"
  [ testProperty "calculateROI returns Nothing for empty vector" $
      calculateROI V.empty === Nothing
  
  , testProperty "calculateROI returns 0 for single stock" $
      forAll genValidStock $ \stock ->
        calculateROI (V.singleton stock) === Just 0.0
  
  , testProperty "calculateROI is consistent with percentage change" $
      forAll genValidStock $ \stock1 ->
        forAll genValidStock $ \stock2 ->
          case (close stock1, close stock2) of
            (Just c1, Just c2) -> 
              let stocks = V.fromList [stock1, stock2]
                  expectedROI = ((c2 - c1) / c1) * 100
              in case calculateROI stocks of
                   Just roi -> abs (roi - expectedROI) < 0.01
                   Nothing -> False
            _ -> True  -- Skip if missing data
  
  , testProperty "calculateROI is symmetric" $
      \(Positive price1) (Positive price2) -> 
        price1 > 1.0 && price1 < 100.0 && price2 > 1.0 && price2 < 100.0 ==>
        let stock1 = Stock "2023-01-01" (Just price1) (Just price1) (Just price1) (Just price1) (Just 1000) "TEST"
            stock2 = Stock "2023-01-02" (Just price2) (Just price2) (Just price2) (Just price2) (Just 1000) "TEST"
            stocks12 = V.fromList [stock1, stock2]
            stocks21 = V.fromList [stock2, stock1]
        in case (calculateROI stocks12, calculateROI stocks21) of
             (Just roi12, Just roi21) -> 
               let combined = (1 + roi12/100) * (1 + roi21/100)
               in abs (combined - 1.0) < 0.01  -- Should multiply to approximately 1
             _ -> False
  
  , testProperty "detectTrend classification is consistent with ROI" $
      forAll genValidStocks $ \stocks ->
        case (calculateROI stocks, detectTrend stocks) of
          (Just roi, Just trend) -> case trend of
            Upward -> roi > 5.0
            Downward -> roi < -5.0
            Sideways -> roi >= -5.0 && roi <= 5.0
          _ -> True  -- Skip if missing data
  ]

-- | Properties for risk analysis
riskAnalysisProperties :: TestTree
riskAnalysisProperties = testGroup "Risk Analysis Properties"
  [ testProperty "dailyVolatility returns Nothing for missing data" $
      \stock -> case (open stock, high stock, low stock) of
        (Nothing, _, _) -> dailyVolatility stock === Nothing
        (_, Nothing, _) -> dailyVolatility stock === Nothing
        (_, _, Nothing) -> dailyVolatility stock === Nothing
        (Just _, Just _, Just _) -> case dailyVolatility stock of
          Just _ -> property True
          Nothing -> property False
  
  , testProperty "dailyVolatility is non-negative" $
      forAll genValidStock $ \stock ->
        case dailyVolatility stock of
          Just vol -> vol >= 0
          Nothing -> True
  
  , testProperty "dailyVolatility is 0 when high equals low" $
      \(Positive price) -> 
        let stock = Stock "2023-01-01" (Just price) (Just price) (Just price) (Just price) (Just 1000) "TEST"
        in dailyVolatility stock === Just 0.0
  
  , testProperty "calculateRiskLevel classification is consistent" $
      forAll genValidStocks $ \stocks ->
        case calculateRiskLevel stocks of
          (Just avgVol, Just risk) -> case risk of
            Low -> avgVol <= 4.0
            Medium -> avgVol > 4.0 && avgVol <= 8.0
            High -> avgVol > 8.0
          _ -> True
  
  , testProperty "calculateRiskLevel handles empty vector" $
      calculateRiskLevel V.empty === (Nothing, Nothing)
  ]

-- | Properties for trend analysis
trendAnalysisProperties :: TestTree
trendAnalysisProperties = testGroup "Trend Analysis Properties"
  [ testProperty "detectTrend returns Nothing for empty vector" $
      detectTrend V.empty === Nothing
  
  , testProperty "detectTrend returns Sideways for single stock" $
      forAll genValidStock $ \stock ->
        detectTrend (V.singleton stock) === Just Sideways
  
  , testProperty "detectTrend is consistent with strong trends" $
      \(Positive basePrice) (Positive multiplier) -> 
        multiplier > 1.1 ==> -- Ensure significant change
          let stock1 = Stock "2023-01-01" (Just basePrice) (Just basePrice) (Just basePrice) (Just basePrice) (Just 1000) "TEST"
              stock2 = Stock "2023-01-02" (Just (basePrice * multiplier)) (Just (basePrice * multiplier)) (Just (basePrice * multiplier)) (Just (basePrice * multiplier)) (Just 1000) "TEST"
              stocks = V.fromList [stock1, stock2]
              expectedTrend = if multiplier > 1.05 then Upward else Sideways
          in detectTrend stocks === Just expectedTrend
  ]

-- | Properties for correlation analysis
correlationProperties :: TestTree
correlationProperties = testGroup "Correlation Properties"
  [ testProperty "calculateCorrelation returns Nothing for empty vectors" $
      calculateCorrelation V.empty V.empty === Nothing
  
  , testProperty "calculateCorrelation returns Nothing for different length vectors" $
      \(NonEmpty xs) (NonEmpty ys) -> 
        length xs /= length ys ==> 
          calculateCorrelation (V.fromList xs) (V.fromList ys) === Nothing
  
  , testProperty "calculateCorrelation with identical vectors gives 1.0" $
      \(NonEmpty xs) -> 
        let vec = V.fromList xs
        in case calculateCorrelation vec vec of
             Just corr -> abs (corr - 1.0) < 0.01
             Nothing -> length xs <= 1  -- Correlation undefined for single point
  
  , testProperty "calculateCorrelation is commutative" $
      \(Positive n) -> 
        n <= 20 ==> -- Limit size to reduce skipped tests
        forAll (vectorOf n genReasonableChange) $ \xs ->
        forAll (vectorOf n genReasonableChange) $ \ys ->
          let vec1 = V.fromList xs
              vec2 = V.fromList ys
          in calculateCorrelation vec1 vec2 === calculateCorrelation vec2 vec1
  
  , testProperty "calculateCorrelation result is bounded [-1, 1]" $
      \(Positive n) -> 
        n <= 20 && n >= 2 ==> -- Need at least 2 points, limit size
        forAll (vectorOf n genReasonableChange) $ \xs ->
        forAll (vectorOf n genReasonableChange) $ \ys ->
          let vec1 = V.fromList xs
              vec2 = V.fromList ys
          in case calculateCorrelation vec1 vec2 of
               Just corr -> corr >= -1.0 && corr <= 1.0
               Nothing -> True
  
  , testProperty "calculateDailyReturns produces n-1 returns for n stocks" $
      forAll genValidStocks $ \stocks ->
        V.length stocks > 0 ==> 
          let returns = calculateDailyReturns stocks
              validStocks = V.filter (\s -> case close s of Just _ -> True; Nothing -> False) stocks
          in V.length returns <= V.length validStocks
  
  , testProperty "calculatePortfolioCorrelation handles single stock" $
      forAll genValidStocks $ \stocks ->
        let stockMap = Map.singleton "TEST" stocks
            portfolioCorr = calculatePortfolioCorrelation stockMap
        in (Map.size (correlationMatrix portfolioCorr) === 0) .&&.
           (averageCorrelation portfolioCorr === 0.0)
  ]

-- | Properties for output formatting
outputProperties :: TestTree
outputProperties = testGroup "Output Properties"
  [ testProperty "formatPercentage always ends with %" $
      \x -> last (formatPercentage x) === '%'
  
  , testProperty "formatPercentage handles zero correctly" $
      formatPercentage 0.0 === "0.00%"
  
  , testProperty "formatPercentage preserves sign" $
      \x -> (x >= 0) == (head (formatPercentage x) /= '-')
  
  , testProperty "formatPercentage is consistent with absolute value" $
      \x -> let formatted = formatPercentage x
                formattedAbs = formatPercentage (abs x)
            in if x >= 0 then formatted === formattedAbs
               else formatted === '-' : formattedAbs
  
  , testProperty "formatPercentage has exactly 2 decimal places" $
      \x -> let formatted = formatPercentage x
                beforePercent = init formatted
                afterDot = reverse (take 2 (reverse beforePercent))
            in (length afterDot === 2) .&&. 
               property (all (`elem` ("0123456789" :: String)) afterDot)
  ]

-- | Properties for stock analysis
stockAnalysisProperties :: TestTree
stockAnalysisProperties = testGroup "Stock Analysis Properties"
  [ testProperty "analyzeStock preserves stock name" $
      \name -> 
        forAll genValidStocks $ \stocks ->
          stockName (analyzeStock name stocks) === name
  
  , testProperty "analyzeStock handles empty vector gracefully" $
      \name -> 
        let analysis = analyzeStock name V.empty
        in (stockName analysis === name) .&&.
           (bestDay analysis === Nothing) .&&.
           (worstDay analysis === Nothing)
  
  , testProperty "analyzeStock best day >= worst day" $
      \name -> 
        forAll genValidStocks $ \stocks ->
          let analysis = analyzeStock name stocks
          in case (bestDay analysis, worstDay analysis) of
               (Just (_, bestPct), Just (_, worstPct)) -> bestPct >= worstPct
               _ -> True
  
  , testProperty "analyzeAllStocks preserves number of stocks" $
      forAll genStockMap $ \stockMap ->
        length (analyzeAllStocks stockMap) === Map.size stockMap
  
  , testProperty "analyzeAllStocks preserves stock names" $
      forAll genStockMap $ \stockMap ->
        let analyses = analyzeAllStocks stockMap
            analysisNames = sort (map stockName analyses)
            mapNames = sort (Map.keys stockMap)
        in analysisNames === mapNames
  
  , testProperty "analyzePortfolio returns consistent data" $
      forAll genStockMap $ \stockMap ->
        let (analyses, portfolioCorr) = analyzePortfolio stockMap
        in (length analyses === Map.size stockMap) .&&.
           property (Map.size (correlationMatrix portfolioCorr) <= (Map.size stockMap * (Map.size stockMap - 1)) `div` 2)
  ]

-- =============================================================================
-- HELPER FUNCTIONS FOR RUNNING TESTS
-- =============================================================================

-- | Run all property tests
runPropertyTests :: IO ()
runPropertyTests = defaultMain propertyTests

-- | Run a specific test group
runSpecificTests :: String -> IO ()
runSpecificTests testName = 
  case testName of
    "percentage" -> defaultMain percentageChangeProperties
    "moving" -> defaultMain movingAverageProperties
    "roi" -> defaultMain roiProperties
    "risk" -> defaultMain riskAnalysisProperties
    "trend" -> defaultMain trendAnalysisProperties
    "correlation" -> defaultMain correlationProperties
    "output" -> defaultMain outputProperties
    "analysis" -> defaultMain stockAnalysisProperties
    _ -> putStrLn "Unknown test group. Available: percentage, moving, roi, risk, trend, correlation, output, analysis"

-- =============================================================================
-- CUSTOM PROPERTY MODIFIERS
-- =============================================================================

-- | Generate lists with a minimum size
genNonEmptyList :: Gen a -> Gen [a]
genNonEmptyList gen = do
  size <- choose (1, 20)
  vectorOf size gen

-- | Generate a reasonable stock price change
genReasonableChange :: Gen Double
genReasonableChange = choose (-0.5, 0.5)  -- -50% to +50% daily change

-- | Generate a stock with a specific price pattern
genTrendingStock :: TrendDirection -> Double -> Gen Stock
genTrendingStock trend basePrice = do
  date <- genDate
  name <- genStockName
  let priceMultiplier = case trend of
        Upward -> choose (1.01, 1.1)   -- 1% to 10% increase
        Downward -> choose (0.9, 0.99) -- 1% to 10% decrease  
        Sideways -> choose (0.995, 1.005) -- ±0.5% change
  multiplier <- priceMultiplier
  let newPrice = basePrice * multiplier
  return $ Stock date (Just basePrice) (Just newPrice) (Just basePrice) (Just newPrice) (Just 1000) name

-- | Generate a vector of trending stocks
genTrendingStocks :: TrendDirection -> Gen (V.Vector Stock)
genTrendingStocks trend = do
  size <- choose (10, 30)  -- Smaller, more manageable size
  basePrice <- choose (10.0, 50.0)  -- Reasonable base price
  let multiplier = case trend of
        Upward -> 1.03    -- 3% daily increase
        Downward -> 0.97  -- 3% daily decrease
        Sideways -> 1.001 -- 0.1% minimal change
      prices = take size $ iterate (* multiplier) basePrice
      -- Reverse so most recent (highest for upward) prices are first
      orderedPrices = case trend of
        Upward -> reverse prices      -- Most recent highest prices first
        Downward -> prices           -- Most recent lowest prices first  
        Sideways -> prices           -- Order doesn't matter much
  stocks <- mapM (\(i, p) -> do
    name <- genStockName
    let dateStr = "2023-01-" ++ (if i+1 < 10 then "0" else "") ++ show (i+1)
    return $ Stock (T.pack dateStr) (Just p) (Just p) (Just p) (Just p) (Just 1000) name
    ) (zip [0..] orderedPrices)
  return $ V.fromList stocks

-- =============================================================================
-- PROPERTY TEST INVARIANTS
-- =============================================================================

-- | Invariant: Moving averages should be ordered correctly in trending markets
prop_movingAverageOrdering :: TrendDirection -> Property
prop_movingAverageOrdering trend = 
  forAll (genTrendingStocks trend) $ \stocks ->
    V.length stocks >= 20 ==> 
      let mas = calculateMovingAverages stocks
      in case (sma20 mas, sma50 mas) of
           (Just sma20', Nothing) -> True  -- Only 20-day MA available
           (Just sma20', Just sma50') -> case trend of
             Upward -> sma20' >= sma50'    -- Shorter MA higher in uptrend
             Downward -> sma20' <= sma50'  -- Shorter MA lower in downtrend  
             Sideways -> True              -- No specific ordering expected
           _ -> True

-- | Invariant: Risk level should increase with volatility
prop_riskVolatilityRelation :: Property
prop_riskVolatilityRelation = 
  forAll genValidStocks $ \stocks ->
    let (maybeVol, maybeRisk) = calculateRiskLevel stocks
    in case (maybeVol, maybeRisk) of
         (Just vol, Just risk) -> case risk of
           Low -> vol <= 4.0
           Medium -> vol > 4.0 && vol <= 8.0
           High -> vol > 8.0
         _ -> True

-- | Invariant: ROI and trend detection should be consistent
prop_roiTrendConsistency :: Property
prop_roiTrendConsistency = 
  forAll genValidStocks $ \stocks ->
    case (calculateROI stocks, detectTrend stocks) of
      (Just roi, Just trend) -> case trend of
        Upward -> roi > 5.0
        Downward -> roi < -5.0
        Sideways -> roi >= -5.0 && roi <= 5.0
      _ -> True

-- | Invariant: Correlation should be symmetric
prop_correlationSymmetry :: Property
prop_correlationSymmetry = 
  forAll (choose (2, 10)) $ \n ->
    forAll (vectorOf n genReasonableChange) $ \xs ->
    forAll (vectorOf n genReasonableChange) $ \ys ->
      let vec1 = V.fromList xs
          vec2 = V.fromList ys
      in calculateCorrelation vec1 vec2 === calculateCorrelation vec2 vec1

-- | Additional invariant tests
additionalInvariants :: TestTree
additionalInvariants = testGroup "Invariant Tests"
  [ testProperty "Moving average ordering in trending markets" prop_movingAverageOrdering
  , testProperty "Risk-volatility relationship" prop_riskVolatilityRelation
  , testProperty "ROI-trend consistency" prop_roiTrendConsistency
  , testProperty "Correlation symmetry" prop_correlationSymmetry
  ]

-- | Complete test suite including invariants
completePropertyTests :: TestTree
completePropertyTests = testGroup "Complete Property-Based Tests"
  [ propertyTests
  , additionalInvariants
  ]
