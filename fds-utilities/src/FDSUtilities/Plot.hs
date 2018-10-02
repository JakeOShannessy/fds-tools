{-# LANGUAGE BangPatterns #-}
module FDSUtilities.Plot where

-- import Control.Concurrent
-- import Control.Concurrent.Thread.Delay
-- import Control.Exception
import Control.Lens
-- import Control.Monad

-- import Data.Char
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Default
-- import Data.List
import Data.Maybe
import Data.Time
-- import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V

import Debug.Trace

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Plot

import System.FilePath
import FDSUtilities.Types
import FDSUtilities.Types.Monitor
import FDSUtilities.Histogram

produceChart destinationPath chartConfig dList title filename = produceChartC destinationPath chartConfig (800,400) dList title filename

produceChartC destinationPath chartConfig (width, height) dList title filename = do
    print "about to render SVG"
    -- TODO: rendering to svg has been hanging
    _ <- renderableToFile (FileOptions (width, height) SVG) (joinPath [destinationPath, filename ++ ".svg"]) chR
    print "SVG rendered"
    print "about to render PNG"
    _ <- renderableToFile (FileOptions (width, height) PNG) (joinPath [destinationPath, filename ++ ".png"]) chR
    print "PNG rendered"
    return $ [joinPath [destinationPath, filename ++ ".png"], joinPath [destinationPath, filename ++ ".svg"]]
    where
        chR = chart title chartConfig dList


produceRunChart :: FilePath -> TimeZone -> Double -> [(UTCTime, Double)] -> IO [FilePath]
produceRunChart destinationPath tZone simEndTime runData = do
    putStr "Charting: "
    putStr "Run Chart"
    putStrLn "..."
    (pickfn) <- {-# SCC producingMonitor #-} renderableToFile (FileOptions (800,400) SVG) (joinPath [destinationPath, "RunTime" ++ ".svg"]) chR
    putStrLn "SVG complete"
    _ <- renderableToFile (FileOptions (800,400) PNG) (joinPath [destinationPath, "RunTime" ++ ".png"]) chR
    putStrLn "PNG complete"
    putStrLn " complete."
    -- return $ [joinPath [destinationPath, "RunTime" ++ ".svg"]]
    return $ [joinPath [destinationPath, "RunTime" ++ ".svg"], joinPath [destinationPath, "RunTime" ++ ".png"]]
    where
        chR = runChart "RunTime" tZone simEndTime runData


takeRight (Right val) = val


-- data Case = Case [Chart] [DataVectorPair] deriving Show
-- data Chart = Chart
    -- String
    -- (Maybe StdCurve)
    -- [String]
    -- deriving Show

nfpaHRRChartConfig = chartRefData .~
        [ DataFuncVector "Slow" "kW" (\t->(growthRateToAlpha NFPASlow)*t**2)
        , DataFuncVector "Medium" "kW" (\t->(growthRateToAlpha NFPAMedium)*t**2)
        , DataFuncVector "Fast" "kW" (\t->(growthRateToAlpha NFPAFast)*t**2)
        , DataFuncVector "Ultrafast" "kW" (\t->(growthRateToAlpha NFPAUltrafast)*t**2)
        ]
        $ def

eurocodeHRRChartConfig = chartRefData .~
        [ DataFuncVector "Slow" "kW" (\t->(growthRateToAlpha EurocodeSlow)*t**2)
        , DataFuncVector "Medium" "kW" (\t->(growthRateToAlpha EurocodeMedium)*t**2)
        , DataFuncVector "Fast" "kW" (\t->(growthRateToAlpha EurocodeFast)*t**2)
        , DataFuncVector "Ultrafast" "kW" (\t->(growthRateToAlpha EurocodeUltrafast)*t**2)
        ]
        $ def

unitsToLabels units = case units of
    "kW"    -> "HRR (kW)"
    "kW/m2" -> "Heat Flux (kW/m²)"
    "C"     -> "Temperature (°C)"
    "s"     -> "Time (s)"
    _       -> "(" ++ units ++ ")"


--
-- chart :: (RealFrac a, V.Unbox a, Enum a, RealFrac b, V.Unbox b, Show b) =>String -> ChartConfig -> [DataVectorPair a b] -> Renderable ()
chart :: (Show b, Enum b, V.Unbox b, RealFloat b, RealFloat t,
          Show t,         V.Unbox t, PlotValue b, PlotValue t) =>
          String -> ChartConfig -> [DataVectorPair b t] -> Renderable ()
chart title chartConfig dVectorPairs = toRenderable layout   -- this currently assumes all time vectors match
    where
        maxX = case chartConfig ^. chartConfigMaxX of
                    Just val -> realToFrac val
                    Nothing  -> foldr maxVPX (realToFrac 0) dVectorPairs
        maxY = case chartConfig ^. chartConfigMaxY of
                    Just val -> realToFrac val
                    Nothing  -> foldr maxVPY (realToFrac 0) dVectorPairs
        minX = case chartConfig ^. chartConfigMinX of
                    Just val -> realToFrac val
                    Nothing  -> foldr minVPX (realToFrac 0) dVectorPairs
        minY = case chartConfig ^. chartConfigMinY of
                    Just val -> realToFrac val
                    Nothing  -> foldr minVPY (realToFrac 0) dVectorPairs

        maxVPY (DataVectorPair _ (DataVector _ _ ys)) acc | V.maximum ys > acc = V.maximum ys
                                                          | otherwise = acc
        maxVPX (DataVectorPair (DataVector _ _ xs) _) acc | V.maximum xs > acc = V.maximum xs
                                                          | otherwise = acc
        minVPY (DataVectorPair _ (DataVector _ _ ys)) acc | V.minimum ys < acc = V.minimum ys
                                                          | otherwise = acc
        minVPX (DataVectorPair (DataVector _ _ xs) _) acc | V.minimum xs < acc = V.minimum xs
                                                          | otherwise = acc

        -- make this output RealFracs
        refDataFuncs = chartConfig ^. chartRefData

        xLabel :: String
        xLabel = case chartConfig ^. chartXLabel of
                    Just label -> label
                    Nothing -> unitsToLabels xUnits

        xUnits :: String
        xUnits = if (and $ map (== head allUnits) (tail allUnits))
                 then head allUnits
                 else "Using multiple units on one axes"
            where
                getXUnits = dataVectorUnits . xVector
                allUnits = map getXUnits dVectorPairs

        yLabel :: String
        yLabel = case chartConfig ^. chartYLabel of
                    Just label -> label
                    Nothing -> unitsToLabels yUnits

        yUnits :: String
        yUnits = if (and $ map (== head allUnits) (tail allUnits))
                 then head allUnits
                 else "Using multiple units on one axes"
            where
                getYUnits = dataVectorUnits . yVector
                allUnits = map getYUnits dVectorPairs

        -- layout :: (RealFrac a, V.Unbox a, RealFrac b, V.Unbox b) => Layout a b
        -- layout :: Layout Double Double
        layout = layout_title .~ title
               $ layout_background .~ solidFillStyle (opaque white)
               $ layout_y_axis . laxis_title .~ yLabel
               -- TODO: Investigate why scale axis hangs when cropping out large amounts of data.
               $ layout_y_axis . laxis_generate .~ scaledAxis def (minY, maxY)
               -- $ layout_y_axis . laxis_generate .~ autoScaledAxis def
               $ layout_x_axis . laxis_title .~ xLabel
               $ layout_x_axis . laxis_generate .~ scaledAxis def (minX, maxX)
               -- -- $ layout_x_axis . laxis_generate .~ autoScaledAxis def

               -- TODO: this currently removes the legend if it gets too large. There should
               -- be a better method of dealing with these cases.
               -- At this point we have long since run out of colours to disambiguate, and
               -- so the legend no longer serves a purpose.
               $ layout_legend .~ (if length dVectorPairs > 10 then Nothing else Just def)

               $ layout_plots .~ map (toPlot) (plotLines ++ refDataLines)
               $ layout_foreground .~ (opaque black)
               $ def

        plotLines = map mkPlotLine $ zip lineStyles dVectorPairs
        mkPlotLine (lineStyle, (DataVectorPair (DataVector _ _ xVals) (DataVector name units yVals))) =
                plot_lines_style .~ lineStyle  -- have to cycle the linestyles
                $ plot_lines_values .~ [[ (x, y) | (x,y) <- zip (V.toList xVals) (V.toList yVals)]]
                $ plot_lines_title .~ name
                $ def

        lineStyles = cycle
            [ lineStyle1
            , lineStyle2
            , lineStyle3
            , lineStyle4
            , lineStyle5
            , lineStyle6
            , lineStyle7
            , lineStyle8
            , lineStyle9
            , lineStyle10
            ]

        lineStyle1 = line_width .~ 1
                   $ line_color .~ opaque blue
                   $ def
        lineStyle2 = line_width .~ 1
                   $ line_color .~ opaque red
                   $ def
        lineStyle3 = line_width .~ 1
                   $ line_color .~ opaque green
                   $ def
        lineStyle4 = line_width .~ 1
                   $ line_color .~ opaque purple
                   $ def
        lineStyle5 = line_width .~ 1
                   $ line_color .~ opaque brown
                   $ def
        lineStyle6 = line_width .~ 1
                   $ line_color .~ opaque dimgrey
                   $ def
        lineStyle7 = line_width .~ 1
                   $ line_color .~ opaque maroon
                   $ def
        lineStyle8 = line_width .~ 1
                   $ line_color .~ opaque olive
                   $ def
        lineStyle9 = line_width .~ 1
                   $ line_color .~ opaque orangered
                   $ def
        lineStyle10 = line_width .~ 1
                   $ line_color .~ opaque powderblue
                   $ def


        refDataLines = map mkPlotLine $ zip stdLineStyles (mkRefVectors refDataFuncs tDel (maxX))
        tDel = 1
        mkRefVectors :: (RealFrac a, V.Unbox a, Enum a, RealFrac b, V.Unbox b) => [FlexibleDataVector] -> Double -> a -> [DataVectorPair a b]
        mkRefVectors refDataFuncs tDel tEnd = map mkVec refDataFuncs
            where
                -- tDat :: (RealFrac a, V.Unbox a) => V.Vector a
                tDat = V.fromList [0,1..tEnd]
                tVector = DataVector "Time" "s" ( tDat)
                -- mkVec :: _
                mkVec (DataFuncVector name units refFunc) =
                    DataVectorPair
                      tVector
                      $ DataVector name units (V.map realToFrac $ V.map refFunc $ V.map realToFrac tDat)


        stdLineStyles =
            [ slowLineStyle
            , fastLineStyle
            , mediumLineStyle
            , ultrafastLineStyle
            ]
        slowLineStyle = (dashedLine 1 [8,5] (opaque (sRGB24read "#008000")))
        fastLineStyle = (dashedLine 1 [8,5] (opaque (sRGB24read "#FF0000")))
        mediumLineStyle = (dashedLine 1 [8,5] (opaque (sRGB24read "#00BFBF")))
        ultrafastLineStyle = (dashedLine 1 [8,5] (opaque (sRGB24read "#BF00BF")))

runChart :: String -> TimeZone -> Double -> [(UTCTime, Double)] -> Renderable ()
runChart title tZone simEndTime [] = error "runChart: no data points"
runChart title tZone simEndTime dPoints
    | length dPoints < 2 = error $ "runChart: insufficient data points: " ++ show dPoints
    | otherwise = toRenderable layout   -- this currently assumes all time vectors match
    where
        layout = layout_title .~ title
               $ layout_background .~ solidFillStyle (opaque white)
               $ layout_y_axis . laxis_title .~ "Simulation Time (s)"
               $ layout_x_axis . laxis_title .~ "Real Date"
               $ layout_plots .~ map (toPlot) [historyLine, predictionLine, simEndLine, clockEndLine]
               $ layout_foreground .~ (opaque black)
               $ def

        historyLine = mkPlotLine "History" (historyLineStyle, dPoints)
        mkPlotLine name (lineStyle, dPoints) =
                plot_lines_style .~ lineStyle  -- have to cycle the linestyles
                $ plot_lines_values .~ [[ ((utcToLocalTime tZone x), y) | (x,y) <- dPoints]]
                $ plot_lines_title .~ name
                $ def


        historyLineStyle = line_width .~ 1
                   $ line_color .~ opaque blue
                   $ def

        ((penUltStepClock, penUltStepSim):(ultStepClock, ultStepSim):[]) = drop (length dPoints - 2) dPoints
        -- negative numbers here cause explosion of memory when linked into other modules on windows.
        timeLeft :: Int
        timeLeft = max 0 $floor $ (dCalcTime / dSimTime) * (simEndTime - (ultStepSim))
            where
                dCalcTime = realToFrac (diffUTCTime (ultStepClock) (penUltStepClock)) :: Double
                dSimTime = (ultStepSim) - (penUltStepSim)
        predEndTime = addUTCTime (fromIntegral timeLeft) ultStepClock

        predictionLine = mkPlotLine "Prediction" (predictionLineStyle, [(ultStepClock, ultStepSim),(predEndTime,simEndTime)])
        predictionLineStyle = (dashedLine 1 [8,5] (opaque (sRGB24read "#FF0000")))
        (simStart, _) = case dPoints of
            (x:xs) -> x
            [] -> error "no dPoints"
        simEndLine = mkPlotLine "" (simEndLineStyle, [(simStart, simEndTime),(predEndTime,simEndTime)])
        clockEndLine = mkPlotLine "" (clockEndLineStyle, [(predEndTime, 0),(predEndTime,simEndTime)])
        simEndLineStyle = (dashedLine 1 [8,5] (opaque (sRGB24read "#00BFBF")))
        clockEndLineStyle = (dashedLine 1 [8,5] (opaque (sRGB24read "#BF00BF")))



histChart :: Bool -> String -> Histogram -> Renderable ()
histChart borders name (Histogram (lower, upper) _ values) = toRenderable layout
  where
    minX = lower
    maxX = upper
    layout = layout_title .~ name
           $ layout_background .~ solidFillStyle (opaque white)
           $ layout_y_axis . laxis_title .~ "Number of Occurrences"
           $ layout_x_axis . laxis_title .~ "Evacuation Time (s)"
           $ layout_x_axis . laxis_generate .~ scaledAxis def (minX, maxX) -- (\x->scaledAxis x (minX, maxX))
           $ layout_plots .~ [ (plotBars bars)]
           $ layout_foreground .~ (opaque black)
           $ def


    bars = plot_bars_values .~ [ (lo,[n]) | (Bin lo hi n) <- values]
      $ plot_bars_spacing .~ BarsFixGap 0 0
      $ plot_bars_alignment .~ BarsLeft
      $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
      $ def

    btitle = if borders then "" else " (no borders)"
    bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
    mkstyle c = (solidFillStyle c, bstyle)


    lineStyle n colour = line_width .~ n * 2
                       $ line_color .~ opaque colour
                       $ def

histChartNormalised :: Bool -> String -> HistogramNormalised -> Maybe (Double -> Double) -> Renderable ()
histChartNormalised borders name (HistogramNormalised (lower, upper) _ values) pdfFunc' = toRenderable layout
  where
    minX = lower
    maxX = upper
    layout = layout_title .~ name
           $ layout_background .~ solidFillStyle (opaque white)
           $ layout_y_axis . laxis_title .~ "Fraction of Occurrences"
           $ layout_x_axis . laxis_title .~ "Evacuation Time (s)"
           $ layout_x_axis . laxis_generate .~ scaledAxis def (minX, maxX) -- (\x->scaledAxis x (minX, maxX))
           $ layout_plots .~ plots
           $ layout_foreground .~ (opaque black)
           $ def

    plots = [plotBars bars] ++ pdfLine

    bars = plot_bars_values .~ [ (lo,[n]) | (BinNormalised lo hi n) <- values]
      $ plot_bars_spacing .~ BarsFixGap 0 0
      $ plot_bars_alignment .~ BarsLeft
      $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
      $ def

    pdfLine = case pdfFunc' of
        Nothing -> []
        Just pdfFunc ->
            let pdfVals = map (\x->(x,pdfFunc x)) [lower,lower+del..upper]
            in [toPlot $
                plot_lines_style .~ lineStyle1
                $ plot_lines_values .~  [pdfVals] -- map (filter (\(x,y) -> (x >= lower) && (x <= upper))) [pdfVals] -- this filtering is now unnecessary
                $ def]


    del = 1

    lineStyle1 = line_width .~ 1
                   $ line_color .~ opaque red
                   $ def

    btitle = if borders then "" else " (no borders)"
    bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
    mkstyle c = (solidFillStyle c, bstyle)


    lineStyle n colour = line_width .~ n * 2
                       $ line_color .~ opaque colour
                       $ def
