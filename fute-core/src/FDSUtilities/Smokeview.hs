{-# LANGUAGE RecordWildCards #-}
module FDSUtilities.Smokeview
    (
    -- makeIni
    -- , makeSSF
    -- , makeSSFInd
    -- -- , mkRenderName  -- TODO: does not belong here
    -- ,
    module FDSUtilities.Smokeview
    )
    where

import FDSUtilities.Smokeview.IniConfig
import FDSUtilities.Smokeview.SSF
import FDSUtilities.Types

import FDSUtilities.Types.Smokeview

import Data.Default
import Control.Lens


-- |This is a custom abstraction to describe a view in smoke view.
-- It includes all viewing angles etc., as well as cutting planes
-- and window dimensions. It does not include data loading scales
-- etc.
data SmokeviewViewConfig = SmokeviewViewConfig
    { smokeviewConfig_windowSize :: (Int,Int)
    , smokeviewConfig_clippingX :: (Maybe Double, Maybe Double)
    , smokeviewConfig_clippingY :: (Maybe Double, Maybe Double)
    , smokeviewConfig_clippingZ :: (Maybe Double, Maybe Double)
    , smokeviewConfig_clippingType :: ClippingType
    , smokeviewConfig_viewDirection :: SomeViewDirection
    -- , smokeviewConfig_eyePos :: (Double, Double, Double)
    }

data SomeViewDirection = SimpleViewXMin | SimpleViewYMax | SimpleViewZMax | CustomView ViewDirection EyePosition

type EyePosition = (Double, Double, Double)

toViewDirection :: SomeViewDirection -> (ViewDirection, EyePosition)
toViewDirection sv = case sv of
    -- TODO: work out a way to determine the correct eye position.
    SimpleViewXMin   -> (ViewDirection 90 0, (-yCen, camYVal, zCen))
    SimpleViewYMax   -> (ViewDirection 0  0, ( xCen, camYVal, zCen))
    SimpleViewZMax   -> (ViewDirection 0 90, ( yCen, camYVal, xCen))
    CustomView vd ep -> (vd, ep)
    where
        camYVal = -2.75 -- TODO: investigate this value
        (xmin, xmax) = (-107, 51)
        (ymin, ymax) = (-67, 48.5)
        (zmin, zmax) = (0, 21)
        xDelR = xmax - xmin
        yDelR = ymax - ymin
        zDelR = zmax - zmin
        -- adjust del values to fit in the 0.0 to 1.0 range
        -- fitting inside a cube. The sides of the cube are set to
        -- the longest length
        ll = maximum [xDelR, yDelR, zDelR]
        xDelV = xDelR/ll
        yDelV = yDelR/ll
        zDelV = zDelR/ll

        xCen = xDelV/2
        yCen = yDelV/2
        zCen = zDelV/2

data ViewDirection = ViewDirection
    { viewDirection_azimuth :: Double
    , viewDirection_elevation :: Double
    }

viewDirectionToTuple :: ViewDirection -> (Double, Double)
viewDirectionToTuple ViewDirection{..} = (viewDirection_azimuth, viewDirection_elevation)
-- |Specifies what data is to be loaded and how.
data SmokeviewDataLoad = SmokeviewDataLoad

-- |Specifies scales contours and the like.
data SmokeviewDataConfig = SmokeviewDataConfig

slicePropertiesT :: SlicePropertiesDict
slicePropertiesT
    = modifySliceKey "temp" (sliceColourBounds . dBoundMax .~ (Just 200))
    $ modifySliceKey "VIS_Soot" (sliceColourBounds . dBoundMax .~ (Just 20))
    $ modifySliceKey "temp" (sliceContourValue.~ (Just 60))
    $ modifySliceKey "X_CO" (sliceColourBounds . dBoundMax .~ (Just 0.002000))
    $ modifySliceKey "X_CO" (sliceContourValue.~ (Just 0.001000))
    $ def

viewConfigToIniConfig :: SmokeviewViewConfig -> IniConfig
viewConfigToIniConfig SmokeviewViewConfig{..}
    =
    -- iniConfigFDSFilename .~ "ignore"
    -- $ iniConfigSliceProperties .~ slicePropertiesT
    -- $
    iniConfig_VIEWPOINT5s .~ [viewpoint5]
    -- $ iniConfigLabelStartupView .~ Just vpName
    -- -- $ iniConfigXYZClip .~ xyzClip
    -- $ iniConfigWindowSize .~ (WindowSize wx wy)
    $ def
    where
        (wx,wy) = smokeviewConfig_windowSize
        -- xyzClip = xyzClipOnOff .~ 0
        --         $ def
        vpName = "theViewPoint"
        toDataBounds (minV, maxV) = DataBounds
            { _dBoundMin = minV
            , _dBoundMax = maxV
            }
        (viewDirection, eyePos) = toViewDirection smokeviewConfig_viewDirection
        viewpoint5
            = viewpoint5EyeView .~ 0
            $ viewpoint5RotationIndex .~ 8
            $ viewpoint5ViewID .~ 4
            -- $ viewPointEyePos .~ (0.507244, -2.472571, 0.320589)
            -- $ viewPointEyePos .~ (0.302240, -2.337570, 0.045590)
            $ viewpoint5EyePos .~ eyePos
            $ viewpoint5Zoom .~ 1.0
            $ viewpoint5ZoomIndex .~ 2
            $ viewpoint5ViewAngle .~ 0.0
            $ viewpoint5DirectionAngle .~ 0.0
            $ viewpoint5ElevationAngle .~ 0.0
            $ viewpoint5ProjectionType .~ Orthographic
            $ viewpoint5ViewDir .~  {- smokeviewConfig_viewDir -} (0.0, 0.0, 0.0)
            -- the viewPointViewDir parameter appears to have little affect in
            -- Orthographic projection.
            $ viewpoint5ZAngle .~ (viewDirectionToTuple viewDirection)
            $ viewpoint5TransformMatrix .~ def
            $ viewpoint5GlobalClippingFlag .~ (clippingTypeToInt smokeviewConfig_clippingType)
            $ viewpoint5XClipping .~ (toDataBounds smokeviewConfig_clippingX)
            $ viewpoint5YClipping .~ (toDataBounds smokeviewConfig_clippingY)
            $ viewpoint5ZClipping .~ (toDataBounds smokeviewConfig_clippingZ)
            $ viewpoint5Name .~ vpName
            $ def
