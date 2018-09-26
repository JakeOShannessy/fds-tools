{-# LANGUAGE RankNTypes, TemplateHaskell, FlexibleInstances #-}
module FDSUtilities.Types.Smokeview where

import Control.Lens
import Data.List (intercalate)
import Data.Default
import Data.Colour.RGBSpace
import qualified Data.Map as M

import Text.Printf


data Script = Script [ScriptInstruction] deriving (Show)

data ScriptInstruction
    = SILoadFile FilePath
    | SILoadVFile FilePath
    | SIRender String
    | SISetTime Double
    | SIUnloadAll
    | SISetRenderDir FilePath
    | SILoadIniFile FilePath
    | SISetViewpoint Viewpoint5
    | SISetSceneClip Axis Bool Double Bool Double
    deriving Show


addIndent indent string = intercalate "\n" $ map (indent ++) $ lines string

-- TODO: the inifile needs to be represented here completely.
-- This is necessary in order to control all defaults, even
-- if they aren't modified by the script, we want to replace all defaults.
data IniConfig = IniConfig
    { _iniConfig_ADJUSTALPHA :: Int -- 0, 1, or 2
    , _iniConfig_AMBIENTLIGHT :: RGB Double
    , _iniConfig_APERTURE :: Int
    -- , _iniConfig_AVATAREVAC
    , _iniConfig_AXISSMOOTH :: Bool
    , _iniConfig_BACKGROUNDCOLOR :: RGB Double
    -- , _iniConfig_BLOCKCOLOR
    -- , _iniConfig_BLOCKLOCATION
    -- , _iniConfig_BLOCKSHININESS
    -- , _iniConfig_BLOCKSPECULAR
    -- , _iniConfig_BOUNDARYTWOSIDE
    -- , _iniConfig_BOUNDCOLOR
    -- , _iniConfig_BOUNDZIPSTEP
    -- , _iniConfig_CACHE_BOUNDARYDATA
    -- , _iniConfig_CACHE_QDATA
    -- , _iniConfig_CELLCENTERTEXT
    -- , _iniConfig_CLIP
    -- , _iniConfig_COLOR2BAR
    -- , _iniConfig_COLORBAR
    -- , _iniConfig_COLORBARFLIP
    -- , _iniConfig_COLORBARTYPE
    -- , _iniConfig_COLORBAR_FLIP
    -- , _iniConfig_COLORTABLE
    -- , _iniConfig_COMPRESSAUTO
    -- , _iniConfig_CONTOURTYPE
    -- , _iniConfig_CUBETETRATEST
    -- , _iniConfig_CULLFACES
    -- , _iniConfig_C_BOUNDARY
    -- , _iniConfig_C_ISO
    -- , _iniConfig_C_PARTICLES
    -- , _iniConfig_C_PLOT3D
    -- , _iniConfig_C_SLICE
    -- , _iniConfig_DEVICEBOUNDS
    -- , _iniConfig_DEVICENORMLENGTH
    -- , _iniConfig_DEVICEORIENTATION
    -- , _iniConfig_DEVICEVECTORDIMENSIONS
    -- , _iniConfig_DIFFUSELIGHT
    -- , _iniConfig_DIRECTIONCOLOR
    -- , _iniConfig_ENABLETEXTURELIGHTING
    -- , _iniConfig_EXTREMECOLORS
    -- , _iniConfig_EYEVIEW
    -- , _iniConfig_EYEX
    -- , _iniConfig_EYEY
    -- , _iniConfig_EYEZ
    -- , _iniConfig_FED
    -- , _iniConfig_FEDCOLORBAR
    -- , _iniConfig_FIRECOLOR
    -- , _iniConfig_FIRECOLORMAP
    -- , _iniConfig_FIREDEPTH
    -- , _iniConfig_FLIP
    -- , _iniConfig_FONTSIZE
    -- , _iniConfig_FOREGROUNDCOLOR
    -- , _iniConfig_FRAMERATE
    -- , _iniConfig_FRAMERATEVALUE
    -- , _iniConfig_GCOLORBAR
    -- , _iniConfig_GEOMDIAGS
    -- , _iniConfig_GRIDLINEWIDTH
    -- , _iniConfig_GRIDPARMS
    -- , _iniConfig_GSLICEPARMS
    -- , _iniConfig_GVERSION
    -- , _iniConfig_HEATOFFCOLOR
    -- , _iniConfig_HEATONCOLOR
    -- , _iniConfig_INPUT_FILE
    -- , _iniConfig_ISOAUTO
    -- , _iniConfig_ISOCOLORS
    -- , _iniConfig_ISOLINEWIDTH
    -- , _iniConfig_ISOPOINTSIZE
    -- , _iniConfig_ISOTRAN2
    -- , _iniConfig_ISOZIPSTEP
    -- , _iniConfig_LABEL
    -- , _iniConfig_LABELSTARTUPVIEW
    -- , _iniConfig_LIGHT0
    -- , _iniConfig_LIGHT1
    -- , _iniConfig_LIGHTMODELLOCALVIEWER
    -- , _iniConfig_LIGHTMODELSEPARATESPECULARCOLOR
    -- , _iniConfig_LIGHTPOS0
    -- , _iniConfig_LIGHTPOS1
    -- , _iniConfig_LINEWIDTH
    -- , _iniConfig_LOADFILESATSTARTUP
    -- , _iniConfig_MESHOFFSET
    -- , _iniConfig_MESHVIS
    -- , _iniConfig_MSCALE
    -- , _iniConfig_MSLICEAUTO
    -- , _iniConfig_NOPART
    -- , _iniConfig_OFFSETSLICE
    -- , _iniConfig_OUTLINEMODE
    -- , _iniConfig_P3CONT3DSMOOTH
    -- , _iniConfig_P3DSURFACESMOOTH
    -- , _iniConfig_P3DSURFACETYPE
    -- , _iniConfig_P3VIEW
    -- , _iniConfig_PART5CLASSVIS
    -- , _iniConfig_PART5COLOR
    -- , _iniConfig_PART5PROPDISP
    -- , _iniConfig_PARTAUTO
    -- , _iniConfig_PARTPOINTSIZE
    -- , _iniConfig_PARTPOINTSTEP
    -- , _iniConfig_PATCHAUTO
    -- , _iniConfig_PATCHDATAOUT
    -- , _iniConfig_PERCENTILELEVEL
    -- , _iniConfig_PIXELSKIP
    -- , _iniConfig_PLOT3DAUTO
    -- , _iniConfig_PLOT3DLINEWIDTH
    -- , _iniConfig_PLOT3DPOINTSIZE
    -- , _iniConfig_PROJECTION
    -- , _iniConfig_PROPINDEX
    -- , _iniConfig_RENDERCLIP
    -- , _iniConfig_RENDERFILELABEL
    -- , _iniConfig_RENDERFILETYPE
    -- , _iniConfig_RENDEROPTION
    -- , _iniConfig_S3DAUTO
    -- , _iniConfig_SBATSTART
    -- , _iniConfig_SCALEDFONT
    -- , _iniConfig_SCRIPTFILE
    -- , _iniConfig_SENSORABSSIZE
    -- , _iniConfig_SENSORCOLOR
    -- , _iniConfig_SENSORNORMCOLOR
    -- , _iniConfig_SENSORRELSIZE
    -- , _iniConfig_SETBW
    -- , _iniConfig_SHOOTER
    -- , _iniConfig_SHOWALLTEXTURES
    -- , _iniConfig_SHOWAXISLABELS
    -- , _iniConfig_SHOWBLOCKLABEL
    -- , _iniConfig_SHOWBLOCKS
    -- , _iniConfig_SHOWCADANDGRID
    -- , _iniConfig_SHOWCADOPAQUE
    -- , _iniConfig_SHOWCEILING
    -- , _iniConfig_SHOWCOLORBARS
    -- , _iniConfig_SHOWCVENTS
    -- , _iniConfig_SHOWDEVICES
    -- , _iniConfig_SHOWDEVICEVALS
    -- , _iniConfig_SHOWDUMMYVENTS
    -- , _iniConfig_SHOWEVACSLICES
    -- , _iniConfig_SHOWEXTREMEDATA
    -- , _iniConfig_SHOWFEDAREA
    -- , _iniConfig_SHOWFLOOR
    -- , _iniConfig_SHOWFRAME
    -- , _iniConfig_SHOWFRAMELABEL
    -- , _iniConfig_SHOWFRAMERATE
    -- , _iniConfig_SHOWGRID
    -- , _iniConfig_SHOWGRIDLOC
    -- , _iniConfig_SHOWHAZARDCOLORS
    -- , _iniConfig_SHOWHMSTIMELABEL
    -- , _iniConfig_SHOWHRRCUTOFF
    -- , _iniConfig_SHOWHRRLABEL
    -- , _iniConfig_SHOWHZONE
    -- , _iniConfig_SHOWIGNITION
    -- , _iniConfig_SHOWISO
    -- , _iniConfig_SHOWISONORMALS
    -- , _iniConfig_SHOWLABELS
    -- , _iniConfig_SHOWMEMLOAD
    -- , _iniConfig_SHOWMISSINGOBJECTS
    -- , _iniConfig_SHOWNORMALWHENSMOOTH
    -- , _iniConfig_SHOWOPENVENTS
    -- , _iniConfig_SHOWOTHERVENTS
    -- , _iniConfig_SHOWPATHNODES
    -- , _iniConfig_SHOWSENSORS
    -- , _iniConfig_SHOWSLICEINOBST
    -- , _iniConfig_SHOWSMOKEPART
    -- , _iniConfig_SHOWSPRINKPART
    -- , _iniConfig_SHOWSTREAK
    -- , _iniConfig_SHOWSZONE
    -- , _iniConfig_SHOWTERRAIN
    -- , _iniConfig_SHOWTETRAS
    -- , _iniConfig_SHOWTHRESHOLD
    -- , _iniConfig_SHOWTICKS
    -- , _iniConfig_SHOWTIMEBAR
    -- , _iniConfig_SHOWTIMELABEL
    -- , _iniConfig_SHOWTITLE
    -- , _iniConfig_SHOWTOURROUTE
    -- , _iniConfig_SHOWTRACERSALWAYS
    -- , _iniConfig_SHOWTRANSPARENT
    -- , _iniConfig_SHOWTRANSPARENTVENTS
    -- , _iniConfig_SHOWTRIANGLECOUNT
    -- , _iniConfig_SHOWTRIANGLES
    -- , _iniConfig_SHOWVENTFLOW
    -- , _iniConfig_SHOWVENTS
    -- , _iniConfig_SHOWVZONE
    -- , _iniConfig_SHOWWALLS
    -- , _iniConfig_SHOWZONEFIRE
    -- , _iniConfig_SKIPEMBEDSLICE
    -- , _iniConfig_SKYBOX
    -- , _iniConfig_SLICEAUTO
    -- , _iniConfig_SLICEAVERAGE
    -- , _iniConfig_SLICEDATAOUT
    -- , _iniConfig_SLICEOFFSET
    -- , _iniConfig_SLICEZIPSTEP
    -- , _iniConfig_SMOKE3DZIPSTEP
    -- , _iniConfig_SMOKEALBEDO
    -- , _iniConfig_SMOKECULL
    -- , _iniConfig_SMOKERTHICK
    -- , _iniConfig_SMOKESENSORS
    -- , _iniConfig_SMOKESKIP
    -- , _iniConfig_SMOKETHICK
    -- , _iniConfig_SMOOTHBLOCKSOLID
    -- , _iniConfig_SMOOTHLINES
    -- , _iniConfig_SPEED
    -- , _iniConfig_SPHERESEGS
    -- , _iniConfig_SPRINKLERABSSIZE
    -- , _iniConfig_SPRINKOFFCOLOR
    -- , _iniConfig_SPRINKONCOLOR
    -- , _iniConfig_STARTUPLANG
    -- , _iniConfig_STATICPARTCOLOR
    -- , _iniConfig_STEREO
    -- , _iniConfig_STREAKLINEWIDTH
    -- , _iniConfig_SURFINC
    -- , _iniConfig_TERRAINPARMS
    -- , _iniConfig_TICKS
    -- , _iniConfig_TIMEBARCOLOR
    -- , _iniConfig_TIMEOFFSET
    -- , _iniConfig_TITLESAFE
    -- , _iniConfig_TLOAD
    -- , _iniConfig_TOURCOLORS
    -- , _iniConfig_TOURCONSTANTVEL
    -- , _iniConfig_TOURINDEX
    -- , _iniConfig_TOURS
    -- , _iniConfig_TOUR_AVATAR
    -- , _iniConfig_TRAINERMODE
    -- , _iniConfig_TRAINERVIEW
    -- , _iniConfig_TRANSPARENT
    -- , _iniConfig_TREECOLORS
    -- , _iniConfig_TWOSIDEDVENTS
    -- , _iniConfig_UNITCLASSES
    -- , _iniConfig_UNLOAD_QDATA
    -- , _iniConfig_USEGPU
    -- , _iniConfig_USENEWDRAWFACE
    -- , _iniConfig_USERTICKS
    -- , _iniConfig_USER_ROTATE
    -- , _iniConfig_V5_PARTICLES
    -- , _iniConfig_VECCONTOURS
    -- , _iniConfig_VECLENGTH
    -- , _iniConfig_VECTORLINEWIDTH
    -- , _iniConfig_VECTORPOINTSIZE
    -- , _iniConfig_VECTORSKIP
    -- , _iniConfig_VENTCOLOR
    -- , _iniConfig_VENTLINEWIDTH
    -- , _iniConfig_VENTOFFSET
    -- , _iniConfig_VIEWALLTOURS
    -- , _iniConfig_VIEWPOINT3
    -- , _iniConfig_VIEWPOINT4
    , _iniConfig_VIEWPOINT5s :: [Viewpoint5]
    -- , _iniConfig_VIEWPOINT6
    -- , _iniConfig_VIEWTIMES
    -- , _iniConfig_VIEWTOURFROMPATH
    -- , _iniConfig_VOLSMOKE
    -- , _iniConfig_VSLICEAUTO
    -- , _iniConfig_V_BOUNDARY
    -- , _iniConfig_V_ISO
    -- , _iniConfig_V_PARTICLES
    -- , _iniConfig_V_PLOT3D
    -- , _iniConfig_V_SLICE
    -- , _iniConfig_V_TARGET
    -- , _iniConfig_V_ZONE
    -- , _iniConfig_WINDOWHEIGHT
    -- , _iniConfig_WINDOWOFFSET
    -- , _iniConfig_WINDOWWIDTH
    -- , _iniConfig_XYZCLIP
    -- , _iniConfig_ZAXISANGLES
    -- , _iniConfig_ZOOM
    } deriving Show

instance Default IniConfig  where
    def = IniConfig
        { _iniConfig_ADJUSTALPHA = 1
        , _iniConfig_AMBIENTLIGHT = RGB 0.6 0.6 0.6
        , _iniConfig_APERTURE  = 2
        -- , _iniConfig_AVATAREVAC
        , _iniConfig_AXISSMOOTH = True
        , _iniConfig_BACKGROUNDCOLOR = RGB 0.0 0.0 0.0
        , _iniConfig_VIEWPOINT5s = []
        }

instance FDSPrint IniConfig where
    fdsPrint conf
        =  (unlines
          ([ "ADJUSTALPHA\n"    ++ "  " ++ fdsPrint (_iniConfig_ADJUSTALPHA conf)
          , "AMBIENTLIGHT\n" ++ "  " ++ fdsPrint (_iniConfig_AMBIENTLIGHT conf)
          , "APERTURE\n"      ++ "  " ++ fdsPrint (_iniConfig_APERTURE conf)
          -- , "AVATAREVAC\n"      ++ "  " ++ fdsPrint (_iniConfig_AVATAREVAC conf)
          , "AXISSMOOTH\n"      ++ "  " ++ fdsPrint (_iniConfig_AXISSMOOTH conf)
          , "BACKGROUNDCOLOR\n"      ++ "  " ++ fdsPrint (_iniConfig_BACKGROUNDCOLOR conf)
          ] ++ map (\x->"VIEWPOINT5\n" ++ fdsPrint x) (_iniConfig_VIEWPOINT5s conf))
        )

-- data IniConfigLua = IniConfigLua
    -- { _iniConfigLuaColors :: ConfigColors
    -- , _iniConfigLuaSizes :: ConfigSizes
    -- , _iniConfigLuaTimeDataBounds :: ConfigTimeDataBounds
    -- -- , _iniConfigLuaVSliceBounds :: VSliceBounds
    -- -- , _iniConfigLuaCSliceBounds :: CSliceBounds
    -- -- , _iniConfigLuaValueMinMax :: ConfigValueMinMax
    -- , _iniConfigLuaDataLoading :: ConfigDataLoading
    -- , _iniConfigLuaContours :: ConfigContours
    -- , _iniConfigLuaVisibility :: ConfigVisibility
    -- , _iniConfigLuaMisc :: ConfigMisc
    -- , _iniConfigLuaZone :: ConfigZone
    -- , _iniConfigLua3DSmokeInfo :: Config3DSmokeInfo
    -- , _iniConfigLuaTourInfo :: ConfigTourInfo
    -- } deriving Show

-- instance Default IniConfigLua where
  -- def = IniConfigLua
    -- { _iniConfigLuaColors = def
    -- , _iniConfigLuaSizes = def
    -- , _iniConfigLuaTimeDataBounds = def
    -- -- , _iniConfigLuaVSliceBounds = undefined
    -- -- , _iniConfigLuaCSliceBounds = undefined
    -- -- , _iniConfigLuaValueMinMax = undefined
    -- , _iniConfigLuaDataLoading = undefined
    -- , _iniConfigLuaContours = undefined
    -- , _iniConfigLuaVisibility = undefined
    -- , _iniConfigLuaMisc = undefined
    -- , _iniConfigLuaZone = undefined
    -- , _iniConfigLua3DSmokeInfo = undefined
    -- , _iniConfigLuaTourInfo = undefined


   -- -- , _iniConfigLuaLighting :: ConfigLighting
   -- -- , _iniConfigLuaSizes :: ConfigSizes
   -- -- , _iniConfigLuaLines :: ConfigLines
   -- -- , _iniConfigLuaOffsets :: ConfigOffsets
   -- -- , _iniConfigLuaTimeMinMax :: ConfigTimeMinMax
   -- -- , _iniConfigLuaVSliceBounds :: VSliceBounds
   -- -- , _iniConfigLuaCSliceBounds :: CSliceBounds
   -- -- , _iniConfigLuaValueMinMax :: ConfigValueMinMax
   -- -- , _iniConfigLuaDataLoading :: ConfigDataLoading
   -- -- , _iniConfigLuaContours :: ConfigContours
   -- -- , _iniConfigLuaVisibility :: ConfigVisibility
   -- -- , _iniConfigLuaMisc :: ConfigMisc
   -- -- , _iniConfigLuaZone :: ConfigZone
   -- -- , _iniConfigLua3DSmokeInfo :: Config3DSmokeInfo
   -- -- , _iniConfigLuaTourInfo :: ConfigTourInfo
    -- }

-- instance FDSPrint IniConfigLua where
    -- fdsPrint conf
        -- =  " ------------ global ini settings ------------\n"
        -- ++ fdsPrint (_iniConfigLuaColors conf :: ConfigColors)
        -- ++ fdsPrint (_iniConfigLuaSizes conf :: ConfigSizes)
        -- ++ fdsPrint (_iniConfigLuaTimeDataBounds conf :: ConfigTimeDataBounds)
   -- -- , _iniConfigLuaSizes :: ConfigSizes
   -- -- , _iniConfigLuaLines :: ConfigLines
   -- -- , _iniConfigLuaOffsets :: ConfigOffsets
   -- -- , _iniConfigLuaTimeMinMax :: ConfigTimeMinMax
   -- -- , _iniConfigLuaVSliceBounds :: VSliceBounds
   -- -- , _iniConfigLuaCSliceBounds :: CSliceBounds
   -- -- , _iniConfigLuaValueMinMax :: ConfigValueMinMax
   -- -- , _iniConfigLuaDataLoading :: ConfigDataLoading
   -- -- , _iniConfigLuaContours :: ConfigContours
   -- -- , _iniConfigLuaVisibility :: ConfigVisibility
   -- -- , _iniConfigLuaMisc :: ConfigMisc
        -- -- ++ fdsPrint (_iniConfigLuaZone conf)
   -- -- , _iniConfigLua3DSmokeInfo :: Config3DSmokeInfo
   -- -- , _iniConfigLuaTourInfo :: ConfigTourInfo

instance FDSPrint Bool where
    fdsPrint True = "1"
    fdsPrint False = "0"

-- data ConfigColors = ConfigColors
  -- { _configColorsColorBar        :: ConfigColorBar
  -- , _configColorsColor2Bar       :: ConfigColor2Bar
  -- , _configColorsIsoColors       :: ConfigIsoColors
  -- , _configColorsVentColor       :: RGB Double
  -- , _configColorsSensorColor     :: RGB Double
  -- , _configColorsSensorNormColor :: RGB Double
  -- , _configColorsHeatOnColor     :: RGB Double
  -- , _configColorsHeatOffColor    :: RGB Double
  -- , _configColorsSprinkOnColor   :: RGB Double
  -- , _configColorsSprinkOffColor  :: RGB Double
  -- , _configColorsBlockColor      :: RGB Double
  -- , _configColorsBoundColor      :: RGB Double
  -- , _configColorsStaticPartColor :: RGB Double
  -- , _configColorsAmbientLight    :: RGB Double
  -- , _configColorsBackgroundColor :: RGB Double
  -- , _configColorsForegroundColor :: RGB Double
  -- , _configColorsFlip            :: Bool
  -- , _configColorsTimebarColor    :: RGB Double
  -- , _configColorsSetBW           :: Bool
  -- , _configColorsColorbarFlip    :: Bool
  -- , _configColorsDiffuseLight    :: RGB Double
  -- , _configColorsDirectionColor  :: RGB Double
  -- } deriving Show

-- instance Default ConfigColors where
  -- def = ConfigColors
    -- { _configColorsColorBar        = def
    -- , _configColorsColor2Bar       = def
    -- , _configColorsIsoColors       = def
    -- , _configColorsVentColor       = RGB 1 0 1
    -- , _configColorsSensorColor     = RGB 1 1 0
    -- , _configColorsSensorNormColor = RGB 1 1 0
    -- , _configColorsHeatOnColor     = RGB 1 0 0
    -- , _configColorsHeatOffColor    = RGB 1 0 0
    -- , _configColorsSprinkOnColor   = RGB 0 1 0
    -- , _configColorsSprinkOffColor  = RGB 1 0 0
    -- , _configColorsBlockColor      = RGB 1 0.8 0.4
    -- , _configColorsBoundColor      = RGB 0.5 0.5 0.5
    -- , _configColorsStaticPartColor = RGB 0 1 0
    -- , _configColorsAmbientLight    = RGB 0.6 0.6 0.6
    -- , _configColorsBackgroundColor = RGB 0 0 0
    -- , _configColorsForegroundColor = RGB 1 1 1
    -- , _configColorsFlip            = True
    -- , _configColorsTimebarColor    = RGB 0.6 0.6 0.6
    -- , _configColorsSetBW           = False
    -- , _configColorsColorbarFlip    = False
    -- , _configColorsDiffuseLight    = RGB 0.5 0.5 0.5
    -- , _configColorsDirectionColor  = RGB 0.152941 0.250980 0.545098
    -- }

-- instance FDSPrint ConfigColors where
    -- fdsPrint conf
        -- =  "\n   *** COLOR/LIGHTING ***\n\n"
        -- ++ (unlines
          -- [ "AMBIENTLIGHT\n"    ++ "  " ++ fdsPrint (_configColorsAmbientLight conf)
          -- , "BACKGROUNDCOLOR\n" ++ "  " ++ fdsPrint (_configColorsBackgroundColor conf)
          -- , "BLOCKCOLOR\n"      ++ "  " ++ fdsPrint (_configColorsBlockColor conf)
          -- , "BOUNDCOLOR\n"      ++ "  " ++ fdsPrint (_configColorsBoundColor conf)
          -- , "COLORBAR\n"        ++ fdsPrint (_configColorsColorBar conf)
          -- , "COLOR2BAR\n"       ++ fdsPrint (_configColorsColor2Bar conf)
          -- , "COLORBAR_FLIP\n"   ++ "  " ++ fdsPrint (_configColorsColorbarFlip conf)
          -- , "DIFFUSELIGHT\n"    ++ "  " ++ fdsPrint (_configColorsDiffuseLight conf)
          -- , "DIRECTIONCOLOR\n"  ++ "  " ++ fdsPrint (_configColorsDirectionColor conf)
          -- , "FLIP\n"            ++ "  " ++ fdsPrint (_configColorsFlip conf)
          -- , "FOREGROUNDCOLOR\n" ++ "  " ++ fdsPrint (_configColorsForegroundColor conf)
          -- , "HEATOFFCOLOR\n"    ++ "  " ++ fdsPrint (_configColorsHeatOffColor conf)
          -- , "HEATONCOLOR\n"     ++ "  " ++ fdsPrint (_configColorsHeatOnColor conf)
          -- , "ISOCOLORS\n"       ++ fdsPrint (_configColorsIsoColors conf)
          -- , "SENSORCOLOR\n"     ++ "  " ++ fdsPrint (_configColorsSensorColor conf)
          -- , "SENSORNORMCOLOR\n" ++ "  " ++ fdsPrint (_configColorsSensorNormColor conf)
          -- , "SETBW\n"           ++ "  " ++ fdsPrint (_configColorsSetBW conf)
          -- , "SPRINKOFFCOLOR\n"  ++ "  " ++ fdsPrint (_configColorsSprinkOffColor conf)
          -- , "SPRINKONCOLOR\n"   ++ "  " ++ fdsPrint (_configColorsSprinkOnColor conf)
          -- , "STATICPARTCOLR\n"  ++ "  " ++ fdsPrint (_configColorsStaticPartColor conf)
          -- , "TIMEBARCOLOR\n"    ++ "  " ++ fdsPrint (_configColorsTimebarColor conf)
          -- , "VENTCOLOR\n"       ++ "  " ++ fdsPrint (_configColorsVentColor conf)
          -- ]
        -- )

-- instance Show a => FDSPrint (RGB a) where
    -- fdsPrint (RGB r g b) = intercalate " " $ map show [r,g,b]

-- -- the two integers (after the number of colors) are of unkown purpose
-- data ConfigColorBar = ConfigColorBar Int Int ([RGB Double]) deriving Show

-- instance Default ConfigColorBar where
  -- def = ConfigColorBar 1 (-1)
    -- [ RGB 0 0 1
    -- , RGB 0 0.359375 1
    -- , RGB 0 0.718750 1
    -- , RGB 0 1 0.921875
    -- , RGB 0 1 0.562500
    -- , RGB 0 1 0.203125
    -- , RGB 0.171875 1 0
    -- , RGB 0.531250 1 0
    -- , RGB 0.890625 1 0
    -- , RGB 1 0.746032 0
    -- , RGB 1 0.380952 0
    -- , RGB 1 0 0
    -- ]

-- instance FDSPrint ConfigColorBar where
  -- fdsPrint (ConfigColorBar a b colors) = ( addIndent " " (
    -- show (length colors) ++ " " ++show a ++ " " ++ show b ++ "\n" ++ intercalate "\n" (map strColor colors)))
    -- where
      -- strColor (RGB r g b)= show r ++ " " ++ show g ++ " " ++ show b

-- data ConfigColor2Bar = ConfigColor2Bar [(RGB Double, String)] deriving Show

-- instance Default ConfigColor2Bar where
  -- def = ConfigColor2Bar
    -- [ (RGB 1 1 1, "white")
    -- , (RGB 1 1 0, "yellow")
    -- , (RGB 0 0 1, "blue")
    -- , (RGB 1 0 0, "red")
    -- , (RGB 0 1 0, "green")
    -- , (RGB 1 0 1, "magenta")
    -- , (RGB 0 1 1, "cyan")
    -- , (RGB 0 0 0, "black")
    -- ]

-- instance FDSPrint ConfigColor2Bar where
  -- fdsPrint (ConfigColor2Bar colors) = (addIndent " " (
    -- show (length colors) ++ "\n" ++ intercalate "\n" (map strColor colors)))
    -- where
      -- strColor (RGB r g b, name)= show r ++ " " ++ show g ++ " " ++ show b ++ " :" ++ name

-- data ConfigIsoColors = ConfigIsoColors
  -- { _configIsoColorsShininess :: Double
  -- , _configIsoColorsTransparency :: Double
  -- , _configIsoColorsSpecular :: RGB Double
  -- , _configIsoColorsColors :: [RGB Double]
  -- } deriving Show

-- instance Default ConfigIsoColors where
  -- def = ConfigIsoColors
    -- { _configIsoColorsShininess = 10
    -- , _configIsoColorsTransparency = 0.8
    -- , _configIsoColorsSpecular = RGB 0.7 0.7 0.7
    -- , _configIsoColorsColors =
        -- [ RGB 0.96 0 0.96
        -- , RGB 0.75 0.8 0.8
        -- , RGB 0 0.96 0.28
        -- ]
    -- }

-- instance FDSPrint ConfigIsoColors where
  -- fdsPrint _ = "undefined"


-- data ConfigSizes = ConfigSizes
  -- { _configSizesVectorPointSize :: Double
  -- , _configSizesVectorLineWidth :: (Double, Double)
  -- , _configSizesVecLength :: (Int, Double, Double)
  -- , _configSizesPartPointSize :: Double
  -- , _configSizesStreakLineWidth :: Double
  -- , _configSizesIsoPointSize :: Double
  -- , _configSizesIsoLineWidth :: Double
  -- , _configSizesPlot3DPointSize :: Double
  -- , _configSizesGridLineWidth :: Double
  -- , _configSizesPlot3DLineWidth :: Double
  -- , _configSizesSensorAbsSize :: Double
  -- , _configSizesSensorRelSize :: Double
  -- , _configSizesSprinklerAbsSize :: Double
  -- , _configSizesSphereSegs :: Int
  -- , _configSizesWindowWidth :: Int
  -- , _configSizesWindowHeight :: Int
  -- , _configSizesWindowOffset :: Int
  -- , _configSizesLineWidth :: Double
  -- , _configSizesVentLineWidth :: Double
  -- , _configSizesVecContours :: Bool
  -- , _configSizesSmoothLines :: Int
  -- , _configSizesUseNewDrawFace :: Bool
  -- , _configSizesVentOffset :: Double
  -- , _configSizesSliceOffset :: Double
  -- } deriving Show



-- instance Default ConfigSizes where
  -- def = ConfigSizes
    -- { _configSizesVectorPointSize = 2.0
    -- , _configSizesVectorLineWidth = (1.0, 1.0)
    -- , _configSizesVecLength = (4, 1.0, 1.0)
    -- , _configSizesPartPointSize = 4.0
    -- , _configSizesStreakLineWidth = 1.0
    -- , _configSizesIsoPointSize = 4.0
    -- , _configSizesIsoLineWidth = 2.0
    -- , _configSizesPlot3DPointSize = 4.0
    -- , _configSizesGridLineWidth = 2.0
    -- , _configSizesPlot3DLineWidth = 2.0
    -- , _configSizesSensorAbsSize = 0.038
    -- , _configSizesSensorRelSize = 1.0
    -- , _configSizesSprinklerAbsSize = 0.07600
    -- , _configSizesSphereSegs = 6
    -- , _configSizesWindowWidth = 1900
    -- , _configSizesWindowHeight = 600
    -- , _configSizesWindowOffset = 45
    -- , _configSizesLineWidth = 2.0
    -- , _configSizesVentLineWidth = 2.0
    -- , _configSizesVecContours = False
    -- , _configSizesSmoothLines = 1
    -- , _configSizesUseNewDrawFace =  False
    -- , _configSizesVentOffset = 0.1
    -- , _configSizesSliceOffset = 0.1
    -- }

-- instance FDSPrint ConfigSizes where
    -- fdsPrint conf
        -- =  "\n   *** SIZES/OFFSETS ***\n\n"
        -- ++ (unlines
          -- [ "GRIDLINEWIDTH\n"    ++ "  " ++ fdsPrint (_configSizesGridLineWidth conf)
          -- , "ISOLINEWIDTH\n"     ++ "  " ++ fdsPrint (_configSizesIsoLineWidth conf)
          -- , "ISOPOINTSIZE\n"     ++ "  " ++ fdsPrint (_configSizesIsoPointSize conf)
          -- , "LINEWIDTH\n"        ++ "  " ++ fdsPrint (_configSizesLineWidth conf)
          -- , "PARTPOINTSIZE\n"    ++ "  " ++ fdsPrint (_configSizesPartPointSize conf)
          -- , "PLOT3DLINEWIDTH\n"  ++ "  " ++ fdsPrint (_configSizesPlot3DLineWidth conf)
          -- , "PLOT3DPOINTSIZE\n"  ++ "  " ++ fdsPrint (_configSizesPlot3DPointSize conf)
          -- , "SENSORABSSIZE\n"    ++ "  " ++ fdsPrint (_configSizesSensorAbsSize conf)
          -- , "SENSORRELSIZE\n"    ++ "  " ++ fdsPrint (_configSizesSensorRelSize conf)
          -- , "SLICEOFFSET\n"      ++ "  " ++ fdsPrint (_configSizesSliceOffset conf)
          -- , "SMOOTLINES\n"       ++ "  " ++ fdsPrint (_configSizesSmoothLines conf)
          -- , "SPHERESEGS\n"       ++ "  " ++ fdsPrint (_configSizesSphereSegs conf)
          -- , "SPRINKLERABSSIZE\n" ++ "  " ++ fdsPrint (_configSizesSprinklerAbsSize conf)
          -- , "STREAKLINEWIDTH\n"  ++ "  " ++ fdsPrint (_configSizesStreakLineWidth conf)
          -- , "USENEWDRAWFACE\n"   ++ "  " ++ fdsPrint (_configSizesUseNewDrawFace conf)
          -- , "VECCONTOURS\n"      ++ "  " ++ fdsPrint (_configSizesVecContours conf)
          -- , "VECLENGTH\n"        ++ "  " ++ fdsPrint (_configSizesVecLength conf)
          -- , "VECTORLINEWIDTH\n"  ++ "  " ++ fdsPrint (_configSizesVectorLineWidth conf)
          -- , "VECTORPOINTSIZE\n"  ++ "  " ++ fdsPrint (_configSizesVectorPointSize conf)
          -- , "VENTLINEWIDTH\n"    ++ "  " ++ fdsPrint (_configSizesLineWidth conf)
          -- , "VENTOFFSET\n"       ++ "  " ++ fdsPrint (_configSizesVentOffset conf)
          -- , "WINDOWOFFSET\n"     ++ "  " ++ fdsPrint (_configSizesWindowOffset conf)
          -- , "WINDOWWIDTH\n"      ++ "  " ++ fdsPrint (_configSizesWindowWidth conf)
          -- , "WINDOWHEIGHT\n"     ++ "  " ++ fdsPrint (_configSizesWindowHeight conf)
          -- ]
        -- )


-- data ConfigTimeDataBounds = ConfigTimeDataBounds
    -- { _configTimeDataBoundsCParticles :: [(DataBounds, String)]
    -- , _configTimeDataBoundsCPlot3d :: [DataBounds]
    -- , _configTimeDataBoundsCSlice :: [CSliceBounds]

    -- , _configTimeDataBoundsCacheBoundaryData :: Bool
    -- , _configTimeDataBoundsCacheQData :: Bool

    -- , _configTimeDataBoundsPatchDataOut :: (Bool, Double, Double, Double, Double, Double, Double, Double, Double)

    -- , _configTimeDataBoundsPercentileLevel :: Double
    -- , _configTimeDataBoundsTimeOffset :: Double
    -- , _configTimeDataBoundsTLoad :: (DataBounds, Bool, Double)

    -- , _configTimeDataBoundsVParticles :: DataBounds
    -- , _configTimeDataBoundsV5Particles :: (DataBounds, String)
    -- , _configTimeDataBoundsVPlot3d :: [DataBounds]
    -- , _configTimeDataBoundsVSlice :: [VSliceBounds]

    -- , _configTimeDataBoundsVTarget :: DataBounds

    -- } deriving Show

-- instance Default ConfigTimeDataBounds where
    -- def = ConfigTimeDataBounds
        -- { _configTimeDataBoundsCParticles = [(noBounds,""), (noBounds, "Uniform")]
        -- , _configTimeDataBoundsCPlot3d = [noBounds, noBounds, noBounds, noBounds, noBounds]
        -- , _configTimeDataBoundsCSlice
            -- [ CSliceBounds {_cSliceBoundsShortName = "VIS_Soot", _cSliceBoundsClipBounds = noBounds}]

        -- , _configTimeDataBoundsCacheBoundaryData = False
        -- , _configTimeDataBoundsCacheQData = True

        -- , _configTimeDataBoundsPatchDataOut
            -- = (False, 1, -1, 1, -1, 1, -1, 1, -1)

        -- , _configTimeDataBoundsPercentileLevel = 0.01
        -- , _configTimeDataBoundsTimeOffset = 0
        -- , _configTimeDataBoundsTLoad = (noBounds, False, 0)

        -- , _configTimeDataBoundsVParticles = noBounds
        -- , _configTimeDataBoundsV5Particles = (noBounds, "Uniform")
        -- , _configTimeDataBoundsVPlot3d = [noBounds, noBounds, noBounds, noBounds, noBounds]
        -- , _configTimeDataBoundsVSlice =
            -- [ VSliceBounds
                -- { _vSliceBoundsShortName = "VIS_Soot"
                -- , _vSliceBoundsClipBounds = noBounds
                -- , _vSliceBoundsContourBounds =  (0,1,1)}
            -- , VSliceBounds
                -- { _vSliceBoundsShortName = "X_CO"
                -- , _vSliceBoundsClipBounds = noBounds
                -- , _vSliceBoundsContourBounds =  (0,1,1)}
            -- , VSliceBounds
                -- { _vSliceBoundsShortName = "temp"
                -- , _vSliceBoundsClipBounds = noBounds
                -- , _vSliceBoundsContourBounds =  (0,1,1)}

            -- ]

        -- , _configTimeDataBoundsVTarget = noBounds

        -- }

-- data VSliceBounds = VSliceBounds
  -- { _vSliceBoundsShortName :: String
  -- , _vSliceBoundsClipBounds :: DataBounds
  -- , _vSliceBoundsContourBounds :: (Double, Double, Int) {- (Min, Max, Active)? -}
  -- } deriving Show
-- data CSliceBounds = CSliceBounds
  -- { _cSliceBoundsShortName :: String
  -- , _cSliceBoundsClipBounds :: DataBounds
  -- } deriving Show

-- -- data ConfigValueMinMax = ConfigValueMinMax
  -- -- , _configValueMinMaxSliceProperties :: SlicePropertiesDict

  -- -- , _configValueMinMaxVZone :: DataBounds
  -- -- , _configValueMinMaxUnloadQData :: Bool
  -- -- } deriving Show

-- data ConfigDataLoading = ConfigDataLoading
  -- { _configDataLoadingFED :: Bool
  -- , _configDataLoadingFEDColorBar :: String
  -- , _configDataLoadingShowFEDArea :: Bool
  -- , _configDataLoadingNoPart :: Bool
  -- , _configDataLoadingPartPointStep :: Int
  -- , _configDataLoadingSliceAverage :: (Int, Double, Int)
  -- , _configDataLoadingSliceDataOut :: Bool
  -- , _configDataLoadingSmoke3DZipStep :: Int
  -- , _configDataLoadingIsoZipStep :: Int
  -- , _configDataLoadingSliceZipStep :: Int
  -- , _configDataLoadingBoundZipStep :: Int
  -- , _configDataLoadingShowTracersAlways :: Bool
  -- , _configDataLoadingShowEvacSlices :: (Bool, Bool, Bool)
  -- , _configDataLoadingDirectionColor :: RGB Double
  -- , _configDataLoadingAvatarEvac :: Int
  -- , _configDataLoadingGridParms :: ((Int, Int, Int), (Int, Int, Int))
  -- , _configDataLoadingGSliceParms :: (Int, Int, Int, Int, Double, Double, Double, Double, Double)
  -- , _configDataLoadingShowDevices :: [String]
  -- , _configDataLoadingShowDeviceVals :: (Int, Int, Int)
  -- , _configDataLoadingSliceAuto :: (Int, Int)
  -- , _configDataLoadingMSliceAuto :: (Int, Int)
  -- , _configDataLoadingCompressAuto :: Int
  -- , _configDataLoadingLoadFilesAtStartup :: Int
  -- , _configDataLoadingPart5PropDisp :: Int
  -- , _configDataLoadingPart5Color :: RGB Double
  -- , _configDataLoadingPart5ClassVis :: (Bool, Bool)
  -- , _configDataLoadingPropIndex ::  [Int]
  -- } deriving Show

-- instance Default ConfigDataLoading where
    -- def = ConfigDataLoading
        -- { _configDataLoadingFED = False
        -- , _configDataLoadingFEDColorBar = "FED"
        -- , _configDataLoadingShowFEDArea = True
        -- , _configDataLoadingNoPart = True
        -- , _configDataLoadingPartPointStep = 1
        -- , _configDataLoadingSliceAverage = (0, 10.0, 1)
        -- , _configDataLoadingSliceDataOut = False
        -- , _configDataLoadingSmoke3DZipStep = 1
        -- , _configDataLoadingIsoZipStep = 1
        -- , _configDataLoadingSliceZipStep = 1
        -- , _configDataLoadingBoundZipStep = 1

        -- , _configDataLoadingShowTracersAlways :: Bool
        -- , _configDataLoadingShowEvacSlices :: (Bool, Bool, Bool)
        -- , _configDataLoadingDirectionColor :: RGB Double
        -- , _configDataLoadingAvatarEvac :: Int
        -- , _configDataLoadingGridParms :: ((Int, Int, Int), (Int, Int, Int))
        -- , _configDataLoadingGSliceParms :: (Int, Int, Int, Int, Double, Double, Double, Double, Double)
        -- , _configDataLoadingShowDevices :: [String]
        -- , _configDataLoadingShowDeviceVals :: (Int, Int, Int)
        -- , _configDataLoadingSliceAuto :: (Int, Int)
        -- , _configDataLoadingMSliceAuto :: (Int, Int)
        -- , _configDataLoadingCompressAuto :: Int
        -- , _configDataLoadingLoadFilesAtStartup :: Int
        -- , _configDataLoadingPart5PropDisp :: Int
        -- , _configDataLoadingPart5Color :: RGB Double
        -- , _configDataLoadingPart5ClassVis :: (Bool, Bool)
        -- , _configDataLoadingPropIndex ::  [Int]
        -- }

-- {-
-- data ConfigGridParms = ConfigGridParms
  -- {
  -- }
-- -}

-- data ConfigView = ConfigView
    -- { _configMiscAperture :: Int
    -- , _configMiscAxisSmooth :: Bool
    -- , _configMiscBlockLocation :: Int
    -- , _configViewBoundaryTwoSide :: Bool
    -- , _configViewClip :: (Double, Double)
    -- , _configViewContourType :: Int
    -- , _configViewCullFaces :: Bool
    -- , _configViewEyeView :: Int
    -- , _configViewEyeX :: Double
    -- , _configViewEyeY :: Double
    -- , _configViewEyeZ :: Double
    -- , _configViewFontSize :: Int
    -- , _configViewFrameRateValue :: Int
    -- , _configViewGeomDiags :: (Int, Int)
    -- , _configViewGVersion :: Int
    -- , _configViewIsoTran2 :: Int
    -- , _configViewMeshVis :: [Bool]
    -- , _configViewOffsetSlice :: Bool
    -- , _configViewOutlineMode :: (Int, Int)
    -- , _configViewP3DSurfaceType :: Int
    -- , _configViewP3DSurfaceSmooth :: Bool
    -- , _configViewP3View :: [(Bool, Int, Bool, Int, Bool, Int)]
    -- , _configViewProjection :: Int
    -- }

-- instance Default ConfigView where
    -- def = ConfigView
        -- { _configMiscAperture = 2
        -- , _configMiscAxisSmooth = True
        -- , _configMiscBlockLocation = 5
        -- , _configViewBoundaryTwoSide = False
        -- , _configViewClip = (0.001, 3.0)
        -- , _configViewContourType = 0
        -- , _configViewCullFaces = False
        -- , _configViewEyeView = 0
        -- , _configViewEyeX = 0.5
        -- , _configViewEyeY = -0.38482
        -- , _configViewEyeZ = 0.5
        -- , _configViewFontSize = 0
        -- , _configViewFrameRateValue = 1000
        -- , _configViewGeomDiags = (1, 0)
        -- , _configViewGVersion = 0
        -- , _configViewIsoTran2 = 4
        -- , _configViewMeshVis = [True] -- TODO: this depends on the number of meshes
        -- , _configViewOffsetSlice = False
        -- , _configViewOutlineMode = (2, 0)
        -- , _configViewP3DSurfaceType = True
        -- , _configViewP3DSurfaceSmooth = True
        -- , _configViewP3View :: [(1, 38, 1, 25, 1, 4)] -- TODO: this is dependent on the meshes
        -- , _configViewProjection = 0
        -- }

-- -- data ConfigContours = ConfigContours
  -- -- { _configContoursContourType :: Int
  -- -- , _configContoursTransparent :: (Bool, Double)
  -- -- , _configContoursSurfInc :: Bool
  -- -- } deriving Show

-- data ConfigVisibility = ConfigVisibility
  -- { _configVisibilityShowTitle :: Bool
  -- , _configVisibilityShowColorBars :: Bool
  -- , _configVisibilityShowBlocks :: Bool
  -- , _configVisibilityShowNormalWhenSmooth :: Bool
  -- , _configVisibilitySmoothBlockSolid :: Bool
  -- , _configVisibilitySBatStart :: Bool
  -- , _configVisibilityShowTransparent :: Bool
  -- -- , _configVisibilityShowVents :: (Bool, Bool, Bool)
  -- , _configVisibilityShowVents :: Bool
  -- , _configVisibilityShowTransparentVents :: Bool
  -- , _configVisibilityShowSensors :: (Bool, Bool)
  -- , _configVisibilityShowTimebar :: Bool
  -- , _configVisibilityShowTimeLabel :: Bool
  -- , _configVisibilityShowFrameLabel :: Bool
  -- , _configVisibilityShowFloor :: Bool
  -- , _configVisibilityShowWalls :: Bool
  -- , _configVisibilityShowCeiling :: Bool
  -- , _configVisibilityShowSmokePart :: Int
  -- , _configVisibilityShowSprinkPart :: Bool
  -- , _configVisibilityShowMemLoad :: Bool
  -- , _configVisibilityShowBlockLabel :: Bool
  -- , _configVisibilityShowAxisLabels :: Bool
  -- , _configVisibilityShowFrame :: Bool
  -- , _configVisibilityShowAllTextures :: Bool
  -- , _configVisibilityShowThreshold :: (Bool, Bool, Double)
  -- , _configVisibilityShowHRRCutOff :: Bool
  -- , _configVisibilityTwoSidedVents :: (Bool, Bool)
  -- , _configVisibilityTrainerView :: Bool
  -- , _configVisibilityShowTerrain :: Bool
  -- , _configVisibilityTerrainParms :: ConfigTerrainParms
  -- , _configVisibilityShowTriangles :: (Bool, Bool, Bool, Bool, Bool, Bool)
  -- , _configVisibilityShowStreak :: (Int, Int, Int, Int)
  -- , _configVisibilityShowIso :: Bool
  -- , _configVisibilityShowIsoNormals :: Bool
  -- , _configVisibilitySmokeSensors :: (Int, Int)
  -- , _configVisibilityVolSmoke :: ConfigVolSmoke
  -- } deriving Show

-- instance Default ConfigVisibility where
    -- def = ConfigVisibility
        -- { _configVisibilityShowTitle = True
        -- , _configVisibilityShowColorBars = True
        -- , _configVisibilityShowBlocks = True
        -- , _configVisibilityShowNormalWhenSmooth = True
        -- , _configVisibilitySmoothBlockSolid = False
        -- , _configVisibilitySBatStart = True
        -- , _configVisibilityShowTransparent = False
        -- , _configVisibilityShowVents = True
        -- , _configVisibilityShowTransparentVents = True
        -- , _configVisibilityShowSensors = (1, 1)
        -- , _configVisibilityShowTimebar = True
        -- , _configVisibilityShowTimeLabel = True
        -- , _configVisibilityShowFrameLabel = True
        -- , _configVisibilityShowFloor = False
        -- , _configVisibilityShowWalls = False
        -- , _configVisibilityShowCeiling = False
        -- , _configVisibilityShowSmokePart =  2
        -- , _configVisibilityShowSprinkPart = True
        -- , _configVisibilityShowMemLoad = False
        -- , _configVisibilityShowBlockLabel = True
        -- , _configVisibilityShowAxisLabels = False
        -- , _configVisibilityShowFrame = True
        -- , _configVisibilityShowAllTextures = False
        -- , _configVisibilityShowThreshold = (0, 0, 400.0)
        -- , _configVisibilityShowHRRCutOff = True
        -- , _configVisibilityTwoSidedVents = (True, False)
        -- , _configVisibilityTrainerView = True
        -- , _configVisibilityShowTerrain = False
        -- , _configVisibilityTerrainParms = def
        -- , _configVisibilityShowTriangles = (True, False, False, False, False, True)
        -- , _configVisibilityShowStreak = (0, 0, 1, -2)
        -- , _configVisibilityShowIso = True
        -- , _configVisibilityShowIsoNormals = False
        -- , _configVisibilitySmokeSensors = (0, 1)
        -- , _configVisibilityVolSmoke = def
        -- }

-- data ConfigTerrainParms = ConfigTerrainParms (Int, Int, Int) (Int, Int, Int) Double deriving Show
-- data ConfigVolSmoke = ConfigVolSmoke Int Int Int Int Int
                      -- Double Double Double Double Double Double Double deriving Show

-- data ConfigMisc = ConfigMisc
  -- { _configMiscStartupLang :: String
  -- , _configMiscShowOpenVents :: (Bool, Bool)
  -- , _configMiscShowDummyVents :: Bool
  -- , _configMiscShowOtherVents :: Bool
  -- , _configMiscShowCVents :: Bool
  -- , _configMiscShowSliceInObst :: Bool
  -- , _configMiscSkipMBedSlice :: Bool
  -- , _configMiscShowTicks :: Bool
 -- -- , _configMiscUserTicks :: ConfigUserTick
 -- -- , _configMiscShooter :: ConfigShooter
  -- , _configMiscShowLabel :: Bool
  -- , _configMiscShowFrameRate :: Bool
  -- , _configMiscFrameRateValue :: Int
  -- , _configMiscVectorSkip :: Bool


  -- , _configMiscShowCADAndGrid :: Int
  -- , _configMiscOutLinesMode :: (Int, Int)
  -- , _configMiscTitleSafe :: Int
  -- , _configMiscFontSize :: Int
  -- , _configMiscScaledFont :: [(Int, Double, Int)]
  -- , _configMiscZoom :: (Int, Double)
  -- , _configMiscRenderFileType :: Int
  -- , _configMiscRenderFileLabel :: Int
  -- , _configMiscRenderOption :: (Int, Int)
  -- , _configMiscShowGrid :: Int
  -- , _configMiscShowGridLoc :: Int
  -- , _configMiscCellCenterText :: Int
  -- , _configMiscPixelSkip :: Int
  -- , _configMiscProjection :: Int
  -- , _configMiscStereo :: Int
  -- , _configMiscUnitClasses :: [Int]
  -- , _configMiscMScale :: (Int, Int, Int)
 -- -- , _configMiscRenderClip ::
 -- -- , _configMiscClip ::
 -- -- , _configMiscXYZClip ::
 -- -- , _configMiscInputFile ::
 -- -- , _configMiscEyeX ::
 -- -- , _configMiscEyeY ::
 -- -- , _configMiscEyeZ ::
 -- -- , _configMiscEyeView ::
 -- -- , _configMiscLabelStartupView ::
 -- -- , _configMiscViewTimes ::
 -- -- , _configMiscTimeOffset ::
 -- -- , _configMiscShowHMSTimeLabel ::
 -- -- , _configMiscCullFaces ::
  -- } deriving Show

-- data ConfigRenderClip = ConfigRenderClip
  -- { _configRenderClipActive :: Bool
  -- , _configRenderClipLeft :: Int
  -- , _configRenderClipRight :: Int
  -- , _configRenderClipBottom :: Int
  -- , _configRenderClipTop ::Int
  -- } deriving Show

-- data ConfigZone = ConfigZone
  -- { _configZoneShowZoneFire :: Bool
  -- , _configZoneShowSZone :: Bool
  -- , _configZoneShowHZone :: Bool
  -- , _configZoneShowVZone :: Bool
  -- , _configZoneShowHazardColors :: Bool
  -- , _configZoneViewPoints :: [ViewPoint]
  -- } deriving Show

-- instance Default ConfigZone where
    -- def = ConfigZone
        -- { _configZoneShowZoneFire = True
        -- , _configZoneShowSZone = False
        -- , _configZoneShowHZone = False
        -- , _configZoneShowVZone = True
        -- , _configZoneShowHazardColors = False
        -- , _configZoneViewPoints = []
        -- }

-- instance FDSPrint ConfigZone where
    -- fdsPrint conf
        -- =   fdsPrint (_configZoneShowZoneFire conf)
        -- ++  fdsPrint (_configZoneShowSZone conf)
        -- ++  fdsPrint (_configZoneShowHZone conf)
        -- ++  fdsPrint (_configZoneShowVZone conf)
        -- ++  fdsPrint (_configZoneShowHazardColors conf)
        -- ++ "\n"
        -- ++  (unlines $ map fdsPrint (_configZoneViewPoints conf))

-- data Config3DSmokeInfo = Config3DSmokeInfo
  -- { _config3DSmokeInfoAdjustAlpha :: Int
  -- , _config3DSmokeInfoUseGPU :: Bool
  -- , _config3DSmokeInfoSmokeCull :: Bool
  -- , _config3DSmokeInfoSmokeSkip :: Int
  -- , _config3DSmokeInfoSmokeAlbedo :: Double
  -- , _config3DSmokeInfoSmokeRThick :: Double
  -- -- , _config3DSmokeInfoFireColor :: RGB DoubleInta -- this colour is rendered in ints for some reason
  -- , _config3DSmokeInfoFireDepth :: Double
 -- -- , _config3DSmokeInfoVolSmoke ::
  -- , _config3DSmokeInfoFireColorMap :: (Int, Int)
 -- -- , _config3DSmokeInfoShowExtremeData ::
 -- -- , _config3DSmokeInfoExtremeColors ::
 -- -- , _config3DSmokeInfoColorBarType ::
  -- } deriving Show

-- data ConfigTourInfo = ConfigTourInfo
  -- { _configTourInfoViewTourFromPath :: Bool
  -- , _configTourInfoViewAllTours :: Bool
  -- , _configTourInfoShowTourRoute :: Bool
  -- , _configTourInfoShowPathNodes :: Bool
  -- , _configTourInfoTourConstantVel :: Bool
 -- -- , _configTourInfoTourColors ::
  -- , _configTourInfoTourIndex :: Int
 -- -- , _configTourInfoDefaultTours :: [Tour]
  -- } deriving Show

-- instance Default IniConfig where
    -- def = IniConfig
            -- { _iniConfigFDSFilename =  error "iniConfigFDSFilename not set"
            -- , _iniConfigColourBarFlip = False
            -- , _iniConfigBarLevel = Nothing
            -- , _iniConfigSliceProperties = def
            -- , _iniConfigTimeBounds = defaultTimeBounds
            -- , _iniConfigUnits = def
            -- , _iniConfigLabelStartupView = Nothing
            -- , _iniConfigViewPoints = []
            -- , _iniConfigXYZClip = def
            -- , _iniConfigWindowSize = WindowSize 1024 768
            -- }

data SliceLineContours = SliceLineContours
    { _sliceLineContoursMin :: Double
    , _sliceLineContoursMax :: Double
    , _sliceLineContoursN :: Int
    } deriving Show

instance Default SliceLineContours where
    def = SliceLineContours
            { _sliceLineContoursMin = 0
            , _sliceLineContoursMax = 1
            , _sliceLineContoursN = 1
            }

data DataBounds = DataBounds
    { _dBoundMin :: Maybe Double
    , _dBoundMax :: Maybe Double
    } deriving Show

defaultTimeBounds = DataBounds
    { _dBoundMin = Nothing
    , _dBoundMax = Nothing
    }

noBounds = DataBounds
    { _dBoundMin = Nothing
    , _dBoundMax = Nothing
    }

data ClippingType = None | BlockagesAndData | Blockages | Data

clippingTypeToInt t = case t of
    None -> 0
    BlockagesAndData -> 1
    Blockages -> 2
    Data -> 3
-- data SmokeviewViewPoint = SmokeviewViewPoint-}
    -- { _viewPointEyeView :: Int  -- ^0 - general rotations, 1 - first person movement, 2 - level rotations
    -- , _viewPointRotationIndex :: Int
    -- , _viewPointViewID :: Int
    -- , _viewPointEyePos :: R3Coords -- ^Coordinates of viewing position (shouldn't matter for orthographic views)
    -- , _viewPointZoom :: Double
    -- , _viewPointZoomIndex :: Int
    -- , _viewPointViewAngle :: Double
    -- , _viewPointDirectionAngle :: Double
    -- , _viewPointElevationAngle :: Double
    -- , _viewPointProjectionType :: Int
    -- , _viewPointZAngle :: (Double, Double)
    -- , _viewPointTransformMatrix :: TransformMatrix  -- ^Viewing transformation matrix
    -- , _viewPointGlobalClippingFlag :: Int
    -- , _viewPointXClipping :: DataBounds
    -- , _viewPointYClipping :: DataBounds
    -- , _viewPointZClipping :: DataBounds
    -- , _viewPointName :: String
    -- { deriving Show
-- data ViewPoint = ViewPoint
    -- { _viewPointEyeView :: Int  -- ^0 - general rotations, 1 - first person movement, 2 - level rotations
    -- , _viewPointRotationIndex :: Int
    -- , _viewPointViewID :: Int
    -- , _viewPointEyePos :: R3Coords -- ^Coordinates of viewing position (shouldn't matter for orthographic views)
    -- , _viewPointZoom :: Double
    -- , _viewPointZoomIndex :: Int
    -- , _viewPointViewAngle :: Double
    -- , _viewPointDirectionAngle :: Double
    -- , _viewPointElevationAngle :: Double
    -- , _viewPointProjectionType :: Projection
    -- , _viewPointViewDir :: R3Coords
    -- , _viewPointZAngle :: (Double, Double)
    -- , _viewPointTransformMatrix :: TransformMatrix  -- ^Viewing transformation matrix
    -- , _viewPointGlobalClippingFlag :: Int
    -- , _viewPointXClipping :: DataBounds
    -- , _viewPointYClipping :: DataBounds
    -- , _viewPointZClipping :: DataBounds
    -- , _viewPointName :: String
    -- } deriving Show
-- -- TODO: figure out how to auto-generate appropriate views.
-- instance Default ViewPoint where
    -- def = ViewPoint
            -- { _viewPointEyeView = 0
            -- , _viewPointRotationIndex = 8
            -- , _viewPointViewID = 5
            -- , _viewPointEyePos = (0.654863, -2.945114, 0.115789)
            -- , _viewPointZoom = 1.0
            -- , _viewPointZoomIndex = 2
            -- , _viewPointViewAngle = 0.0
            -- , _viewPointDirectionAngle = 0.0
            -- , _viewPointElevationAngle = 0.0
            -- , _viewPointProjectionType = Orthographic
            -- , _viewPointViewDir =  (0.500000, 0.297368, 0.115789)
            -- , _viewPointZAngle = (0, 0)
            -- , _viewPointTransformMatrix = def
            -- , _viewPointGlobalClippingFlag = 2
            -- , _viewPointXClipping = noBounds
            -- , _viewPointYClipping = noBounds
            -- , _viewPointZClipping = noBounds
            -- , _viewPointName = "defaultView"
            -- }


data Viewpoint5 = Viewpoint5
    { _viewpoint5EyeView :: Int  -- ^0 - general rotations, 1 - first person movement, 2 - level rotations
    , _viewpoint5RotationIndex :: Int
    , _viewpoint5ViewID :: Int
    , _viewpoint5EyePos :: R3Coords -- ^Coordinates of viewing position (shouldn't matter for orthographic views)
    , _viewpoint5Zoom :: Double
    , _viewpoint5ZoomIndex :: Int
    , _viewpoint5ViewAngle :: Double
    , _viewpoint5DirectionAngle :: Double
    , _viewpoint5ElevationAngle :: Double
    , _viewpoint5ProjectionType :: Projection
    , _viewpoint5ViewDir :: R3Coords
    , _viewpoint5ZAngle :: (Double, Double)
    , _viewpoint5TransformMatrix :: TransformMatrix  -- ^Viewing transformation matrix
    , _viewpoint5GlobalClippingFlag :: Int
    , _viewpoint5XClipping :: DataBounds
    , _viewpoint5YClipping :: DataBounds
    , _viewpoint5ZClipping :: DataBounds
    , _viewpoint5Name :: String
    } deriving Show
-- TODO: figure out how to auto-generate appropriate views.
instance Default Viewpoint5 where
    def = Viewpoint5
        { _viewpoint5EyeView = 0
        , _viewpoint5RotationIndex = 8
        , _viewpoint5ViewID = 5
        , _viewpoint5EyePos = (0.654863, -2.945114, 0.115789)
        , _viewpoint5Zoom = 1.0
        , _viewpoint5ZoomIndex = 2
        , _viewpoint5ViewAngle = 0.0
        , _viewpoint5DirectionAngle = 0.0
        , _viewpoint5ElevationAngle = 0.0
        , _viewpoint5ProjectionType = Orthographic
        , _viewpoint5ViewDir =  (0.500000, 0.297368, 0.115789)
        , _viewpoint5ZAngle = (0, 0)
        , _viewpoint5TransformMatrix = def
        , _viewpoint5GlobalClippingFlag = 2
        , _viewpoint5XClipping = noBounds
        , _viewpoint5YClipping = noBounds
        , _viewpoint5ZClipping = noBounds
        , _viewpoint5Name = "defaultView"
        }
data Projection = Orthographic | Perspective deriving (Show)

newtype TransformMatrix = TransformMatrix
    ( (Double, Double, Double, Double)
    , (Double, Double, Double, Double)
    , (Double, Double, Double, Double)
    , (Double, Double, Double, Double)
    ) deriving (Show)

instance Default TransformMatrix where
    def = TransformMatrix
        ( (1.0, 0.0, 0.0, 0.0)
        , (0.0, 1.0, 0.0, 0.0)
        , (0.0, 0.0, 1.0, 0.0)
        , (0.0, 0.0, 0.0, 1.0)
        )

type R3Coords = (Double, Double, Double)

data Tour = Tour
    { tourName :: String
    , tourNKeyframes :: Int
    , tourGlobalTension :: Maybe Double
    , tourAvatarNum :: Int
    , tourKeyframes :: [TourKeyframe]
    } deriving Show

data TourKeyframe = TourKeyframe
    { tourKeyframeTime :: Double
    , tourKeyframeXPos :: Double
    , tourKeyframeYPos :: Double
    , tourKeyframeZPos :: Double
    , tourKeyframeViewDirection :: TourKeyframeViewDirection
    , tourKeyframeContinuity :: Double
    , tourKeyframeTension :: Double
    , tourKeyframeZoom :: Double
    , tourKeyframeLocalSpeedFlag :: Double
    } deriving Show

data TourKeyframeViewDirection = AzimElev
    { viewDirectionAzimuth :: Double
    , viewDirectionElevation :: Double
    , viewDirectionBias :: Double
    }
    | Cartesian
    { viewDirectionXView :: Double
    , viewDirectionYView :: Double
    , viewDirectionZView :: Double
    } deriving Show

data UnitsConfig = UnitsConfig
    { unitsConfigNum :: Int
    , unitsConfigVals :: [Int]
    } deriving Show

instance Default UnitsConfig where
    def = UnitsConfig
            { unitsConfigNum = 6
            , unitsConfigVals = [0, 0, 0, 0, 0, 1]
            }

data XYZClip = XYZClip
        { _xyzClipOnOff :: Int
        , _xyzClipXBounds :: DataBounds
        , _xyzClipYBounds :: DataBounds
        , _xyzClipZBounds :: DataBounds
        } deriving Show

instance Default XYZClip where
    def = XYZClip
            { _xyzClipOnOff = 0
            , _xyzClipXBounds = noBounds
            , _xyzClipYBounds = noBounds
            , _xyzClipZBounds = noBounds
            }

newtype SlicePropertiesDict = SlicePropertiesDict (M.Map String SliceProperties) deriving (Show)
dictToMap (SlicePropertiesDict m) = m
modifySliceKey k transform' (SlicePropertiesDict m)
    = SlicePropertiesDict $ M.alter transform k m
    where
        transform (Just x) = Just $ transform' x
        transform Nothing = Just $ transform' def

instance Default SlicePropertiesDict where
    def = SlicePropertiesDict $ M.fromList
        [ ("temp", temperatureDefaults)
        , ("VIS_Soot", sootVisibilityDefaults)
        , ("X_CO", coVolumeFractionDefaults)
        , ("rho_CO", coDensityDefaults)
        , ("vel", velDefaults)
        , ("U-VEL", uVelDefaults)
        , ("V-VEL", vVelDefaults)
        , ("W-VEL", wVelDefaults)
        ]
data SliceProperties = SliceProperties
    { _sliceName :: String
    , _sliceAbbrev :: String
    , _sliceColourBarFlip :: Bool
    , _sliceColourBounds :: DataBounds
    , _sliceClipBounds :: DataBounds
    , _sliceContourValue :: Maybe Double
    , _sliceLineContours :: SliceLineContours
    } deriving Show

instance Default SliceProperties where
    def = SliceProperties
        { _sliceName = "unnamed"
        , _sliceAbbrev = error "_sliceAbbrev undefined"
        , _sliceColourBarFlip = False
        , _sliceColourBounds = noBounds
        , _sliceClipBounds = noBounds
        , _sliceContourValue = Nothing
        , _sliceLineContours = def
        }

defaultSliceProperties =
    [ temperatureDefaults
    , sootVisibilityDefaults
    , coVolumeFractionDefaults
    , coDensityDefaults
    , velDefaults
    , uVelDefaults
    , vVelDefaults
    , wVelDefaults
    ]
temperatureDefaults = SliceProperties
    { _sliceName = "TEMPERATURE"
    , _sliceAbbrev = "temp"
    , _sliceColourBarFlip = False
    , _sliceColourBounds = DataBounds
        { _dBoundMin = Just 20
        , _dBoundMax = Just 100
        }
    , _sliceClipBounds = noBounds
    , _sliceContourValue = Just 60
    , _sliceLineContours = def
    }

sootVisibilityDefaults = SliceProperties
    { _sliceName = "SOOT VISIBILITY"
    , _sliceAbbrev = "VIS_Soot"
    , _sliceColourBarFlip = True
    , _sliceColourBounds = DataBounds
        { _dBoundMin = Just 0
        , _dBoundMax = Just 20
        }
    , _sliceClipBounds = noBounds
    , _sliceContourValue = Just 10
    , _sliceLineContours = def
    }

coVolumeFractionDefaults = SliceProperties
    { _sliceName = "CARBON MONOXIDE VOLUME FRACTION"
    , _sliceAbbrev = "X_CO"
    , _sliceColourBarFlip = False
    , _sliceColourBounds = DataBounds
        { _dBoundMin = Just 0
        , _dBoundMax = Just 0.002400
        }
    , _sliceClipBounds = noBounds
    , _sliceContourValue = Just 0.001400
    , _sliceLineContours = def
    }

coDensityDefaults = SliceProperties
    { _sliceName = "CARBON MONOXIDE DENSITY"
    , _sliceAbbrev = "rho_CO"
    , _sliceColourBarFlip = False
    , _sliceColourBounds = DataBounds
        { _dBoundMin = Just 0
        , _dBoundMax = Just 0.001800
        }
    , _sliceClipBounds = noBounds
    , _sliceContourValue = Just 0.000900
    , _sliceLineContours = def
    }

velDefaults = SliceProperties
    { _sliceName = "VELOCITY"
    , _sliceAbbrev = "vel"
    , _sliceColourBarFlip = False
    , _sliceColourBounds = noBounds
    , _sliceClipBounds = noBounds
    , _sliceContourValue = Just 0.000900
    , _sliceLineContours = def
    }

uVelDefaults = SliceProperties
    { _sliceName = "U-VELOCITY"
    , _sliceAbbrev = "U-VEL"
    , _sliceColourBarFlip = False
    , _sliceColourBounds = noBounds
    , _sliceClipBounds = noBounds
    , _sliceContourValue = Just 0.000900
    , _sliceLineContours = def
    }

vVelDefaults = SliceProperties
    { _sliceName = "V-VELOCITY"
    , _sliceAbbrev = "V-VEL"
    , _sliceColourBarFlip = False
    , _sliceColourBounds = noBounds
    , _sliceClipBounds = noBounds
    , _sliceContourValue = Nothing
    , _sliceLineContours = def
    }

wVelDefaults = SliceProperties
    { _sliceName = "W-VELOCITY"
    , _sliceAbbrev = "W-VEL"
    , _sliceColourBarFlip = False
    , _sliceColourBounds = noBounds
    , _sliceClipBounds = noBounds
    , _sliceContourValue = Nothing
    , _sliceLineContours = def
    }

class FDSPrint a where
    fdsPrint :: a -> String

data Axis = X | Y | Z deriving Show
data WindowSize = WindowSize Int Int deriving Show

makeLenses ''IniConfig
-- makeLenses ''IniConfigLua
-- makeLenses ''ConfigZone
makeLenses ''DataBounds
makeLenses ''XYZClip
-- makeLenses ''ViewPoint
makeLenses ''Viewpoint5
makeLenses ''SliceProperties
makeLenses ''SliceLineContours



-- instance FDSPrint ViewPoint where
    -- fdsPrint = printViewPoint

instance FDSPrint WindowSize where
    fdsPrint (WindowSize width height) = "WINDOWWIDTH\n"
        ++ (addIndent " " (fdsPrint width)) ++ "\n"
        ++ "WINDOWHEIGHT\n"
        ++ (addIndent " " (fdsPrint height)) ++ "\n"

instance FDSPrint XYZClip where
    fdsPrint xyzClip = "XYZCLIP\n " ++ show (xyzClip ^. xyzClipOnOff) ++ "\n" ++ addIndent " " (
                            printBounds (xyzClip ^. xyzClipXBounds) ++ "\n"
                            ++ printBounds (xyzClip ^. xyzClipYBounds) ++ "\n"
                            ++ printBounds (xyzClip ^. xyzClipZBounds) ++ "\n"
                            )

instance FDSPrint Double where
    fdsPrint = printf "%4.6f"

instance FDSPrint (RGB Double) where
    fdsPrint (RGB r g b)= printf "%4.6f %4.6f %4.6f" r g b

instance FDSPrint Int where
    fdsPrint = show

instance (FDSPrint a, FDSPrint b) => FDSPrint (a,b) where
    fdsPrint (a,b) = fdsPrint a ++ fdsPrint b

instance (FDSPrint a, FDSPrint b, FDSPrint c) => FDSPrint (a,b,c) where
    fdsPrint (a,b,c) = fdsPrint a ++ fdsPrint b ++ fdsPrint c


printSlicePropertiesVSlice :: SliceProperties -> String
printSlicePropertiesVSlice sliceProps = "V_SLICE\n " ++ printBounds (sliceProps ^. sliceColourBounds) ++ " " ++  (sliceProps ^. sliceAbbrev) ++ " : " ++ printlineContourProps (sliceProps ^. sliceLineContours) ++ "\n"

printSlicePropertiesCSlice :: SliceProperties -> String
printSlicePropertiesCSlice sliceProps = "C_SLICE\n " ++ printBounds (sliceProps ^. sliceClipBounds) ++ " " ++ (sliceProps ^. sliceAbbrev) ++ "\n"

printBounds dBound = printBound (dBound ^. dBoundMin ) ++ " " ++ printBound (dBound ^. dBoundMax)

printBound :: Maybe Double -> String
printBound bndVal = case bndVal of
                Just val -> "1 " ++ printIniVal val
                Nothing -> "0 0.00000"
printlineContourProps sliceContProps = printIniVal (sliceContProps ^. sliceLineContoursMin) ++ " " ++ printIniVal (sliceContProps ^. sliceLineContoursMax) ++ " " ++ show (sliceContProps ^. sliceLineContoursN)

-- printViewPoint :: ViewPoint -> String
-- printViewPoint viewPoint = "VIEWPOINT5\n" ++ ( addIndent " " (
    -- show (viewPoint ^. viewPointEyeView) ++ " " ++ show (viewPoint ^.viewPointRotationIndex) ++ " " ++ show (viewPoint ^. viewPointViewID) ++ "\n"
    -- ++ printR3Coords (viewPoint ^. viewPointEyePos) ++ " " ++ printIniVal (viewPoint ^. viewPointZoom) ++ " " ++ show ( viewPoint ^. viewPointZoomIndex) ++ "\n"
    -- ++ printIniVal (viewPoint ^. viewPointViewAngle) ++ " " ++ printIniVal (viewPoint ^. viewPointDirectionAngle) ++ " " ++ printIniVal (viewPoint ^. viewPointElevationAngle) ++ " " ++ printIniVal (viewPoint ^. viewPointProjectionType) ++ "\n"
    -- ++ printR3Coords (viewPoint ^.viewPointViewDir) ++ "\n"
    -- ++ printIniVal (fst (viewPoint ^. viewPointZAngle)) ++ " " ++ printIniVal (snd (viewPoint ^. viewPointZAngle)) ++ "\n"
    -- ++ printTransformMatrix (viewPoint ^. viewPointTransformMatrix )
    -- ++ printViewPointClipping viewPoint
    -- ++ viewPoint ^. viewPointName
    -- ))

printViewPointClipping :: Viewpoint5 -> String
printViewPointClipping viewPoint =
    show (viewPoint ^. viewpoint5GlobalClippingFlag) ++ " " ++ xMinBool ++ " " ++ yMinBool ++ " " ++ zMinBool ++ " " ++ xMaxBool ++ " " ++ yMaxBool ++ " " ++ zMaxBool ++ "\n"
    ++ xMin ++ " " ++ yMin ++ " " ++ zMin ++ " " ++ xMax ++ " " ++ yMax ++ " " ++ zMax ++ "\n"
    where

        (xMinBool, xMin) = case viewPoint^.viewpoint5XClipping^.dBoundMin of
                Nothing -> ("0", "0.0")
                Just val -> ("1", fdsPrint val)
        (yMinBool, yMin) = case viewPoint^.viewpoint5YClipping^.dBoundMin of
                Nothing -> ("0", "0.0")
                Just val -> ("1", fdsPrint val)
        (zMinBool, zMin) = case viewPoint^.viewpoint5ZClipping^.dBoundMin of
                Nothing -> ("0", "0.0")
                Just val -> ("1", fdsPrint val)
        (xMaxBool, xMax) = case viewPoint^.viewpoint5XClipping^.dBoundMax of
                Nothing -> ("0", "0.0")
                Just val -> ("1", fdsPrint val)
        (yMaxBool, yMax) = case viewPoint^.viewpoint5YClipping^.dBoundMax of
                Nothing -> ("0", "0.0")
                Just val -> ("1", fdsPrint val)
        (zMaxBool, zMax) = case viewPoint^.viewpoint5ZClipping^.dBoundMax of
                Nothing -> ("0", "0.0")
                Just val -> ("1", fdsPrint val)

printR3Coords :: R3Coords -> String
printR3Coords (x,y,z) = printIniVal x ++ " " ++ printIniVal y ++ " " ++ printIniVal z

printTransformMatrix :: TransformMatrix -> String
printTransformMatrix (TransformMatrix (l1, l2, l3, l4)) = printTransformMatrixLine l1 ++ printTransformMatrixLine l2 ++ printTransformMatrixLine l3 ++ printTransformMatrixLine l4
    where
        printTransformMatrixLine (a, b, c, d) = printIniVal a ++ " " ++ printIniVal b ++ " " ++ printIniVal c ++ " " ++ printIniVal d ++ "\n"

class IniVal a where
    printIniVal :: a -> String
instance IniVal Double where
    printIniVal = printf "%4.5f"
instance IniVal Projection where
    printIniVal Perspective = "0"
    printIniVal Orthographic = "1"



instance FDSPrint Viewpoint5 where
    fdsPrint viewPoint = ( addIndent "  " (
        show (_viewpoint5EyeView viewPoint) ++ " " ++ show (_viewpoint5RotationIndex viewPoint) ++ " " ++ show (_viewpoint5ViewID viewPoint) ++ "\n"
        ++ printR3Coords (_viewpoint5EyePos viewPoint) ++ " " ++ printIniVal (_viewpoint5Zoom viewPoint) ++ " " ++ show (_viewpoint5ZoomIndex viewPoint) ++ "\n"
        ++ printIniVal (_viewpoint5ViewAngle viewPoint) ++ " " ++ printIniVal (_viewpoint5DirectionAngle viewPoint) ++ " " ++ printIniVal (_viewpoint5ElevationAngle viewPoint) ++ " " ++ printIniVal (_viewpoint5ProjectionType viewPoint) ++ "\n"
        ++ printR3Coords (_viewpoint5ViewDir viewPoint) ++ "\n"
        ++ printIniVal (fst (_viewpoint5ZAngle viewPoint)) ++ " " ++ printIniVal (snd (_viewpoint5ZAngle viewPoint)) ++ "\n"
        ++ printTransformMatrix (_viewpoint5TransformMatrix viewPoint)
        ++ printViewPointClipping viewPoint
        ++ _viewpoint5Name viewPoint
        ))
