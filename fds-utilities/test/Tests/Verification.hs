{-# LANGUAGE OverloadedStrings #-}
module Tests.Verification where

import qualified Data.Map as M
import qualified Data.Array as A
import Data.Default

import FDSUtilities.Verification.Tests
import Text.Namelist
import Test.HUnit
import FDSUtilities.Types
import FDSUtilities.FDSFileFunctions

import Text.Parsec (SourcePos)
import Text.Parsec.Pos (initialPos)

verificationTests = TestList
    [ testFlowDevc1
    , testFlowDevc2
    , testFlowDevc3
    , testFlowDevc4
    , fdsData1
    ]

testFlowDevc1 = TestLabel "Unmetered flow device" $
    let ventNml = Namelist "VENT" "" (M.fromList
                [("XB",ParArray (M.fromList
                    [ ((1,1),ParDouble 110)
                    , ((2,1),ParDouble 200)
                    , ((3,1),ParDouble (-225))
                    , ((4,1),ParDouble (-175))
                    , ((5,1),ParDouble 0)
                    , ((6,1),ParDouble 50)
                    ]))
                ])
                (initialPos "Test Input")
        fdsData = NamelistFile ""
            [ ventNml
            , Namelist "SURF" "" (M.fromList
                [ ("ID", ParString "Ex")
                , ("VOLUME_FLOW", ParDouble 5)
                ])
                (initialPos "Test Input")
            ]
    in hasFlowDevc fdsData ventNml ~?= False

testFlowDevc2 = TestLabel "Metered flow device" $
    let ventNml = Namelist "VENT" "" (M.fromList
                [("XB",ParArray (M.fromList
                    [ ((1,1),ParDouble 110)
                    , ((2,1),ParDouble 200)
                    , ((3,1),ParDouble (-225))
                    , ((4,1),ParDouble (-175))
                    , ((5,1),ParDouble 0)
                    , ((6,1),ParDouble 50)
                    ]))
                ])
                (initialPos "Test Input")
        flowDevcNml = Namelist "DEVC" "" (M.fromList
                [("XB",ParArray (M.fromList
                    [ ((1,1),ParDouble 110)
                    , ((2,1),ParDouble 200)
                    , ((3,1),ParDouble (-225))
                    , ((4,1),ParDouble (-175))
                    , ((5,1),ParDouble 0)
                    , ((6,1),ParDouble 50)
                    ]))
                , ("QUANTITY", ParString "VOLUME FLOW")
                ])
                (initialPos "Test Input")
        fdsData = NamelistFile ""
            [ ventNml
            , flowDevcNml
            , Namelist "SURF" "" (M.fromList
                [ ("ID", ParString "Ex")
                , ("VOLUME_FLOW", ParDouble 5)
                ])
                (initialPos "Test Input")
            ]
    in hasFlowDevc fdsData ventNml ~?= True

testFlowDevc3 = TestLabel "Incorrectly metered flow device (small device)" $
    let ventNml = Namelist "VENT" "" (M.fromList
                [("XB",ParArray (M.fromList
                    [ ((1,1),ParDouble 110)
                    , ((2,1),ParDouble 200)
                    , ((3,1),ParDouble (-225))
                    , ((4,1),ParDouble (-175))
                    , ((5,1),ParDouble 0)
                    , ((6,1),ParDouble 50)
                    ]))
                ])
                (initialPos "Test Input")
        flowDevcNml = Namelist "DEVC" "" (M.fromList
                [("XB",ParArray (M.fromList
                    [ ((1,1),ParDouble 110)
                    , ((2,1),ParDouble 150)
                    , ((3,1),ParDouble (-225))
                    , ((4,1),ParDouble (-175))
                    , ((5,1),ParDouble 0)
                    , ((6,1),ParDouble 50)
                    ]))
                , ("QUANTITY", ParString "VOLUME FLOW")
                ])
                (initialPos "Test Input")
        fdsData = NamelistFile ""
            [ ventNml
            , flowDevcNml
            , Namelist "SURF" "" (M.fromList
                [ ("ID", ParString "Ex")
                , ("VOLUME_FLOW", ParDouble 5)
                ])
                (initialPos "Test Input")
            ]
    in hasFlowDevc fdsData ventNml ~?= False

testFlowDevc4 = TestLabel "Incorrectly metered flow device (wrong QUANTITY)" $
    let ventNml = Namelist "VENT" "" (M.fromList
                [("XB",ParArray (M.fromList
                    [ ((1,1),ParDouble 110)
                    , ((2,1),ParDouble 200)
                    , ((3,1),ParDouble (-225))
                    , ((4,1),ParDouble (-175))
                    , ((5,1),ParDouble 0)
                    , ((6,1),ParDouble 50)
                    ]))
                ])
                (initialPos "Test Input")
        flowDevcNml = Namelist "DEVC" "" (M.fromList
                [("XB",ParArray (M.fromList
                    [ ((1,1),ParDouble 110)
                    , ((2,1),ParDouble 200)
                    , ((3,1),ParDouble (-225))
                    , ((4,1),ParDouble (-175))
                    , ((5,1),ParDouble 0)
                    , ((6,1),ParDouble 50)
                    ]))
                , ("QUANTITY", ParString "TEMPERATURE")
                ])
                (initialPos "Test Input")
        fdsData = NamelistFile ""
            [ ventNml
            , flowDevcNml
            , Namelist "SURF" "" (M.fromList
                [ ("ID", ParString "Ex")
                , ("VOLUME_FLOW", ParDouble 5)
                ])
                (initialPos "Test Input")
            ]
    in hasFlowDevc fdsData ventNml ~?= False

-- |Should parse an example NamelistFile into an FDSFile with a single
-- obstruction.
fdsData1 = TestLabel "Single Obst test" $
    let ventNml = Namelist "OBST" "" (M.fromList
                [("XB",ParArray (M.fromList
                    [ ((1,1),ParDouble 110)
                    , ((2,1),ParDouble 200)
                    , ((3,1),ParDouble (-225))
                    , ((4,1),ParDouble (-175))
                    , ((5,1),ParDouble 0)
                    , ((6,1),ParDouble 50)
                    ]))
                ])
                (initialPos "Test Input")
        fdsData = NamelistFile ""
            [ ventNml
            , Namelist "SURF" "" (M.fromList
                [ ("ID", ParString "Ex")
                , ("VOLUME_FLOW", ParDouble 5)
                ])
                (initialPos "Test Input")
            ]
        expectedObst = Obst
            { obst_ALLOW_VENT = True
            , obst_BNDF_FACE = (False, False, False, False, False, False)
            , obst_BNDF_OBST = True
            , obst_BULK_DENSITY = Nothing
            , obst_COLOR = Nothing
            , obst_CTRL_ID = Nothing
            , obst_DEVC_ID = Nothing
            , obst_EVACUATION = False
            , obst_FYI = Nothing
            , obst_HT3D = False
            , obst_ID = Nothing
            , obst_MATL_ID = Nothing
            , obst_MESH_ID = Nothing
            , obst_MULT_ID = Nothing
            -- , obst_NOTERRAIN :: Bool
            , obst_OUTLINE = False
            , obst_OVERLAY = True
            , obst_PERMIT_HOLE = True
            , obst_PROP_ID = Nothing
            , obst_REMOVABLE = True
            , obst_RGB = Nothing
            , obst_SURF_ID = Nothing
            , obst_SURF_ID6 = Nothing
            , obst_SURF_IDS = Nothing
            , obst_TEXTURE_ORIGIN = XYZ 0 0 0
            , obst_THICKEN = False
            , obst_TRANSPARENCY = 1
            , obst_XB = XB 110 200 (-225) (-175) 0 50
            }
        expectedFDSFile = def
            { fdsFile_Obsts = [expectedObst]
            }
        decodedNamelistFile = (decodeNamelistFile fdsData) {
            fdsFile_unknownNamelists = []
        }
    in decodedNamelistFile ~?= expectedFDSFile
