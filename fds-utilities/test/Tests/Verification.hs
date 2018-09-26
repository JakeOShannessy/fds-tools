{-# LANGUAGE OverloadedStrings #-}
module Tests.Verification where

import qualified Data.Map as M
import qualified Data.Array as A

import FDSUtilities.Verification.Tests
import Text.Namelist
import Test.HUnit

verificationTests = TestList
    [ testFlowDevc1
    , testFlowDevc2
    , testFlowDevc3
    , testFlowDevc4
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
        fdsData = NamelistFile ""
            [ ventNml
            , Namelist "SURF" "" (M.fromList
                [ ("ID", ParString "Ex")
                , ("VOLUME_FLOW", ParDouble 5)
                ])
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
        fdsData = NamelistFile ""
            [ ventNml
            , flowDevcNml
            , Namelist "SURF" "" (M.fromList
                [ ("ID", ParString "Ex")
                , ("VOLUME_FLOW", ParDouble 5)
                ])
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
        fdsData = NamelistFile ""
            [ ventNml
            , flowDevcNml
            , Namelist "SURF" "" (M.fromList
                [ ("ID", ParString "Ex")
                , ("VOLUME_FLOW", ParDouble 5)
                ])
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
        fdsData = NamelistFile ""
            [ ventNml
            , flowDevcNml
            , Namelist "SURF" "" (M.fromList
                [ ("ID", ParString "Ex")
                , ("VOLUME_FLOW", ParDouble 5)
                ])
            ]
    in hasFlowDevc fdsData ventNml ~?= False