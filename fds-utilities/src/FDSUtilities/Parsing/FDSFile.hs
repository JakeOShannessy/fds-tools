{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.Parsing.FDSFile
    ( parseFDSFile
    -- , parseFDSFileMeshes
    -- nmlFile2FDSFile
    -- , fdsFile2NmlFile
    -- , getPlot3d
    -- , getNml
    -- , findParam
    -- , getAllNml
    -- -- , fdsParser
    ) where

import Text.FDSInputParser
import Data.Text.IO as T

parseFDSFile filepath = parseFDSInputFile filepath
-- parseFDSFileMeshes filepath = do
--     raw <- readNml filepath
--     case raw of
--         Left e -> error $ show e
--         Right r -> do
--             T.putStrLn $ pprint r
--             return $ Right r

-- parseFDSFileNative filepath = convertToFDS $ readNml filepath



-- convertToFDS :: NamelistFile -> FDSFile

-- nmlFile2FDSFile :: NamelistFile -> FDSCase
-- nmlFile2FDSFile (NamelistFile namelists) = map nml2fds namelists

-- nml2fds (Namelist name comments parameters) =

-- data FDSNamelist = FDSNamelist String String (Map String a)


-- nmlFile2FDSFile :: NamelistFile -> FDSCase
-- nmlFile2FDSFile (NamelistFile nmlList) =
    -- let currCase = FDSCase
                    -- (Just $ headIn $ fromJust $ findSingleNml "HEAD" nmlList)
                    -- (Just $ timeIn $ fromJust $ findSingleNml "TIME" nmlList)
                    -- (Just $ dumpIn $ fromJust $ findSingleNml "DUMP" nmlList)
                    -- (Just $ miscIn $ fromJust $ findSingleNml "MISC" nmlList)
                    -- (map meshIn $ findMultiNml "MESH" nmlList)
                    -- (map reacIn $ findMultiNml "REAC" nmlList)
                    -- (map devcIn $ findMultiNml "DEVC" nmlList)
                    -- (map matlIn $ findMultiNml "MATL" nmlList)
                    -- (map (surfIn (getMatls currCase)) $ findMultiNml "SURF" nmlList)
                    -- (map (obstIn (getSurfs currCase)) $ findMultiNml "OBST" nmlList)
                    -- (map holeIn $ findMultiNml "HOLE" nmlList)
                    -- (map ventIn $ findMultiNml "VENT" nmlList)
                    -- (map bndfIn $ findMultiNml "BNDF" nmlList)
                    -- (map isofIn $ findMultiNml "ISOF" nmlList)
                    -- (map slcfIn $ findMultiNml "SLCF" nmlList)
    -- in currCase

-- getMatls (FDSCase head time dump misc mesh reac devc matl surf obst hole vent bndf isof slcf) = matl
-- getSurfs (FDSCase head time dump misc mesh reac devc matl surf obst hole vent bndf isof slcf) = surf
-- getPlot3d (FDSCase head time dump misc mesh reac devc matl surf obst hole vent bndf isof slcf) = plot3d
    -- where
        -- plot3d = (\(Dump _ pp _ _) -> pp) (fromJust dump)

-- fdsFile2NmlFile :: FDSCase -> NamelistFile
-- fdsFile2NmlFile (FDSCase head time dump misc meshS reacS devcS matlS surfS obstS holeS ventS bndfS isofS slcfS) = NamelistFile $
    -- [ (headOut $ fromJust head)
    -- , (timeOut $ fromJust time)
    -- , (dumpOut $ fromJust dump)
    -- , (miscOut $ fromJust misc)
    -- ]
    -- ++ (map meshOut meshS)
    -- ++ (map reacOut reacS)
    -- ++ (map devcOut devcS)
    -- ++ (map matlOut matlS)
    -- ++ (map surfOut surfS)
    -- ++ (map obstOut obstS)
    -- ++ (map holeOut holeS)
    -- ++ (map ventOut ventS)
    -- ++ (map bndfOut bndfS)
    -- ++ (map isofOut isofS)
    -- ++ (map slcfOut slcfS)
    -- ++ [tailOut]

-- findSingleNml :: String -> [Namelist] -> Maybe Namelist
-- findSingleNml fdsName nmlList = find (\(Namelist name _ _) -> name == fdsName) nmlList

-- findMultiNml :: String -> [Namelist] -> [Namelist]
-- findMultiNml fdsName nmlList = filter (\(Namelist name _ _) -> name == fdsName) nmlList

-- tailOut :: Namelist
-- tailOut = Namelist "TAIL" "" []

-- dumpIn :: Namelist -> Dump
-- dumpIn (Namelist _ _ parameters) = Dump
    -- (findParam parameters ("DT_RESTART", False, FDSNone))
    -- (findParam parameters ("PLOT3D_QUANTITY", False, FDSNone))
    -- (findParam parameters ("WRITE_XYZ", False, FDSNone))
    -- (findParam parameters ("RENDER_FILE", False, FDSNone))
-- dumpOut :: Dump -> Namelist
-- dumpOut (Dump dt_restart plot3d_quantity write_xyz render_file) = Namelist "DUMP" "" $
    -- exParameter "DT_RESTART"     dt_restart ++
    -- exParameter "PLOT3D_QUANTITY"     plot3d_quantity ++
    -- exParameter "WRITE_XYZ"     write_xyz ++
    -- exParameter "RENDER_FILE"     render_file

-- miscIn :: Namelist -> Misc
-- miscIn (Namelist _ _ parameters) = Misc
        -- (findParam parameters ("SURF_DEFAULT", True, FDSNone))
        -- (findParam parameters ("HUMIDITY", True, FDSNone))
        -- (findParam parameters ("TMPA", True, FDSNone))
-- miscOut :: Misc -> Namelist
-- miscOut  (Misc surf_default humidity tmpa) =
    -- Namelist "MISC" "" $
        -- exParameter "SURF_DEFAULT"     surf_default ++
        -- exParameter "HUMIDITY"     humidity ++
        -- exParameter "TMPA"     tmpa

-- bndfIn :: Namelist -> Bndf
-- bndfIn (Namelist _ _ parameters) = Bndf
    -- (findParam parameters ("CELL_CENTERED", False, FDSValue False))
    -- (findParam parameters ("FYI", False, FDSNone))
    -- (findParam parameters ("PART_ID", False, FDSNone))
    -- (findParam parameters ("PROP_ID", False, FDSNone))
    -- (findParam parameters ("RECOUNT_DRIP", False, FDSValue False))
    -- (findParam parameters ("QUANTITY", False, FDSNone))
    -- (findParam parameters ("SPEC_ID", False, FDSNone))
-- bndfOut :: Bndf -> Namelist
-- bndfOut  (Bndf cell_centered fyi part_id prop_id recount_drip quantity spec_id) =
    -- Namelist "BNDF" "" $
        -- exParameter "CELL_CENTERED"     cell_centered ++
        -- exParameter "FYI"               fyi ++
        -- exParameter "PART_ID"           part_id ++
        -- exParameter "PROP_ID"           prop_id ++
        -- exParameter "RECOUNT_DRIP"      recount_drip ++
        -- exParameter "QUANTITY"          quantity ++
        -- exParameter "SPEC_ID"           spec_id

-- slcfIn :: Namelist -> Slcf
-- slcfIn (Namelist _ _ parameters) = Slcf
    -- (findParam parameters ("QUANTITY", False, FDSNone))
    -- (findParam parameters ("VECTOR", False, FDSValue False))
    -- (FDSValue plane)
    -- offset
    -- where
        -- (plane, offset)
            -- | paramExists parameters "PBX" = (X, (findParam parameters ("PBX", False, FDSNone)))
            -- | paramExists parameters "PBY" = (Y, (findParam parameters ("PBY", False, FDSNone)))
            -- | paramExists parameters "PBZ" = (Z, (findParam parameters ("PBZ", False, FDSNone)))
    -- --TODO: Possibly check that multiple planes aren't specified
-- slcfOut :: Slcf -> Namelist
-- slcfOut (Slcf quantity vector plane offset) = Namelist "SLCF" "" $
    -- exParameter "QUANTITY"      quantity ++
    -- exParameter "VECTORY"       vector ++
    -- posParameter
    -- where
        -- posParameter = case (\(FDSValue x) -> x) plane of
            -- X -> exParameter "PBX"      offset
            -- Y -> exParameter "PBY"      offset
            -- Z -> exParameter "PBZ"      offset

-- isofIn :: Namelist -> Isof
-- isofIn (Namelist _ _ parameters) = Isof
        -- (findParam parameters ("QUANTITY", True, FDSNone))
        -- (findParam parameters ("VALUE", True, FDSNone))
-- isofOut :: Isof -> Namelist
-- isofOut  (Isof quantity value) =
    -- Namelist "ISOF" "" $
        -- exParameter "QUANTITY"     quantity ++
        -- exParameter "VALUE"     value

-- holeIn :: Namelist -> Hole
-- holeIn (Namelist _ _ parameters) = Hole
        -- (findParam parameters ("XB", True, FDSNone))
-- holeOut :: Hole -> Namelist
-- holeOut  (Hole xb) =
    -- Namelist "HOLE" "" $
        -- exParameter "XB"     xb

-- timeIn :: Namelist -> Time
-- timeIn (Namelist _ _ parameters) = Time
        -- (findParam parameters ("DT", False, FDSNone))
        -- (findParam parameters ("T_BEGIN", False, FDSNone))
        -- (findParam parameters ("T_END", False, FDSNone))
        -- (findParam parameters ("SYNCHRONIZE", False, FDSValue False))
-- timeOut :: Time -> Namelist
-- timeOut  (Time dt t_begin t_end synchronize) =
    -- Namelist "TIME" "" $ --TODO: add comment support
        -- exParameter "DT"            dt ++
        -- exParameter "T_BEGIN"       t_begin ++
        -- exParameter "T_END"         t_end ++
        -- exParameter "SYNCHRONIZE"   synchronize

-- headIn :: Namelist -> Head
-- headIn (Namelist _ _ parameters) = Head
        -- (findParam parameters ("CHID",  False, FDSNone))
        -- (findParam parameters ("FYI",   False, FDSNone))
        -- (findParam parameters ("TITLE", False, FDSNone))
-- headOut  (Head chid fyi title) =
    -- Namelist "HEAD" "" $
        -- exParameter "CHID"  chid ++
        -- exParameter "FYI"   fyi ++
        -- exParameter "TITLE" title

-- reacIn :: Namelist -> Reac
-- reacIn (Namelist _ _ parameters) = Reac
    -- (findParam parameters ("ID", False, FDSNone))
    -- (findParam parameters ("FYI", False, FDSNone))
    -- (findParam parameters ("SOOT_YIELD", False, FDSValue 0.01))
    -- (findParam parameters ("N", False, FDSValue 0))
    -- (findParam parameters ("C", False, FDSValue 3))
    -- (findParam parameters ("H", False, FDSValue 8))
    -- (findParam parameters ("O", False, FDSValue 0))
    -- (findParam parameters ("OTHER", False, FDSValue 0))
    -- (findParam parameters ("MW_OTHER", False, FDSValue 28))
    -- (findParam parameters ("Y_CO", False, FDSValue 0))
    -- (findParam parameters ("Y_H2", False, FDSValue 0))
    -- (findParam parameters ("HEAT_OF_COMBUSTION", False, FDSNone))
    -- (findParam parameters ("EPUMO2", False, FDSValue 13100))
    -- (findParam parameters ("SOOT_H_FRACTION", False, FDSValue 0.1))
-- reacOut (Reac id fyi soot_yield n c h o other mw_other y_co y_h2 heat_of_combustion epumo2 soot_h_fraction) =
    -- Namelist "REAC" "" $
        -- exParameter "ID" id ++
        -- exParameter "FYI" fyi ++
        -- exParameter "SOOT_YIELD" soot_yield ++
        -- exParameter "N" n ++
        -- exParameter "C" c ++
        -- exParameter "H" h ++
        -- exParameter "O" o ++
        -- exParameter "OTHER" other ++
        -- exParameter "MW_OTHER" mw_other ++
        -- exParameter "Y_CO" y_co ++
        -- exParameter "Y_H2" y_h2 ++
        -- exParameter "HEAT_OF_COMBUSTION" heat_of_combustion ++
        -- exParameter "EPUMO2" epumo2 ++
        -- exParameter "SOOT_H_FRACTION" soot_h_fraction

-- devcIn :: Namelist -> Devc
-- devcIn (Namelist _ _ parameters) = Devc
    -- (findParam parameters ("CTRL_ID", False, FDSNone))
    -- (findParam parameters ("FYI", False, FDSNone))
    -- (findParam parameters ("ID", False, FDSNone))
    -- (findParam parameters ("ORIENTATION", False, FDSNone))
    -- (findParam parameters ("PROP_ID", False, FDSNone))
    -- (findParam parameters ("QUANTITY", False, FDSNone))
    -- (findParam parameters ("ROTATION", False, FDSNone))
    -- (findParam parameters ("XB", False, FDSNone))
    -- (findParam parameters ("XYZ", False, FDSNone))
-- devcOut (Devc ctrl_id fyi id orientation prop_id quantity rotation xb xyz) =
    -- Namelist "DEVC" "" $
        -- exParameter "CTRL_ID"       ctrl_id ++
        -- exParameter "FYI"           fyi ++
        -- exParameter "ID"            id ++
        -- exParameter "ORIENTATION"   orientation ++
        -- exParameter "PROP_ID"       prop_id ++
        -- exParameter "QUANTITY"      quantity ++
        -- exParameter "ROTATION"      rotation ++
        -- exParameter "XB"            xb ++
        -- exParameter "XYZ"           xyz

-- meshIn :: Namelist -> Mesh
-- meshIn (Namelist _ _ parameters) = Mesh
    -- (findParam parameters ("ID", False, FDSNone))
    -- (findParam parameters ("XB", False, FDSNone))
    -- (findParam parameters ("IJK", False, FDSNone))
-- meshOut (Mesh id xb ijk) =
    -- Namelist "MESH" "" $
        -- exParameter "ID"       id ++
        -- exParameter "XB"       xb ++
        -- exParameter "IJK"      ijk

-- matlIn :: Namelist -> Matl
-- matlIn (Namelist _ _ parameters) = Matl
    -- (findParam parameters ("ID", False, FDSNone))
    -- (findParam parameters ("SPECIFIC_HEAT", False, FDSNone))
    -- (findParam parameters ("CONDUCTIVITY", False, FDSValue 0))
    -- (findParam parameters ("DENSITY", False, FDSValue 0))
    -- (findParam parameters ("HEAT_OF_COMBUSTION", False, FDSNone))
    -- (findParam parameters ("N_REACTIONS", False, FDSValue 0))
    -- (findParam parameters ("NU_FUEL", False, FDSValue 0))
    -- (findParam parameters ("N_S", False, FDSValue 1.0))
    -- pyrolysis_reac
    -- where --TODO: improve test to decide which pyrolysis to use (add error reporting)
        -- --shared
        -- b1 = paramExists parameters "HEAT_OF_REACTION"
        -- --Arrhenius
        -- b2 = paramExists parameters "A"
        -- b3 = paramExists parameters "E"
        -- --TGA
        -- b4 = paramExists parameters "REFERENCE_TEMPERATURE"
        -- b5 = paramExists parameters "HEATING_RATE"
        -- b6 = paramExists parameters "PYROLYSIS_RANGE"
        -- pyrolysis_reac
            -- | b2 || b3 = --TODO: make this behaviour match fds and report errors
                -- PyrolysisReacAE
                    -- (findParam parameters ("A", False, FDSNone))
                    -- (findParam parameters ("E", False, FDSNone))
                    -- (findParam parameters ("HEAT_OF_REACTION", False, FDSValue 0))
            -- | b4 || b5 || b6 =
                -- PyrolysisReacTGM
                    -- (findParam parameters ("REFERENCE_TEMPERATURE", False, FDSNone))
                    -- (findParam parameters ("HEATING_RATE", False, FDSValue 5))
                    -- (findParam parameters ("PYROLYSIS_RANGE", False, FDSValue 80))
                    -- (findParam parameters ("HEAT_OF_REACTION", False, FDSValue 0))
            -- | not (b1 && b2 && b3 && b4 && b5 && b6) = NoPyrolysis
-- matlOut (Matl id specific_heat conductivity density heat_of_combustion n_reactions nu_fuel n_s pyrolysis_reac) =
    -- Namelist "MATL" "" $
        -- exParameter "ID"                    id ++
        -- exParameter "SPECIFIC_HEAT"         specific_heat ++
        -- exParameter "CONDUCTIVITY"          conductivity ++
        -- exParameter "DENSITY"               conductivity ++
        -- exParameter "HEAT_OF_COMBUSTION"    heat_of_combustion ++
        -- exParameter "N_REACTIONS"           n_reactions ++
        -- exParameter "NU_FUEL"               nu_fuel ++
        -- exParameter "N_S"                   n_s
        -- ++ (pyroReacOut pyrolysis_reac)
-- pyroReacOut (PyrolysisReacAE a e heat_of_reaction) =
    -- exParameter "A"                 a ++
    -- exParameter "E"                 e ++
    -- exParameter "HEAT_OF_REACTION"  heat_of_reaction
-- pyroReacOut (PyrolysisReacTGM reference_temperature heating_rate pyrolysis_range heat_of_reaction) =
    -- exParameter "REFERENCE_TEMPERATURE"    reference_temperature ++
    -- exParameter "HEATING_RATE"      heating_rate ++
    -- exParameter "PYROLYSIS_RANGE"   pyrolysis_range ++
    -- exParameter "HEAT_OF_REACTION"  heat_of_reaction
-- pyroReacOut (NoPyrolysis) = []

-- surfIn :: [Matl] -> Namelist -> Surf
-- surfIn matlsFDS (Namelist _ _ parameters) = Surf
    -- (findParam parameters ("ID", False, FDSNone))
    -- (findParam parameters ("RGB", False, FDSNone))
    -- (findParam parameters ("COLOR", False, FDSNone))
    -- (findParam parameters ("BURN_AWAY", False, FDSValue False))
    -- surfLayers
    -- burner
    -- where
        -- thicknesses = (findParam parameters ("THICKNESS", False, FDSNone)) :: FDSValue [FDSValue Double]
        -- materials = (findParam parameters ("MATL_ID", False, FDSNone)) :: FDSValue [[FDSValue String]]
        -- matl_mass_fracs = (findParam parameters ("MATL_MASS_FRACTION", False, FDSNone)) -- :: [[FDSValue Double]]
        -- surfLayers = case thicknesses of
            -- FDSNone -> []
            -- _       -> map mkSLayer (zip3 thickVals matlVals massFracVals)
                -- where
                    -- thickVals = fromFDSValue thicknesses
                    -- massFracVals = fromFDSValue matl_mass_fracs
                    -- matlVals = fromFDSValueMatl materials
                    -- mkSLayer (thickness, matls, mass_fracs) = SurfLayer thickness (map mkSComp (zip matls mass_fracs))
                    -- mkSComp (matlName,mass_frac) = SurfLayerComponent matl mass_frac
                                                    -- where matl = fromJust (find ((==) (fromFDSValue matlName) . (\(Matl (FDSValue id) _ _ _ _ _ _ _ _) -> id)) matlsFDS)
        -- burner = SurfBurner
            -- (findParam parameters ("HRRPUA", False, FDSNone))
            -- (findParam parameters ("TAU_Q", False, FDSNone))
-- fromFDSValue x = case x of
    -- FDSValue y -> y
    -- FDSNone    -> error "missing values"
-- fromFDSValueMatl x = case x of
    -- FDSValue y -> y
    -- FDSNone    -> error "missing materials values"
-- surfOut (Surf id rgb color burn_away layers burner) =
    -- Namelist "SURF" "" $
        -- exParameter "ID"                    id ++
        -- exParameter "RGB"                    rgb ++
        -- exParameter "BURN_AWAY"         burn_away
        -- ++ (concat $ map surfLayerOut layers)
        -- ++ (burnerOut burner)
-- surfLayerOut (SurfLayer thickness components) =
    -- exParameter "THICKNESS"     thickness
    -- ++ (concat $ map surfComponentOut components)
-- surfComponentOut (SurfLayerComponent (Matl matl_id _ _ _ _ _ _ _ _) matl_mass_frac) =
    -- exParameter "MATL"    matl_id ++
    -- exParameter "MATL_MASS_FRACTION"      matl_mass_frac
-- burnerOut (SurfBurner hrrpua tau_q) =
    -- exParameter "HRRPUA"    hrrpua ++
    -- exParameter "TAU_Q"      tau_q

-- ventIn :: Namelist -> Vent
-- ventIn (Namelist _ _ parameters) = Vent
    -- (findParam parameters ("SURF_ID", False, FDSNone))
    -- (findParam parameters ("XB", False, FDSNone))
    -- (findParam parameters ("COLOR", False, FDSNone))
-- ventOut :: Vent -> Namelist
-- ventOut (Vent surf_id xb color) = Namelist "VENT" "" $
    -- exParameter "SURF_ID"   surf_id ++
    -- exParameter "XB"        xb ++
    -- exParameter "COLOR"     color
-- obstIn :: [Surf] -> Namelist -> Obst
-- obstIn surfsFDS (Namelist _ _ parameters) = Obst
        -- (findParam parameters ("XB", False, FDSNone))
        -- (fromJust (find ((==) surfName . (\(Surf (FDSValue id) _ _ _ _ _) -> id)) surfsFDS))
    -- where surfName = (\(FDSValue x) -> x) (findParam parameters ("SURF_ID", False, FDSNone))
-- obstOut :: Obst -> Namelist
-- obstOut (Obst xb (Surf surf_id _ _ _ _ _)) = Namelist "OBST" "" $
    -- exParameter "XB"        xb ++
    -- exParameter "SURF_ID"   surf_id



-- getNml nmlName namelists = fromJust $ find ((==) nmlName . (\(Namelist name _ _) -> name)) namelists

-- getAllNml nmlName namelists = filter ((==) nmlName . (\(Namelist name _ _) -> name)) namelists

-- --TODO: only output parameters that differ from the defaults


-- paramExists params parName =
    -- case matchResult of
        -- Just foundParam -> True
        -- Nothing -> False
    -- where matchResult = find ((==) parName . (\(Parameter name _) -> name)) params

-- --exParameter' (pName,fdsValue) = exParameter pName fdsValue
-- --exParameter pName (Matl id _ _ _ _ _ _ _ _) = [Parameter pName (exFDS id)]
-- exParameter pName fdsValue =
    -- case fdsValue of
        -- FDSNone -> []
        -- _       -> [Parameter pName (exFDS fdsValue)]

-- findParam :: (FDSType a) => [Parameter] -> (String, Bool, FDSValue a) -> FDSValue a
-- findParam [] (_,False,defVal) = defVal
-- findParam params (parName, mandatory, defVal) =
    -- case matchResult of
        -- Just foundParam -> inFDS $ (\(Parameter _ paramValue) -> paramValue) foundParam
        -- Nothing -> defVal
    -- where matchResult = find ((==) parName . (\(Parameter name _) -> name)) params
-- --          remWrapper (FDSValue (x:xs)) = (x:xs)
-- --          remWrapper x = x
-- -}
