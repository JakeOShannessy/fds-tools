-- |Check that a simulation is internally consistent
module FDSUtilities.Verification.CheckSim where

-- locate the FDS input file
-- get the CHID from the file
-- check that it matches the FDS file name and the prescribed CHID
-- check that a .smv file exists
-- check that all of the data files in the .smv are present
-- check that there are no additional non-listed data files
-- warn about the files names with the CHID but not recognised
-- check that the slice files are the same length (of time)
-- check that all data files of the same type have the same length
--   (of time)