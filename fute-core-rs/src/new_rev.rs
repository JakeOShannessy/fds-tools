
// createNewRev path' = do
// let path = dropTrailingPathSeparator path'
// let dirName = takeBaseName path
//     (projectNumber, mSpec, rSpec, name) = case parse parseDirName path dirName of
//       Right x -> x
//       Left e -> error $ show e
//     newRevSpec = rSpec + 1
//     newDirName = case name of
//       Just nm -> projectNumber ++ "_M" ++ mSpec ++ "_R" ++ show newRevSpec ++ "_" ++ nm
//       Nothing -> projectNumber ++ "_M" ++ mSpec ++ "_R" ++ show newRevSpec
// -- create a directory of the new revision in working directory
// exDir <- doesDirectoryExist newDirName
// if exDir then error "directory already exists" else return ()
// createDirectory newDirName
// let oldFDSPath = (joinPath [path, dirName ++ ".fds"])
// -- copy the fds file into the new directory with the new name
// fdsScript <- readFile oldFDSPath
// let newFDSScript = replace dirName newDirName fdsScript
//     newFDSPath = (joinPath [".", newDirName, newDirName ++ ".fds"])
// writeFile (joinPath [".", newDirName, newDirName ++ ".fds"]) newFDSScript
// return ()
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct RevName {
    /// The base name without the revision number but including 'R' if there is
    /// one. Does not include padding zeroes.
    pub name: String,
    /// The full width of the revision number (in decimal digits) left-padded
    /// with zeroes.
    pub rev_width: usize,
    /// The revision number.
    pub rev_num: u64,
}

impl RevName {
    pub fn new(name: String, rev_width: usize, rev_num: u64) -> Self {
        Self {
            name: name,
            rev_width,
            rev_num,
        }
    }
}

/// Parse a directory and .fds file into something we understand as having a
/// revision. This will comprise a string and either _N or _RN where N is a
/// number, possibly padded with zeroes. This must occur at the end of a string
/// and padding must be preserved.
pub fn parse_dir_name(dir_name: String) -> Option<RevName> {
    use regex::Regex;
    let re = Regex::new(r"^(.*?_R??)(0?)(\d*)$").unwrap();
    let captures = re.captures(&dir_name)?;
    println!("captures: {:?}", captures);
    if captures.len() != 4 {
        None
    } else {
        let name = &captures[1];
        let zero_padding = &captures[2];
        let rev_num_str: &str = &captures[3];
        let rev_width: usize = zero_padding.len() + rev_num_str.len();
        let rev_num: u64 = rev_num_str.parse().ok()?;

        Some(RevName {
            name: name.to_string(),
            rev_width,
            rev_num,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_example() {
        assert_eq!(parse_dir_name("1234_M1_R01".to_string()), Some(RevName::new("1234_M1_R".to_string(), 2_usize, 1_u64)));
        assert_eq!(parse_dir_name("1234_M1_R1".to_string()), Some(RevName::new("1234_M1_R".to_string(), 1_usize, 1_u64)));
        assert_eq!(parse_dir_name("1234_M1R1".to_string()), None);
        assert_eq!(parse_dir_name("1234_M1_R1x".to_string()), None);
    }
}
