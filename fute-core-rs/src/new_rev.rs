use std::path::{Path, PathBuf};

pub fn create_new_rev(path: &Path) {
    if !path.is_dir() {
        panic!("path is not a directory");
    }
    let dir_name = path.file_name().unwrap().to_str().unwrap();
    let containing_dir = path.parent().unwrap();
    let mut current_rev = parse_dir_name(&dir_name).unwrap();
    // Create a new directory for this revision after incrementing. If the
    // directory already exists we just need to keep incrementing. TODO: warn
    // about increased width.
    loop {
        current_rev.increment();
        let new_dir_name = current_rev.to_dir_name();
        let mut new_dir_path = PathBuf::from(containing_dir);
        new_dir_path.push(&new_dir_name);
        match std::fs::create_dir_all(&new_dir_path) {
            Ok(_) => {
                // TODO: modify new FDS file here.
                let mut old_fds_path = PathBuf::from(path);
                old_fds_path.push(format!("{}.fds", dir_name));
                let mut new_fds_path = PathBuf::from(&new_dir_path);
                new_fds_path.push(format!("{}.fds", new_dir_name));
                // TODO: rather than just copy the file, we need to modify the
                // CHID parameter. We want to stream it.
                std::fs::copy(old_fds_path, new_fds_path).unwrap();
                return;
            }
            Err(err) => {
                if err.kind() == std::io::ErrorKind::AlreadyExists {
                    continue;
                } else {
                    panic!("error creating dir: {:?}", err);
                }
            }
        }
    }
}

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
    /// The notes trailing the CHID.
    pub tail_string: String,
}

impl RevName {
    pub fn new(name: String, rev_width: usize, rev_num: u64, tail_string: String) -> Self {
        Self {
            name: name,
            rev_width,
            rev_num,
            tail_string,
        }
    }

    pub fn increment(&mut self) {
        self.rev_num += 1;
    }

    pub fn to_dir_name(&self) -> String {
        format!(
            "{}{:0width$}{}",
            self.name,
            self.rev_num,
            self.tail_string,
            width = self.rev_width,
        )
    }
}

/// Parse a directory and .fds file into something we understand as having a
/// revision. This will comprise a string and either _N or _RN where N is a
/// number, possibly padded with zeroes. This must occur at the end of a string
/// and padding must be preserved.
pub fn parse_dir_name(dir_name: &str) -> Option<RevName> {
    use regex::Regex;
    let re = Regex::new(r"^(.*_R)(0?)(\d*)(.*)$").unwrap();
    let captures = re.captures(dir_name)?;
    if captures.len() != 5 {
        None
    } else {
        let name = &captures[1];
        let zero_padding = &captures[2];
        let rev_num_str: &str = &captures[3];
        let rev_width: usize = zero_padding.len() + rev_num_str.len();
        let rev_num: u64 = rev_num_str.parse().ok()?;
        let tail_str: &str = &captures[4];

        Some(RevName {
            name: name.to_string(),
            rev_width,
            rev_num,
            tail_string: tail_str.to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_example() {
        assert_eq!(
            parse_dir_name("1234_M1_R01"),
            Some(RevName::new("1234_M1_R".to_string(), 2_usize, 1_u64, "".to_string()))
        );
        assert_eq!(
            parse_dir_name("1234_M1_R1"),
            Some(RevName::new("1234_M1_R".to_string(), 1_usize, 1_u64, "".to_string()))
        );
        assert_eq!(parse_dir_name("1234_M1R1"), None);
        assert_eq!(parse_dir_name("1234_M1_R1x"), None);
    }

    #[test]
    fn basic_example_string() {
        let rev = parse_dir_name("1234_M1_R01").unwrap();
        assert_eq!(rev.to_dir_name(), "1234_M1_R01".to_string());
    }
}
