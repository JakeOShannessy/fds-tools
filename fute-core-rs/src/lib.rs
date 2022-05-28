#![allow(unused)]
#[macro_use]
extern crate nom;
pub mod burners;
pub mod simple_flow;
pub mod csv_parser;
pub mod new_rev;
pub mod out_parser;
pub mod rename;
pub mod html;
mod slice_parser;
mod smv_parser;
pub mod summary;
mod verification_tests;
use arrayvec::ArrayString;
use burners::Burner;
use csv_parser::{CsvDataBlock, SmvValue};
use data_vector::DataVector;
pub use fds_input_parser::decode;
pub use fds_input_parser::parse_and_decode_fds_input_file;
pub use fds_input_parser::FdsFile;
use serde::{
    de::{self, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};
use simple_flow::{SimpleFlow, extracts::{self}, supplies};
pub use slice_parser::parse_slice_file;
pub use smv_parser::parse_smv_file;
use smv_parser::SMVFile;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::str::FromStr;
pub use verification_tests::verify;
pub use verification_tests::verify_input;
pub use verification_tests::print_verification_tree;
use verification_tests::{SmokeDetector, Sprinkler, ThermalDetector};
pub mod hrrs;

const READ_BUFFER_SIZE: usize = 8192;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Chid(ArrayString<[u8; 50]>);

impl Chid {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl std::fmt::Display for Chid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ParseChidError {
    InvalidChar { position: usize, character: char },
    TooLong,
}

impl std::fmt::Display for ParseChidError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::InvalidChar {
                position,
                character,
            } => write!(
                f,
                "Invalid character {} at position {}",
                character, position
            ),
            Self::TooLong => write!(f, "CHID too long"),
        }
    }
}

impl std::error::Error for ParseChidError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self {
            Self::InvalidChar { .. } => None,
            Self::TooLong => None,
        }
    }
}

impl FromStr for Chid {
    type Err = ParseChidError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let array_string = match ArrayString::<[_; 50]>::from(s) {
            Ok(s) => s,
            Err(_) => return Err(ParseChidError::TooLong),
        };
        // Check that there are no invalid characters.
        match array_string.find(|c: char| c == '.' || c == ' ') {
            Some(position) => {
                return Err(ParseChidError::InvalidChar {
                    position,
                    character: array_string.chars().nth(position).unwrap(),
                })
            }
            None => (),
        }
        Ok(Chid(array_string))
    }
}

impl Serialize for Chid {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

struct ChidVisitor;

impl<'de> Visitor<'de> for ChidVisitor {
    type Value = Chid;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a run id beginning with \"scr-\"")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        value.parse().map_err(de::Error::custom)
    }
}

impl<'de> Deserialize<'de> for Chid {
    fn deserialize<D>(deserializer: D) -> Result<Chid, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(ChidVisitor)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Title(ArrayString<[u8; 256]>);

impl Title {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl std::fmt::Display for Title {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ParseTitleError {
    TooLong,
}

impl std::fmt::Display for ParseTitleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::TooLong => write!(f, "Title too long"),
        }
    }
}

impl std::error::Error for ParseTitleError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self {
            Self::TooLong => None,
        }
    }
}

impl FromStr for Title {
    type Err = ParseTitleError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let array_string = match ArrayString::<[_; 256]>::from(s) {
            Ok(s) => s,
            Err(_) => return Err(ParseTitleError::TooLong),
        };
        Ok(Title(array_string))
    }
}

pub struct Outputs {
    pub smv_path: PathBuf,
    pub smv: SMVFile,
}

impl Outputs {
    pub fn new(smv_path: PathBuf) -> Self {
        let mut file =
            File::open(&smv_path).expect(&format!("Could not open smv file: {:?}", smv_path));
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .expect("Could not read smv file");
        let (_, smv) = parse_smv_file(&contents).expect("Could not parse smv file");
        Self { smv_path, smv }
    }

    pub fn get_csv_vec(
        &mut self,
        csv_type: String,
        vec_name: String,
    ) -> Result<DataVector<f64, SmvValue>, Box<dyn std::error::Error>> {
        // TODO: add caching
        let hrr_csvf = self
            .smv
            .csvfs
            .iter()
            .find(|csvf| csvf.type_ == csv_type.as_str())
            .expect("No CSV file.");
        let smv_dir = PathBuf::from(self.smv_path.parent().unwrap());
        let mut csv_file_path = PathBuf::new();
        csv_file_path.push(smv_dir);
        csv_file_path.push(hrr_csvf.filename.clone());
        let data_block = CsvDataBlock::from_file(&csv_file_path)?;
        let vec = data_block
            .make_data_vector("Time", &vec_name)
            .ok_or("no vector")?;
        Ok(vec)
    }

    pub fn get_csv_vec_f64(
        &mut self,
        csv_type: String,
        vec_name: String,
    ) -> Result<DataVector<f64, f64>, Box<dyn std::error::Error>> {
        let vec = self.get_csv_vec(csv_type, vec_name)?;
        Ok(take_f64_vec(vec)?)
    }
}

fn take_f64_vec(
    vec: DataVector<f64, SmvValue>,
) -> Result<DataVector<f64, f64>, Box<dyn std::error::Error>> {
    let n = vec.values().len();
    let values = vec.values();
    let mut new_dv = DataVector::new(
        vec.name.clone(),
        vec.x_name.clone(),
        vec.y_name.clone(),
        vec.x_units.clone(),
        vec.y_units.clone(),
        Vec::with_capacity(n),
    );
    for value in values.into_iter() {
        let x = value.x;
        let y = match value.y {
            SmvValue::Float(y) => y,
            _ => return Err("not float")?,
        };
        new_dv.insert(data_vector::Point { x, y });
    }
    Ok(new_dv)
}

fn read_slice_file() -> std::io::Result<()> {
    let file = std::fs::File::open("src/room_fire_01.sf")?;
    // let mut buf = Vec::new();
    // file.read_to_end(&mut buf)?;
    let mut buf_reader = std::io::BufReader::new(file);
    let mut read_buffer: Vec<u8> = vec![0; READ_BUFFER_SIZE];
    // parse_slice_file(&buf).unwrap();
    loop {
        let read = buf_reader.read(&mut read_buffer)?;
        println!("read {} bytes", read);
        match parse_slice_file(&read_buffer) {
            Ok(slice_file) => break,
            Err(nom::Err::Incomplete(n)) => println!("Needed: {:?}", n),
            Err(nom::Err::Error(e)) => panic!("Error: {:?}", e.1),
            Err(nom::Err::Failure(e)) => panic!("Failure: {:?}", e.1),
        }
        if read == 0 {
            break;
        }
    }
    // assert_eq!(contents, "Hello, world!");
    Ok(())
}

// impl Burner {
//     pub fn from_obst(fds_file: &FdsFile, obst: &Obst) -> Self {
//         if obst.surf_id.is_some() {
//             let surf_id: &String = &obst.surf_id.as_ref().unwrap();
//             let surf = fds_file.get_surf(surf_id).unwrap();
//             let hrrpua = surf.hrrpua.unwrap();
//             let area = 2.0 * obst.area_x().unwrap()
//                 + 2.0 * obst.area_y().unwrap()
//                 + 2.0 * obst.area_z().unwrap();
//             Self { hrr: hrrpua * area }
//         } else if obst.surf_ids.is_some() {
//             let surf_ids: &(String, String, String) = obst.surf_ids.as_ref().unwrap();
//             let surf_top = fds_file.get_surf(&surf_ids.0).unwrap();
//             let surf_sides = fds_file.get_surf(&surf_ids.1).unwrap();
//             let surf_bottom = fds_file.get_surf(&surf_ids.2).unwrap();
//             let hrrpua_top = surf_top.hrrpua.unwrap_or(0.0);
//             let hrrpua_sides = surf_sides.hrrpua.unwrap_or(0.0);
//             let hrrpua_bottom = surf_bottom.hrrpua.unwrap_or(0.0);
//             let area_top = obst.area_z().unwrap();
//             let area_sides = 2.0 * obst.area_x().unwrap() + 2.0 * obst.area_y().unwrap();
//             let area_bottom = obst.area_z().unwrap();
//             Self {
//                 hrr: hrrpua_top * area_top
//                     + hrrpua_bottom * area_bottom
//                     + hrrpua_sides * area_sides,
//             }
//         } else if obst.surf_id6.is_some() {
//             panic!("SURF_ID6 not supported")
//         } else {
//             panic!("not valid burner")
//         }
//     }

//     pub fn from_vent(fds_file: &FdsFile, vent: &Vent) -> Self {
//         let surf_id: &String = &vent.surf_id.as_ref().unwrap();
//         let surf = fds_file.get_surf(surf_id).unwrap();
//         let hrrpua = surf.hrrpua.unwrap();
//         Self {
//             hrr: hrrpua * (*vent).area().unwrap(),
//         }
//     }
// }

pub trait FdsFileExt {
    fn burners(&self) -> Vec<Burner>;
    fn sprinklers(&self) -> Vec<Sprinkler>;
    fn extracts(&self) -> Vec<SimpleFlow>;
    fn supplies(&self) -> Vec<SimpleFlow>;
    fn smoke_detectors(&self) -> Vec<SmokeDetector>;
    fn thermal_detectors(&self) -> Vec<ThermalDetector>;
    fn heat_of_combustion(&self) -> f64;
    fn soot_production_rate(&self) -> f64;
}

impl FdsFileExt for FdsFile {
    fn burners(&self) -> Vec<Burner> {
        burners::burners(self)
    }
    fn sprinklers(&self) -> Vec<Sprinkler> {
        self.devc
            .iter()
            .filter_map(|devc| {
                println!("Looking at device: {:?}", devc.id);
                if devc.is_sprinkler(self) {
                    Some(Sprinkler::from_devc(devc.clone(), self))
                } else {
                    None
                }
            })
            .collect()
    }
    fn extracts(&self) -> Vec<SimpleFlow> {
        extracts::extracts(self)
    }
    fn supplies(&self) -> Vec<SimpleFlow> {
        supplies::supplies(self)
    }
    fn smoke_detectors(&self) -> Vec<SmokeDetector> {
        // todo!("list smoke detectors")
        vec![]
    }
    fn thermal_detectors(&self) -> Vec<ThermalDetector> {
        // todo!("list thermal detectors")
        vec![]
    }
    fn heat_of_combustion(&self) -> f64 {
        // todo!("return heat of combustion")
        0.0
    }
    fn soot_production_rate(&self) -> f64{
        // todo!("return soot production rate")
        0.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_buffered_slice() {
        read_slice_file().unwrap();
    }

    #[test]
    fn read_buffered_header() {
        let file = std::fs::File::open("src/room_fire_01.sf").unwrap();
        let mut sp = slice_parser::SliceParser::new(file);
        println!("{:?}", sp.header);
        println!("{:?}", sp.next());
        println!("{:?}", sp.next());
        // read_slice_file().unwrap();
    }

    #[test]
    fn parse_chid() {
        assert_eq!(
            Err(ParseChidError::TooLong),
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".parse::<Chid>()
        );
        assert_eq!("hello", "hello".parse::<Chid>().unwrap().as_str());
        assert_eq!(
            Err(ParseChidError::InvalidChar {
                position: 3,
                character: '.',
            }),
            "hel.lo".parse::<Chid>()
        );
    }
}
