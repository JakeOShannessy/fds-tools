#![allow(unused)]
#![allow(clippy::type_complexity)]
#![allow(non_snake_case)]
#[macro_use]
extern crate nom;
pub mod burners;
pub mod csv_parser;
pub mod html;
pub mod new_rev;
pub mod out_parser;
pub mod rename;
pub mod simple_flow;
mod slice_parser;
mod smv_parser;
pub mod summary;
pub mod verification_tests;
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
use simple_flow::{
    extracts::{self},
    supplies, SimpleFlow,
};
pub use slice_parser::parse_slice_file;
pub use smv_parser::parse_smv_file;
use smv_parser::SMVFile;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::str::FromStr;
pub use verification_tests::print_verification_tree;
pub use verification_tests::verify;
pub use verification_tests::verify_input;
use verification_tests::{SmokeDetector, Sprinkler, ThermalDetector};
pub mod hrrs;

const READ_BUFFER_SIZE: usize = 8192;
pub struct Outputs {
    pub smv_path: PathBuf,
    pub smv: SMVFile,
}

impl Outputs {
    pub fn new(smv_path: PathBuf) -> Self {
        let mut file = File::open(&smv_path)
            .unwrap_or_else(|_| panic!("Could not open smv file: {:?}", smv_path));
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
        take_f64_vec(vec)
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
    for value in values.iter() {
        let x = value.x;
        let y = match value.y {
            SmvValue::Float(y) => y,
            _ => return Err("not float".into()),
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
        match parse_slice_file(&read_buffer) {
            Ok(slice_file) => break,
            Err(nom::Err::Incomplete(n)) => println!("Needed: {:?}", n),
            Err(nom::Err::Error(e)) => panic!("Error: {:?}", e.code),
            Err(nom::Err::Failure(e)) => panic!("Failure: {:?}", e.code),
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
    fn total_max_hrr(&self) -> f64;
}

impl FdsFileExt for FdsFile {
    fn burners(&self) -> Vec<Burner> {
        burners::burners(self)
    }
    fn sprinklers(&self) -> Vec<Sprinkler> {
        self.devc
            .iter()
            .filter_map(|devc| {
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
        self.devc
            .iter()
            .filter_map(|devc| {
                if devc.is_smoke_detector(self) {
                    Some(SmokeDetector::from_devc(devc.clone(), self))
                } else {
                    None
                }
            })
            .collect()
    }
    fn thermal_detectors(&self) -> Vec<ThermalDetector> {
        // todo!("list thermal detectors")
        vec![]
    }
    /// To calculate the heat of combustion, deteremine the oxygen consumption
    /// then multiply by EPUMO2
    /// TODO: include the other methods of specifying HoC.
    /// Investigate the minor difference between this and the .out file value
    fn heat_of_combustion(&self) -> f64 {
        // TODO: deal with multiple reacs.
        let reac = self.reac.first().unwrap();
        let y_s = reac.soot_yield.unwrap_or(0.0);
        let y_co = reac.co_yield.unwrap_or(0.0);
        let soot_h_fraction = reac.soot_h_fraction.unwrap_or(0.1);
        // -- soot_h_fraction = case getParameterMaybe reac "SOOT_H_FRACTION" of
        // --     Just (ParDouble x) -> x
        // --     Nothing -> 0 -- TODO: abstract defaults to a separate table
        let epumo2 = reac.epumo2.unwrap_or(13100.0);
        // -- for fuel molecule CxHyOzNv
        // TODO: add defaults
        let x = reac.c.unwrap_or(0.0);
        let y = reac.h.unwrap_or(0.0);
        let z = reac.o.unwrap_or(0.0);
        let v = reac.n.unwrap_or(0.0);

        let w_c = 12.01; // Molar mass of atomic carbon.
        let w_h = 1.008; // Molar mass of atomic hydrogen.
        let w_o = 15.999; // Molar mass of atomic oxygen.
        let w_n = 14.007; //Molar mass of atomic nitrogen.

        let w_o2 = w_o * 2.0;
        let w_co = 1.0 * w_c + 1.0 * w_o;

        let v_F = 1.0;

        // Molar mass of fuel.
        let w_F = x * w_c + y * w_h + z * w_o + v * w_n;

        // 'v_' represents molar fraction
        let w_S = soot_h_fraction * w_h + (1.0 - soot_h_fraction) * w_c;
        let v_s = w_F / w_S * y_s;
        let v_co = w_F / w_co * y_co;
        let v_co2 = x - v_co - (1.0 - soot_h_fraction) * v_s;
        let v_h2o = y / 2.0 - soot_h_fraction / 2.0 * v_s;
        let v_o2 = v_co2 + v_co / 2.0 + v_h2o / 2.0 - z / 2.0;
        let v_n2 = v / 2.0;
        v_o2 * w_o2 * epumo2 / (v_F * w_F)
    }
    fn soot_production_rate(&self) -> f64 {
        let reac = self.reac.first().unwrap();
        let y_s = reac.soot_yield.unwrap_or(0.0);
        let hoc = self.heat_of_combustion();
        let hrr = self.total_max_hrr();
        y_s / hoc * hrr
    }
    fn total_max_hrr(&self) -> f64 {
        self.burners().iter().map(|burner| burner.max_hrr()).sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[ignore]
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
}
