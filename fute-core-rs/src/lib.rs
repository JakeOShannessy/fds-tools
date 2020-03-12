#[macro_use]
extern crate nom;
mod slice_parser;
mod smv_parser;
pub mod csv_parser;
pub mod new_rev;
mod verification_tests;

use std::io::Read;

pub use slice_parser::parse_slice_file;
pub use smv_parser::parse_smv_file;
pub use fds_input_parser::FDSFile;
pub use fds_input_parser::parse_and_decode_fds_input_file;
pub use fds_input_parser::decode;
use fds_input_parser::decode::*;
use std::path::PathBuf;
use std::fs::File;
use smv_parser::SMVFile;
use data_vector::DataVector;

const READ_BUFFER_SIZE: usize = 8192;

pub struct Outputs {
    pub smv_path: PathBuf,
    pub smv: SMVFile,
}

impl Outputs {
    pub fn new(smv_path: PathBuf) -> Self {
        let mut file = File::open(&smv_path).expect("Could not open smv file");
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .expect("Could not read smv file");
        let (_, smv) = parse_smv_file(&contents).expect("Could not parse smv file");
        Self {
            smv_path,
            smv,
        }
    }

    pub fn get_csv_vec(&mut self, csv_type: String, vec_name: String) -> Result<DataVector<f64>, Box<dyn std::error::Error>> {
        // TODO: add caching

        let hrr_csvf = self.smv
            .csvfs
            .iter()
            .find(|csvf| csvf.type_ == csv_type.as_str())
            .expect("No CSV file.");
        // println!("csvfhrr: {:?}", hrr_csvf);
        let smv_dir = PathBuf::from(self.smv_path.parent().unwrap());
        let mut csv_file_path = PathBuf::new();
        csv_file_path.push(smv_dir);
        csv_file_path.push(hrr_csvf.filename.clone());
        let csv_data = csv_parser::get_csv_data(&csv_file_path)?;
        for dv in csv_data {
            if dv.name == vec_name {
                return Ok(dv.clone());
            }
        }
        panic!("could not find dv")
    }
}

fn read_slice_file() -> std::io::Result<()> {
    let mut file = std::fs::File::open("src/room_fire_01.sf")?;
    // let mut buf_reader = std::io::BufReader::new(file);
    let mut read_buffer: Vec<u8> = vec![0; READ_BUFFER_SIZE];
    loop {
        let read = file.read(&mut read_buffer)?;
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

#[derive(Clone, Copy, Debug)]
pub struct Burner {
    // pub hrrpua: f64,
    // pub area: f64,
    pub hrr: f64,
}

impl Burner {
    pub fn from_obst(fds_file: &FDSFile, obst: &Obst) -> Self {
        if obst.surf_id.is_some() {
            let surf_id: &String = &obst.surf_id.as_ref().unwrap();
            let surf = fds_file.get_surf(surf_id).unwrap();
            let hrrpua = surf.hrrpua.unwrap();
            let area = 2.0*obst.area_x().unwrap() + 2.0*obst.area_y().unwrap() + 2.0*obst.area_z().unwrap();
            Self {
                hrr: hrrpua*area,
            }
        } else if obst.surf_ids.is_some() {
            let surf_ids: &(String,String,String) = obst.surf_ids.as_ref().unwrap();
            let surf_top = fds_file.get_surf(&surf_ids.0).unwrap();
            let surf_sides = fds_file.get_surf(&surf_ids.1).unwrap();
            let surf_bottom = fds_file.get_surf(&surf_ids.2).unwrap();
            let hrrpua_top = surf_top.hrrpua.unwrap_or(0.0);
            let hrrpua_sides = surf_sides.hrrpua.unwrap_or(0.0);
            let hrrpua_bottom = surf_bottom.hrrpua.unwrap_or(0.0);
            let area_top = obst.area_z().unwrap();
            let area_sides = 2.0*obst.area_x().unwrap() + 2.0*obst.area_y().unwrap();
            let area_bottom = obst.area_z().unwrap();
            Self {
                hrr: hrrpua_top*area_top + hrrpua_bottom*area_bottom + hrrpua_sides*area_sides,
            }
        } else if obst.surf_id6.is_some() {
            panic!("SURF_ID6 not supported")

        } else {
            panic!("not valid burner")
        }
    }

    pub fn from_vent(fds_file: &FDSFile, vent: &Vent) -> Self {
        let surf_id: &String = &vent.surf_id.as_ref().unwrap();
        let surf = fds_file.get_surf(surf_id).unwrap();
        let hrrpua = surf.hrrpua.unwrap();
        Self {
            hrr: hrrpua*(*vent).area().unwrap(),
        }
    }
}

pub trait FDSFileExt {
    fn burners(&self) -> Vec<Burner>;
}

impl FDSFileExt for FDSFile {
    fn burners(&self) -> Vec<Burner> {
        // First find all the obstructions which have a HRRPUA set. TODO: this
        // does not define all burners, as there are MLRPUA etc.
        let burner_surfs: Vec<&Surf> = self.surfs.iter().filter(|surf| surf.hrrpua.is_some()).collect();
        let burner_obsts: Vec<&Obst> = self.obsts.iter().filter(|obst| {
            for s in &burner_surfs {
                match &s.id {
                    Some(id) => if (**obst).has_surf(&id) {
                        return true;
                    },
                    _ => (),
                }

            }
            false
        }).collect();
        let burner_vents: Vec<&Vent> = self.vents.iter().filter(|vent| {
            for s in &burner_surfs {
                match &s.id {
                    Some(id) => if (**vent).has_surf(&id) {
                        return true;
                    },
                    _ => (),
                }

            }
            false
        }).collect();
        let mut obst_burners: Vec<Burner> = burner_obsts.iter().map(|obst| Burner::from_obst(self, obst)).collect();
        let mut vent_burners: Vec<Burner> = burner_vents.iter().map(|vent| Burner::from_vent(self, vent)).collect();
        obst_burners.append(&mut vent_burners);
        let burners = obst_burners;
        burners
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_buffered_slice() {
        read_slice_file().unwrap();
    }
}
