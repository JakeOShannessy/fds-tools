use serde::{Serialize, Deserialize};
use nom::number::streaming::le_u32;
use nom::{multi::many0, IResult};
use data_vector::{DataVector, Point};
use std::path::{Path, PathBuf};
use csv;
use chrono::prelude::*;


// println!("smv path: {:?}", smv_path);
//     let mut file = File::open(&smv_path).expect("Could not open smv file");
//     let mut contents = String::new();
//     file.read_to_string(&mut contents)
//         .expect("Could not read smv file");
//     let (_, smv_file) = parse_smv_file(&contents).expect("Could not parse smv file");
//     let hrr_csvf = smv_file
//         .csvfs
//         .iter()
//         .find(|csvf| csvf.type_ == "hrr")
//         .expect("No HRR CSV file.");
//     // println!("csvfhrr: {:?}", hrr_csvf);
//     let smv_dir = PathBuf::from(smv_path.parent().unwrap());
//     let value = "HRR";
//     let mut csv_file_path = PathBuf::new();
//     csv_file_path.push(smv_dir);
//     csv_file_path.push(hrr_csvf.filename.clone());

#[derive(Clone)]
pub enum GetCsvDataError {
    CsvError
}

impl std::fmt::Display for GetCsvDataError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "A csv parsing error occurred.")
	}
}

impl std::fmt::Debug for GetCsvDataError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		<GetCsvDataError as std::fmt::Display>::fmt(self, f)
	}
}

impl std::error::Error for GetCsvDataError { }

#[derive(Clone, Serialize, Deserialize)]
pub enum SmvValue {
    Float(f64),
    // DateTime(DateTime<Utc>),
    String(String),
}

impl SmvValue {
    pub fn take_string(self) -> String {
        match self {
            SmvValue::String(s) => s,
            _ => panic!("expected string"),
        }
    }

    pub fn take_float(self) -> f64 {
        match self {
            SmvValue::Float(f) => f,
            _ => panic!("expected string"),
        }
    }
}

/// Parse all the information in the file and return a vector of DataVector.
/// This relies on the first entry being time.
pub fn get_csv_data<T: Clone + std::str::FromStr>(csv_path: &Path) -> Result<Vec<DataVector<T>>, Box<dyn std::error::Error>> {
    use std::fs::File;
    use std::io::Read;
    let mut csv_file = File::open(&csv_path)?;
    // First we need to trim the first line from the csv
    // We start with a single byte buffer. This is a little hacky but it
    // works

    // let mut csv_contents = String::new();
    // file.read_to_string(&mut contents).unwrap();
    // Build the CSV reader and iterate over each record.

    let mut rdr = csv::ReaderBuilder::new()
        .has_headers(false)
        .trim(csv::Trim::All)
        .from_reader(csv_file);

    let mut rdr: csv::DeserializeRecordsIter<'_, std::fs::File, Vec<String>> = rdr.deserialize();


    let mut data_vectors = Vec::new();
    let mut value_index_option: Option<usize> = None;
    let mut time_index_option: Option<usize> = None;
    // let mut headers = rdr.headers()?.into_iter();

    {

        let mut units_line = rdr.next().unwrap().unwrap().into_iter();
        let x_units: Option<String> = units_line.next();
        let x_units = x_units.unwrap();
        // Get all the units
        for units in units_line {
            data_vectors.push(DataVector {
                name: "unknown".to_string(),
                x_units: x_units.clone(),
                x_name: "unknown".to_string(),
                y_units: units,
                y_name: "unknown".to_string(),
                values: Vec::new(),
            });
        }
    }
    {

        let mut names_line = rdr.next().unwrap().unwrap().into_iter();
        let x_name: Option<String> = names_line.next();
        let x_name = x_name.unwrap();
        // Get all the units
        for (name, dv) in names_line.zip(data_vectors.iter_mut()) {
            dv.name = name.clone();
            dv.x_name = x_name.clone();
            dv.y_name = name;
        }
    }
    // let first_header = headers.next().ok_or(GetCsvDataError::CsvError)?;
    // // let other_headers: Vec<String> = headers.collect();
    // for header in headers {
    //

    for result in rdr {
        // The iterator yields Result<StringRecord, Error>, so we check the
        // error here.
        let record: Vec<String> = result?;
        let mut record_iter = record.into_iter();
        let x_val: f64 = record_iter.next().ok_or(GetCsvDataError::CsvError)?.parse().unwrap();
        for (entry, dv) in record_iter.zip(data_vectors.iter_mut()) {
            // Currently, we only support vectors of floats. We want to support
            // dates and ctrls as well. In particular dates at the moment.
            match dv.y_units.as_str() {
                // We currently can't parse unknown units.
                "" => continue,
                // Anything else is assumed to be a float.
                _ => match entry.parse::<T>() {
                    Err(_) => panic!("invalid float"),
                    Ok(value) => {
                        dv.values.push(Point {
                            x: x_val,
                            y: value,
                        });
                    }
                }
            }


        }
    }
    Ok(data_vectors)
}
