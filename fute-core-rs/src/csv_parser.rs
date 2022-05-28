use chrono::prelude::*;
use csv;
use data_vector::{DataVector, Point};
use serde::{Deserialize, Serialize};
use std::path::Path;

#[derive(Clone)]
pub enum GetCsvDataError {
    CsvError,
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

impl std::error::Error for GetCsvDataError {}

#[derive(Clone, Serialize, Deserialize, PartialOrd, PartialEq)]
pub enum SmvValue {
    Float(f64),
    DateTime(DateTime<Utc>),
    String(String),
}

impl SmvValue {
    pub fn take_string(&self) -> String {
        match self {
            SmvValue::String(s) => s.clone(),
            _ => panic!("expected string"),
        }
    }

    pub fn take_float(&self) -> f64 {
        match self {
            SmvValue::Float(f) => *f,
            _ => panic!("expected string"),
        }
    }
}
pub trait SmvVec: downcast_rs::Downcast {
    fn name(&self) -> &String;
    fn set_name(&mut self, name: String);
    fn x_name(&self) -> &String;
    fn set_x_name(&mut self, name: String);
    fn y_name(&self) -> &String;
    fn set_y_name(&mut self, name: String);
    fn x_units(&self) -> &String;
    fn set_x_units(&mut self, units: String);
    fn y_units(&self) -> &String;
    fn set_y_units(&mut self, units: String);
    fn push_value(&mut self, x: f64, y_string: &str);
}
downcast_rs::impl_downcast!(SmvVec);

/// This struct contains all the data from a single csv file, preserving all of
/// the structural information. [`DataVector`]s, which compare two vectors can
/// be pulled from these data blocks. The safe usage of this data structure
/// relies on its vectors having matching lengths, so the fields are therefore
/// not directly modifiable.
pub struct CsvDataBlock {
    units: Vec<String>,
    names: Vec<String>,
    values: Vec<Vec<SmvValue>>,
}

impl CsvDataBlock {
    pub fn new() -> Self {
        Self {
            units: Vec::new(),
            names: Vec::new(),
            values: Vec::new(),
        }
    }

    /// Return the number of data vectors in the block.
    pub fn n_vectors(&self) -> usize {
        self.names.len()
    }
    /// Return the length of the vectors (they all share the same length).
    /// Return zero if there are no vectors.
    pub fn vec_len(&self) -> usize {
        self.values.get(0).map(|x| x.len()).unwrap_or(0)
    }
    /// Build data vectors from two vectors. Only takes the first vector if
    /// there are duplicates. Return None if no such vectors exist.
    pub fn make_data_vector(
        &self,
        x_name: &str,
        y_name: &str,
    ) -> Option<DataVector<f64, SmvValue>> {
        // First find the index of the first vector.
        let x_index = self.names.iter().position(|x| x == x_name)?;
        let y_index = self.names.iter().position(|x| x == y_name)?;
        let mut dv: DataVector<f64, SmvValue> = DataVector::new(
            y_name.to_string(),
            self.units.get(x_index).cloned()?,
            x_name.to_string(),
            self.units.get(y_index).cloned()?,
            y_name.to_string(),
            Vec::with_capacity(self.vec_len()),
        );
        let x_vec = self.values.get(x_index)?.iter();
        let y_vec = self.values.get(y_index)?.iter();
        for (x_val, y_val) in x_vec.zip(y_vec) {
            dv.insert(Point {
                x: x_val.take_float(),
                y: y_val.clone(),
            });
        }
        Some(dv)
    }

    pub fn default_vecs(&self) -> Vec<DataVector<f64, SmvValue>> {
        let mut names = self.names.iter();
        // The first name is our default x name.
        let x_name: &String = names.next().unwrap();
        let mut vecs = Vec::new();
        for y_name in names {
            let vec = self
                .make_data_vector(x_name.as_str(), y_name.as_str())
                .unwrap();
            vecs.push(vec);
        }
        vecs
    }

    pub fn from_file(csv_path: &Path) -> Result<CsvDataBlock, Box<dyn std::error::Error>> {
        let csv_file = std::fs::File::open(&csv_path)?;

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

        let mut rdr: csv::DeserializeRecordsIter<'_, std::fs::File, Vec<String>> =
            rdr.deserialize();

        let units_line = rdr.next().unwrap().unwrap().into_iter();
        let units: Vec<String> = units_line.collect();

        let names_line = rdr.next().unwrap().unwrap().into_iter();
        let names: Vec<String> = names_line.collect();
        let mut values = Vec::with_capacity(names.len());
        values.resize(names.len(), Vec::new());
        for result in rdr {
            // The iterator yields Result<StringRecord, Error>, so we check the
            // error here.
            let record: Vec<String> = result?;
            let record_iter = record.into_iter();
            for (((entry, vec), units), name) in record_iter
                .zip(values.iter_mut())
                .zip(units.iter())
                .zip(names.iter())
            {
                // Currently, we only support vectors of floats. We want to support
                // dates and ctrls as well. In particular dates at the moment.
                let value = match units.as_str() {
                    "" => {
                        if name == "Wall Time" {
                            // println!("entry")
                            SmvValue::DateTime(entry.parse().unwrap())
                        } else {
                            SmvValue::Float(entry.parse().unwrap())
                        }
                    }
                    _ => SmvValue::Float(entry.parse().unwrap_or(f64::NAN)),
                };
                vec.push(value);
            }
        }
        Ok(CsvDataBlock {
            units,
            names,
            values,
        })
    }
}

impl Default for CsvDataBlock {
    fn default() -> Self {
        Self::new()
    }
}

impl SmvVec for DataVector<f64, f64> {
    fn name(&self) -> &String {
        &self.name
    }
    fn set_name(&mut self, name: String) {
        self.name = name;
    }
    fn x_name(&self) -> &String {
        &self.x_name
    }
    fn set_x_name(&mut self, name: String) {
        self.x_name = name;
    }
    fn y_name(&self) -> &String {
        &self.y_name
    }
    fn set_y_name(&mut self, name: String) {
        self.y_name = name;
    }
    fn x_units(&self) -> &String {
        &self.x_units
    }
    fn set_x_units(&mut self, units: String) {
        self.x_name = units;
    }
    fn y_units(&self) -> &String {
        &self.y_units
    }
    fn set_y_units(&mut self, units: String) {
        self.y_units = units;
    }
    fn push_value(&mut self, x: f64, y_string: &str) {
        match self.y_units().as_str() {
            // We currently can't parse unknown units.
            "" => (),
            // Anything else is assumed to be a float.
            _ => match y_string.parse::<f64>() {
                Err(_) => panic!("invalid float"),
                Ok(value) => {
                    self.insert(Point { x, y: value });
                }
            },
        }
    }
}

impl SmvVec for DataVector<f64, DateTime<Utc>> {
    fn name(&self) -> &String {
        &self.name
    }
    fn set_name(&mut self, name: String) {
        self.name = name;
    }
    fn x_name(&self) -> &String {
        &self.x_name
    }
    fn set_x_name(&mut self, name: String) {
        self.x_name = name;
    }
    fn y_name(&self) -> &String {
        &self.y_name
    }
    fn set_y_name(&mut self, name: String) {
        self.y_name = name;
    }
    fn x_units(&self) -> &String {
        &self.x_units
    }
    fn set_x_units(&mut self, units: String) {
        self.x_name = units;
    }
    fn y_units(&self) -> &String {
        &self.y_units
    }
    fn set_y_units(&mut self, units: String) {
        self.y_units = units;
    }
    fn push_value(&mut self, x: f64, y_string: &str) {
        match y_string.parse::<DateTime<Utc>>() {
            Err(e) => panic!("invalid datetime for \"{}\": {}", y_string, e),
            Ok(value) => {
                println!("adding: {:?}", y_string);
                self.insert(Point { x, y: value });
            }
        }
    }
}

/// Parse all the information in the file and return a vector of DataVector.
/// This relies on the first entry being time.
pub fn get_csv_data(csv_path: &Path) -> Result<Vec<Box<dyn SmvVec>>, Box<dyn std::error::Error>> {
    use std::fs::File;

    let csv_file = File::open(&csv_path)?;
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

    let mut data_vectors: Vec<Box<dyn SmvVec>> = Vec::new();
    let mut value_index_option: Option<usize> = None;
    let mut time_index_option: Option<usize> = None;
    // let mut headers = rdr.headers()?.into_iter();

    {
        let mut units_line = rdr.next().unwrap().unwrap().into_iter();
        let x_units: Option<String> = units_line.next();
        let x_units = x_units.unwrap();
        // Get all the units
        for units in units_line {
            match units.as_str() {
                "" => {
                    if csv_path
                        .file_name()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .ends_with("_steps.csv")
                    {
                        let vec: DataVector<f64, DateTime<Utc>> = DataVector::new(
                            "unknown".to_string(),
                            x_units.clone(),
                            "unknown".to_string(),
                            units,
                            "unknown".to_string(),
                            Vec::new(),
                        );
                        let dv = Box::new(vec);
                        data_vectors.push(dv);
                    } else {
                        let vec: DataVector<f64, f64> = DataVector::new(
                            "unknown".to_string(),
                            x_units.clone(),
                            "unknown".to_string(),
                            units,
                            "unknown".to_string(),
                            Vec::new(),
                        );
                        let dv = Box::new(vec);
                        data_vectors.push(dv);
                    }
                }
                _ => {
                    let vec: DataVector<f64, f64> = DataVector::new(
                        "unknown".to_string(),
                        x_units.clone(),
                        "unknown".to_string(),
                        units,
                        "unknown".to_string(),
                        Vec::new(),
                    );
                    let dv = Box::new(vec);
                    data_vectors.push(dv);
                }
            };
        }
    }
    {
        let mut names_line = rdr.next().unwrap().unwrap().into_iter();
        let x_name: Option<String> = names_line.next();
        let x_name = x_name.unwrap();
        // Get all the units
        for (name, dv) in names_line.zip(data_vectors.iter_mut()) {
            dv.set_name(name.clone());
            dv.set_x_name(x_name.clone());
            dv.set_y_name(name);
            // dv.name = name.clone();
            // dv.x_name = x_name.clone();
            // dv.y_name = name;
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
        let x_val: f64 = record_iter
            .next()
            .ok_or(GetCsvDataError::CsvError)?
            .parse()
            .unwrap();
        for (entry, dv) in record_iter.zip(data_vectors.iter_mut()) {
            // Currently, we only support vectors of floats. We want to support
            // dates and ctrls as well. In particular dates at the moment.
            dv.push_value(x_val, entry.as_str());
        }
    }
    Ok(data_vectors)
}
