use nom::number::streaming::le_u32;
use nom::{multi::many0, IResult};
use data_vector::{DataVector, Point};
use std::path::{Path, PathBuf};
use csv;


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

/// Parse all the information in the file and return a vector of DataVector.
/// This relies on the first entry being time.
pub fn get_csv_data(csv_path: &Path) -> Vec<DataVector> {
    use std::fs::File;
    use std::io::Read;
    let mut csv_file = File::open(&csv_path).expect("Could not open CSV file");
    // First we need to trim the first line from the csv
    // We start with a single byte buffer. This is a little hacky but it
    // works
    let mut buffer = [0; 1];
    loop {
        // Read a single byte off the start of the buffer
        let _n: usize = csv_file.read(&mut buffer).unwrap();

        // We have reached the end of the line (works for CRLF and LF)
        // '\n' == 10
        if buffer[0] == 10 {
            break;
        }
    }
    // let mut csv_contents = String::new();
    // file.read_to_string(&mut contents).unwrap();
    // Build the CSV reader and iterate over each record.
    let mut rdr = csv::ReaderBuilder::new()
        .has_headers(true)
        .trim(csv::Trim::All)
        .from_reader(csv_file);

    let mut data_vectors = Vec::new();
    let mut value_index_option: Option<usize> = None;
    let mut time_index_option: Option<usize> = None;
    let mut headers = rdr.headers().unwrap().into_iter();
    let first_header = headers.next().unwrap();
    // let other_headers: Vec<String> = headers.collect();
    for header in headers {
        data_vectors.push(DataVector {
            name: header.to_string(),
            x_units: "unknown".to_string(),
            x_name: first_header.to_string(),
            y_units: "unknown".to_string(),
            y_name: header.to_string(),
            values: Vec::new(),
        });
    }

    for result in rdr.deserialize() {
        // The iterator yields Result<StringRecord, Error>, so we check the
        // error here.
        let record: Vec<f64> = result.unwrap();
        let mut record_iter = record.into_iter();
        let x_val = record_iter.next().unwrap();
        for (i, entry) in record_iter.enumerate() {
            let dv = data_vectors.get_mut(i).unwrap();
            dv.values.push(Point {
                x: x_val,
                y: entry,
            });
        }
    }
    data_vectors
}
