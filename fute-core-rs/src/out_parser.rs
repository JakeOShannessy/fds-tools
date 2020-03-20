use chrono::{NaiveDateTime};
use serde::{Serialize, Deserialize};
use data_vector::DataVector;
use regex::Regex;
use std::{
    io::{BufRead, BufReader, Read}, path::Path
};


#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RunData {
    pub start_time: Option<f64>,
    pub end_time: Option<f64>,
    pub time_steps: DataVector<NaiveDateTime>,
}

impl RunData {
    pub fn from_out_file(out_path: &Path) -> Result<Self, std::io::Error> {
        let out_file = std::fs::File::open(out_path)?;
        Ok(Self::from_out_reader(out_file))
    }
    pub fn from_out_reader<R: Read>(out_file: R) -> Self {
        let parser = ReadOutParser::new(out_file);
        let mut data_vector = DataVector {
            name: "Run Time".to_string(),
            x_name: "Simulation Time".to_string(),
            y_name: "Wall Time".to_string(),
            x_units: "s".to_string(),
            y_units: "datetime".to_string(),
            values: Vec::new(),
        };
        let start_time = parser.sim_start;
        let end_time = parser.sim_end;
        for entry in parser {
            data_vector.values.push(data_vector::Point {
                x: entry.total_time,
                y: entry.datetime,
            })
        }
        Self {
            start_time,
            end_time,
            time_steps: data_vector,
        }
    }
}

pub struct ReadOutParser<R> {
    reader: std::io::Lines<BufReader<R>>,
    time_step_entry: Option<RuntimeEntry>,
    sim_start: Option<f64>,
    sim_end: Option<f64>,
    time_line_re: Regex,
    step_line_re: Regex,
    sim_start_re: Regex,
    sim_end_re: Regex,
}

impl<R: Read> ReadOutParser<R> {
    pub fn new(input: R) -> Self {
        // Simulation Start Time (s)          0.0
        let sim_start_re =
            Regex::new(r"Simulation Start Time \(s\)\s+(?P<start_time_string>.+)$").unwrap();
        // Simulation End Time (s)         2000.0
        let sim_end_re =
            Regex::new(r"Simulation End Time \(s\)\s+(?P<start_time_string>.+)$").unwrap();
        // Time Step   42800   March 13, 2020  22:28:11
        let time_line_re =
            Regex::new(r"Time Step\s*(?P<time_step>\d+)\s+(?P<date_string>.+)$").unwrap();
        // Step Size:    0.592E-02 s, Total Time:     520.76 s
        let step_line_re = Regex::new(
            r"Step Size\s*:\s*(?P<step_size>[\d\.eE\-\+]+)\s*s,\s*Total Time\s*:\s*(?P<total_time>[\d\.eE\-\+]+) s$",
        ).unwrap();
        Self {
            reader: BufReader::new(input).lines(),
            time_step_entry: None,
            time_line_re,
            step_line_re,
            sim_start_re,
            sim_end_re,
            sim_start: None,
            sim_end: None,
        }
    }
}

impl<R: Read> Iterator for ReadOutParser<R> {
    type Item = TimeStep;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let line = match self.reader.next() {
                Some(line) => line,
                None => return None,
            };
            let line = line.unwrap();
            let line = line.trim();
            if line.len() == 0 {
                continue;
            }

            if line.starts_with("Simulation Start Time") {
                for cap in self.sim_start_re.captures_iter(line) {
                    let sim_start: f64 = cap["sim_start_time"].parse().unwrap();
                    self.sim_start = Some(sim_start);
                    continue;
                }
            } else if line.starts_with("Simulation End Time") {
                for cap in self.sim_end_re.captures_iter(line) {
                    let sim_end: f64 = cap["sim_end_time"].parse().unwrap();
                    self.sim_end = Some(sim_end);
                    continue;
                }
            } else if line.starts_with("Time Step") {
                //   Time Step   42800   March 13, 2020  22:28:11
                for cap in self.time_line_re.captures_iter(line) {
                    let time_step: u64 = cap["time_step"].parse().unwrap();
                    let datetime: NaiveDateTime =
                        NaiveDateTime::parse_from_str(&cap["date_string"], "%B %e, %Y  %H:%M:%S")
                            .unwrap();
                    self.time_step_entry = Some(RuntimeEntry::TimeStep {
                        time_step,
                        datetime,
                    });
                    continue;
                }
            } else if line.starts_with("Step Size") {
                // Step Size:    0.592E-02 s, Total Time:     520.76 s
                for cap in self.step_line_re.captures_iter(line) {
                    let step_size: f64 = cap["step_size"].parse().unwrap();
                    let total_time: f64 = cap["total_time"].parse().unwrap();
                    if let Some(RuntimeEntry::TimeStep {
                        time_step,
                        datetime,
                    }) = self.time_step_entry
                    {
                        let r = Some(TimeStep {
                            time_step,
                            datetime,
                            step_size,
                            total_time,
                        });
                        self.time_step_entry = None;
                        return r;
                    } else {
                        return None;
                    }
                }
            } else {
                continue;
            }
        }
        None
    }
}

pub enum RuntimeEntry {
    TimeStep {
        time_step: u64,
        datetime: NaiveDateTime,
    },
    StepSize {
        step_size: f64,
        total_time: f64,
    },
}

pub struct TimeStep {
    time_step: u64,
    datetime: NaiveDateTime,
    step_size: f64,
    total_time: f64,
}
