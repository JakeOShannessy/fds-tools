use chrono::{DateTime, FixedOffset, NaiveDateTime, NaiveTime, Utc};
use data_vector::DataVector;
use regex::Regex;
use std::{
    ffi::OsString,
    io::{BufRead, BufReader, Read},
    path::PathBuf,
};

pub fn read_out<R: Read>(out_file: R) -> DataVector<NaiveDateTime> {
    let parser = ReadOutParser::new(out_file);
    let mut data_vector = DataVector {
        name: "Run Time".to_string(),
        x_name: "Simulation Time".to_string(),
        y_name: "Wall Time".to_string(),
        x_units: "s".to_string(),
        y_units: "datetime".to_string(),
        values: Vec::new(),
    };
    for entry in parser {
        data_vector.values.push(data_vector::Point {
            x: entry.total_time,
            y: entry.datetime,
        })
    }
    data_vector
}

pub struct ReadOutParser<R> {
    reader: std::io::Lines<BufReader<R>>,
    time_step_entry: Option<RuntimeEntry>,
    time_line_re: Regex,
    step_line_re: Regex,
}

impl<R: Read> ReadOutParser<R> {
    pub fn new(input: R) -> Self {
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

            if line.starts_with("Time Step") {
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
