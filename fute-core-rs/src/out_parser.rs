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
        let mut parser = ReadOutParser::new(out_file);

        parser.parse();

        let start_time = parser.sim_start;
        let end_time = parser.sim_end;

        Self {
            start_time,
            end_time,
            time_steps: parser.time_step_vec,
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
    time_step_vec: DataVector<NaiveDateTime>,
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
            time_step_vec: DataVector {
                name: "Run Time".to_string(),
                x_name: "Simulation Time".to_string(),
                y_name: "Wall Time".to_string(),
                x_units: "s".to_string(),
                y_units: "datetime".to_string(),
                values: Vec::new(),
            },
        }
    }

    fn parse(&mut self) {
        loop {
            let line = match self.reader.next() {
                Some(line) => line,
                None => return,
            };
            let line = match line {
                Err(_) => continue,
                Ok(line) => line,
            };
            let line = line.trim();
            if line.len() == 0 {
                continue;
            }

            if line.starts_with("Simulation Start Time") {
                println!("found start: {}", line);
                for cap in self.sim_start_re.captures_iter(line) {
                    println!("cap: {:?}", cap);
                    match cap.get(1) {
                        None => (),
                        Some(sim_start_string) => {
                            match sim_start_string.as_str().parse() {
                                Err(_) => (),
                                Ok(time) => {
                                    println!("start time: {:?}", time);
                                    self.sim_start = Some(time)
                                },
                            }
                        }
                    }
                    continue;
                }
            } else if line.starts_with("Simulation End Time") {
                for cap in self.sim_end_re.captures_iter(line) {
                    match cap.get(1) {
                        None => (),
                        Some(sim_end_string) => {
                            match sim_end_string.as_str().parse() {
                                Err(_) => (),
                                Ok(time) => {
                                    self.sim_end = Some(time)
                                },
                            }
                        }
                    }
                    continue;
                }
            } else if line.starts_with("Time Step") {
                //   Time Step   42800   March 13, 2020  22:28:11
                for cap in self.time_line_re.captures_iter(line) {
                    let time_step: Option<u64> = match cap.get(1) {
                        None => None,
                        Some(string) => {
                            match string.as_str().parse() {
                                Err(_) => None,
                                Ok(x) => {
                                    Some(x)
                                },
                            }
                        }
                    };
                    let datetime: Option<NaiveDateTime> = match cap.get(2) {
                        None => None,
                        Some(match_val) => {
                            match NaiveDateTime::parse_from_str(match_val.as_str(), "%B %e, %Y  %H:%M:%S") {
                                Err(_) => None,
                                Ok(x) => {
                                    Some(x)
                                },
                            }
                        }
                    };
                    if let (Some(time_step), Some(datetime)) = (time_step, datetime) {
                        self.time_step_entry = Some(RuntimeEntry::TimeStep {
                            time_step,
                            datetime,
                        });
                    }
                    continue;
                }
            } else if line.starts_with("Step Size") {
                // Step Size:    0.592E-02 s, Total Time:     520.76 s
                for cap in self.step_line_re.captures_iter(line) {
                    let step_size: Option<f64> = match cap.get(1) {
                        None => None,
                        Some(string) => {
                            match string.as_str().parse() {
                                Err(_) => None,
                                Ok(x) => {
                                    Some(x)
                                },
                            }
                        }
                    };
                    let total_time: Option<f64> = match cap.get(2) {
                        None => None,
                        Some(string) => {
                            match string.as_str().parse() {
                                Err(_) => None,
                                Ok(x) => {
                                    Some(x)
                                },
                            }
                        }
                    };
                    if let Some(RuntimeEntry::TimeStep {
                        time_step,
                        datetime,
                    }) = self.time_step_entry
                    {
                        if let (Some(step_size), Some(total_time)) = (step_size, total_time) {
                            let r = TimeStep {
                                time_step,
                                datetime,
                                step_size,
                                total_time,
                            };
                            self.time_step_entry = None;
                            self.time_step_vec.values.push(data_vector::Point {
                                x: r.total_time,
                                y: r.datetime,
                            })
                        } else {
                            self.time_step_entry = None;
                            continue;
                        }
                    }
                }
            } else {
                continue;
            }
        }
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


#[cfg(test)]
mod test {
    use super::*;
    use std::path::PathBuf;

    #[ignore]
    #[test]
    fn out_parse_test() {
        let run_data = RunData::from_out_file(&PathBuf::from("room_fire.out")).unwrap();
        println!("run_data: {:?}", run_data);
    }

    #[test]
    fn start_regex() {
        let sim_start_re =
            Regex::new(r"Simulation Start Time \(s\)\s+(?P<start_time_string>.+)$").unwrap();
        let line = "Simulation Start Time (s)          0.0";
        assert!(line.starts_with("Simulation Start Time"));
        for cap in sim_start_re.captures_iter(line) {
            match cap.get(1) {
                None => (),
                Some(sim_start_string) => {
                    match sim_start_string.as_str().parse() {
                        Err(_) => (),
                        Ok(time) => {
                            let time: f64 = time;
                            println!("time: {:?}", time);
                            // self.sim_start = Some(time)
                        },
                    }
                }
            }
            continue;
        }
    }
}
