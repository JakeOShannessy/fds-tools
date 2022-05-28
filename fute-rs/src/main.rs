use chid::Chid;
use clap::{crate_authors, crate_version, Arg, Command};
use std::path::PathBuf;
mod commands;
use commands::*;
use fute_core::csv_parser::SmvValue;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();
    let matches = Command::new("fute")
        .arg_required_else_help(true)
        .version(crate_version!())
        .author(crate_authors!())
        .about("A collection of tools for FDS-SMV.")
        .subcommand(
            Command::new("count-cells")
                .about("Count the total number of cells")
                .arg(
                    Arg::new("FDS-FILE")
                        .required(true)
                        .help("Path to FDS input file."),
                ),
        )
        .subcommand(
            Command::new("meshes")
                .about("Display information on each mesh")
                .arg(
                    Arg::new("FDS-FILE")
                        .required(true)
                        .help("Path to FDS input file."),
                ),
        )
        .subcommand(
            Command::new("mesh-check")
                .about("Check that the meshes are well behaved")
                .arg(
                    Arg::new("FDS-FILE")
                        .required(true)
                        .help("Path to FDS input file."),
                ),
        )
        // This will understand the ACL
        .subcommand(
            Command::new("plot-hrr").about("Plot the HRR").arg(
                Arg::new("SMV-FILE")
                    .required(true)
                    .help("Path to SMV file."),
            ),
        )
        // This will understand the ACL
        .subcommand(
            Command::new("show-hrr").about("Plot and show the HRR").arg(
                Arg::new("SMV-FILE")
                    .required(true)
                    .help("Path to SMV file."),
            ),
        )
        .subcommand(
            Command::new("peak-hrr")
                .about("Print the highest HRR value from available data")
                .arg(
                    Arg::new("FDS-FILE")
                        .required(true)
                        .help("Path to FDS file."),
                ),
        )
        .subcommand(
            Command::new("verify-input")
                .about("Verify an FDS input file")
                .arg(
                    Arg::new("FDS-FILE")
                        .required(true)
                        .help("Path to FDS input file."),
                ),
        )
        .subcommand(
            Command::new("copy-inputs")
                .about("Copy input and relevant output files.")
                .arg(
                    Arg::new("SRC-DIR")
                        .required(true)
                        .help("Path to the source directory."),
                )
                .arg(
                    Arg::new("DEST-DIR")
                        .required(true)
                        .help("Path to the destination directory."),
                ),
        )
        .subcommand(
            Command::new("verify")
                .about("Verify both the input and the output")
                .arg(
                    Arg::new("SMV-FILE")
                        .required(true)
                        .help("Path to an SMV file."),
                ),
        )
        .subcommand(
            Command::new("rename")
                .arg(
                    Arg::new("PATH")
                        .required(true)
                        .help("Path to SMV file or directory."),
                )
                .arg(
                    Arg::new("NEW-CHID")
                        .required(true)
                        .help("Thenew CHID to use."),
                )
                .about("Rename a simulation"),
        )
        .subcommand(
            Command::new("compare")
                .arg(
                    Arg::new("ITEM")
                        .required(true)
                        .help("The value we will compare."),
                )
                .arg(
                    Arg::new("SMV-FILES")
                        .required(true)
                        .min_values(1)
                        .help("Path to SMV files."),
                )
                .about("Compare data vectors from multiple different simulations"),
        )
        .subcommand(
            Command::new("new-rev")
                .arg(
                    Arg::new("DIR")
                        .required(true)
                        .help("Path to a directory holding an FDS file."),
                )
                .about("Create a new revision of a simulation"),
        )
        .subcommand(
            Command::new("current-progress")
                .arg(
                    Arg::new("SMV-FILE")
                        .required(true)
                        .help("Path to SMV file."),
                )
                .about("Output the current progress of the simulation"),
        )
        .subcommand(
            Command::new("plot-out")
                .arg(
                    Arg::new("OUT-FILE")
                        .required(true)
                        .help("Path to an FDS out file."),
                )
                .about("Plot information from the .out file"),
        )
        .subcommand(
            Command::new("chart")
                .arg(
                    Arg::new("SMV-FILE")
                        .required(true)
                        .help("Path to an SMV file."),
                )
                .arg(
                    Arg::new("open")
                        .long("open")
                        .short('o')
                        .takes_value(false)
                        .help("Open the results"),
                )
                .about("Compile a summary of information"),
        )
        .subcommand(
            Command::new("read-out")
                .arg(
                    Arg::new("OUT-FILE")
                        .required(true)
                        .help("Path to a .out file."),
                )
                .about("Read .out info"),
        )
        .subcommand(
            Command::new("hrr-vector")
                .arg(
                    Arg::new("SMV-FILE")
                        .required(true)
                        .help("Path to a .smv file."),
                )
                .about("Read .out info"),
        )
        .get_matches();

    if let Some(count_cells_matches) = matches.subcommand_matches("count-cells") {
        let input_path = PathBuf::from(count_cells_matches.value_of("FDS-FILE").unwrap());
        let n_cells = commands::count_cells(&input_path);
        println!("{}", n_cells);
    } else if let Some(meshes_matches) = matches.subcommand_matches("meshes") {
        let fds_path = PathBuf::from(
            meshes_matches
                .value_of("FDS-FILE")
                .expect("Invalid arguments"),
        );
        commands::meshes(&fds_path)
    } else if let Some(_mesh_check_matches) = matches.subcommand_matches("mesh-check") {
    } else if let Some(plot_hrr_matches) = matches.subcommand_matches("plot-hrr") {
        println!("plot-hrr");
        let smv_path = PathBuf::from(
            plot_hrr_matches
                .value_of("SMV-FILE")
                .expect("Invalid arguments"),
        );
        commands::plot_hrr(&smv_path)
    } else if let Some(compare_matches) = matches.subcommand_matches("compare") {
        let vector_name = compare_matches
            .value_of("ITEM")
            .expect("Invalid arguments")
            .to_string();
        let smv_paths: Vec<PathBuf> = compare_matches
            .values_of("SMV-FILES")
            .expect("Invalid arguments")
            .into_iter()
            .map(PathBuf::from)
            .collect();
        commands::compare(vector_name, smv_paths);
    } else if let Some(_show_hrr_matches) = matches.subcommand_matches("show-hrr") {
    } else if let Some(peak_hrr_matches) = matches.subcommand_matches("peak-hrr") {
        let fds_path = PathBuf::from(
            peak_hrr_matches
                .value_of("FDS-FILE")
                .expect("Invalid arguments"),
        );
        commands::peak_hrr(&fds_path)
    } else if let Some(opts) = matches.subcommand_matches("verify-input") {
        let fds_path = PathBuf::from(opts.value_of("FDS-FILE").unwrap());
        commands::verify_input(&fds_path).unwrap();
    } else if let Some(opts) = matches.subcommand_matches("copy-inputs") {
        let src_dir = PathBuf::from(opts.value_of("SRC-DIR").unwrap());
        let dest_dir = PathBuf::from(opts.value_of("DEST-DIR").unwrap());
        commands::copy_inputs(&src_dir, &dest_dir).unwrap();
    } else if let Some(opts) = matches.subcommand_matches("verify") {
        let smv_path = PathBuf::from(opts.value_of("SMV-FILE").unwrap());
        commands::verify(&smv_path).unwrap();
    } else if let Some(rename_matches) = matches.subcommand_matches("rename") {
        let path = PathBuf::from(rename_matches.value_of("PATH").unwrap());
        let new_chid: Chid = rename_matches
            .value_of("NEW-CHID")
            .unwrap()
            .parse()
            .unwrap();
        fute_core::rename::rename_simulation(&path, new_chid).unwrap();
    } else if let Some(new_rev_matches) = matches.subcommand_matches("new-rev") {
        let dir_path = PathBuf::from(new_rev_matches.value_of("DIR").unwrap());
        fute_core::new_rev::create_new_rev(&dir_path);
    } else if let Some(_current_progress_matches) = matches.subcommand_matches("current-progress") {
    } else if let Some(_plot_out_matches) = matches.subcommand_matches("plot-out") {
    } else if let Some(quick_chart_matches) = matches.subcommand_matches("chart") {
        let smv_path = PathBuf::from(quick_chart_matches.value_of("SMV-FILE").unwrap());
        quick_chart(&smv_path)?;
    } else if let Some(read_out_matches) = matches.subcommand_matches("read-out") {
        let out_path = PathBuf::from(read_out_matches.value_of("OUT-FILE").unwrap());
        read_out(&out_path)?;
    } else if let Some(hrr_vector_matches) = matches.subcommand_matches("hrr-vector") {
        let smv_path = PathBuf::from(hrr_vector_matches.value_of("SMV-FILE").unwrap());
        let hrr_vector = hrr_vector(&smv_path)?;
        let new_dv: data_vector::DataVector<f64, f64> = match hrr_vector.values()[0].y {
            SmvValue::Float(_) => {
                let vec = hrr_vector
                    .values()
                    .iter()
                    .map(|v| match v.y {
                        SmvValue::Float(d) => data_vector::Point { x: v.x, y: d },
                        _ => panic!("type changes part way through vector"),
                    })
                    .collect();
                data_vector::DataVector::new(
                    hrr_vector.name.clone(),
                    hrr_vector.x_name.clone(),
                    hrr_vector.y_name.clone(),
                    hrr_vector.x_units.clone(),
                    hrr_vector.y_units.clone(),
                    vec,
                )
            }
            _ => panic!("incorrect type"),
        };
        println!("{}", serde_json::to_string_pretty(&new_dv)?);
    }
    Ok(())
}
