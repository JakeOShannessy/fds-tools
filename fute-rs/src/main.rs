#[macro_use]
extern crate log;
#[macro_use]
extern crate clap;
use clap::{App, AppSettings, Arg, SubCommand};
// use rustc_hex::ToHex;
use env_logger;
use std::fs::create_dir;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
mod commands;
use commands::*;

fn main() {
    env_logger::init();
    let matches = App::new("fute")
        .setting(AppSettings::ArgRequiredElseHelp)
        .version(crate_version!())
        .author(crate_authors!())
        .about("A collection of tools for FDS-SMV.")
        .subcommand(
            SubCommand::with_name("count-cells")
                .about("Count the total number of cells")
                .arg(
                    Arg::with_name("FDS-FILE")
                        .required(true)
                        .help("Path to FDS input file."),
                ),
        )
        .subcommand(
            SubCommand::with_name("meshes")
                .about("Display information on each mesh")
                .arg(
                    Arg::with_name("FDS-FILE")
                        .required(true)
                        .help("Path to FDS input file."),
                ),
        )
        .subcommand(
            SubCommand::with_name("mesh-check")
                .about("Check that the meshes are well behaved")
                .arg(
                    Arg::with_name("FDS-FILE")
                        .required(true)
                        .help("Path to FDS input file."),
                ),
        )
        // This will understand the ACL
        .subcommand(
            SubCommand::with_name("plot-hrr").about("Plot the HRR").arg(
                Arg::with_name("SMV-FILE")
                    .required(true)
                    .help("Path to SMV file."),
            ),
        )
        // This will understand the ACL
        .subcommand(
            SubCommand::with_name("show-hrr")
                .about("Plot and show the HRR")
                .arg(
                    Arg::with_name("SMV-FILE")
                        .required(true)
                        .help("Path to SMV file."),
                ),
        )
        .subcommand(
            SubCommand::with_name("peak-hrr")
                .about("Print the highest HRR value from available data")
                .arg(
                    Arg::with_name("FDS-FILE")
                        .required(true)
                        .help("Path to FDS file."),
                ),
        )
        .subcommand(
            SubCommand::with_name("verify-input")
                .about("Verify an FDS input file")
                .arg(
                    Arg::with_name("FDS-FILE")
                        .required(true)
                        .help("Path to FDS input file."),
                ),
        )
        .subcommand(SubCommand::with_name("rename").about("Rename a simulation"))
        .subcommand(
            SubCommand::with_name("new-rev")
                .arg(
                    Arg::with_name("DIR")
                        .required(true)
                        .help("Path to a directory holding an FDS file."),
                )
                .about("Create a new revision of a simulation"),
        )
        .subcommand(
            SubCommand::with_name("current-progress")
                .arg(
                    Arg::with_name("SMV-FILE")
                        .required(true)
                        .help("Path to SMV file."),
                )
                .about("Output the current progress of the simulation"),
        )
        .subcommand(
            SubCommand::with_name("plot-out")
                .arg(
                    Arg::with_name("OUT-FILE")
                        .required(true)
                        .help("Path to an FDS out file."),
                )
                .about("Plot information from the .out file"),
        )
        .subcommand(
            SubCommand::with_name("chart")
                .arg(
                    Arg::with_name("SMV-FILE")
                        .required(true)
                        .help("Path to an SMV file."),
                )
                .arg(
                    Arg::with_name("open")
                        .long("open")
                        .short("o")
                        .takes_value(false)
                        .help("Open the results"),
                )
                .about("Compile a summary of information"),
        )
        .subcommand(
            SubCommand::with_name("read-out")
                .arg(
                    Arg::with_name("OUT-FILE")
                        .required(true)
                        .help("Path to a .out file."),
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
    } else if let Some(_show_hrr_matches) = matches.subcommand_matches("show-hrr") {
    } else if let Some(peak_hrr_matches) = matches.subcommand_matches("peak-hrr") {
        let fds_path = PathBuf::from(
            peak_hrr_matches
                .value_of("FDS-FILE")
                .expect("Invalid arguments"),
        );
        commands::peak_hrr(&fds_path)
    } else if let Some(_verify_input_matches) = matches.subcommand_matches("verify-input") {
    } else if let Some(_rename_matches) = matches.subcommand_matches("rename") {
    } else if let Some(new_rev_matches) = matches.subcommand_matches("new-rev") {
        let dir_path = PathBuf::from(new_rev_matches.value_of("DIR").unwrap());
        fute_core::new_rev::create_new_rev(&dir_path);
    } else if let Some(_current_progress_matches) = matches.subcommand_matches("current-progress") {
    } else if let Some(_plot_out_matches) = matches.subcommand_matches("plot-out") {
    } else if let Some(quick_chart_matches) = matches.subcommand_matches("chart") {
        let smv_path = PathBuf::from(quick_chart_matches.value_of("SMV-FILE").unwrap());
        quick_chart(&smv_path);
    } else if let Some(read_out_matches) = matches.subcommand_matches("read-out") {
        let out_path = PathBuf::from(read_out_matches.value_of("OUT-FILE").unwrap());
        read_out(&out_path);
    }
}
