use clap::{App, AppSettings, Arg, SubCommand, crate_version, crate_authors};
use env_logger;
use std::path::PathBuf;
mod commands;
use commands::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
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
        .subcommand(
            SubCommand::with_name("rename")
                .arg(
                    Arg::with_name("PATH")
                        .required(true)
                        .help("Path to SMV file or directory."),
                )
                .arg(
                    Arg::with_name("NEW-CHID")
                        .required(true)
                        .help("Thenew CHID to use."),
                )
                .about("Rename a simulation"),
        )
        .subcommand(
            SubCommand::with_name("compare")
                .arg(
                    Arg::with_name("ITEM")
                        .required(true)
                        .help("The value we will compare."),
                )
                .arg(
                    Arg::with_name("SMV-FILE-A")
                        .required(true)
                        .help("The first smv file to compare."),
                )
                .arg(
                    Arg::with_name("SMV-FILE-B")
                        .required(true)
                        .help("The second smv file to compare."),
                )
                .about("Compare data vectors from two different simulations"),
        )
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
    } else if let Some(compare_matches) = matches.subcommand_matches("compare") {
        let vector_name = compare_matches
                .value_of("ITEM")
                .expect("Invalid arguments").to_string();
        let smv_path_a = PathBuf::from(
            compare_matches
                .value_of("SMV-FILE-A")
                .expect("Invalid arguments"),
        );
        let smv_path_b = PathBuf::from(
            compare_matches
                .value_of("SMV-FILE-B")
                .expect("Invalid arguments"),
        );
        commands::compare(vector_name, smv_path_a, smv_path_b);
    } else if let Some(_show_hrr_matches) = matches.subcommand_matches("show-hrr") {
    } else if let Some(peak_hrr_matches) = matches.subcommand_matches("peak-hrr") {
        let fds_path = PathBuf::from(
            peak_hrr_matches
                .value_of("FDS-FILE")
                .expect("Invalid arguments"),
        );
        commands::peak_hrr(&fds_path)
    } else if let Some(_verify_input_matches) = matches.subcommand_matches("verify-input") {
    } else if let Some(rename_matches) = matches.subcommand_matches("rename") {
        let path = PathBuf::from(rename_matches.value_of("PATH").unwrap());
        let new_chid = rename_matches.value_of("NEW-CHID").unwrap();
        fute_core::rename::rename_simulation(&path, new_chid);
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
    }
    Ok(())
}
