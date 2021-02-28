use fds_input_parser::{FDSFile, decode::Mesh, decode::Obst, decode::Resolution, decode::Surf, decode::Vent, xb::{HasXB, MightHaveXB}};

use super::SimpleFlow;

pub fn extracts<'a>(fds_data: &'a FDSFile) -> Vec<SimpleFlow<'a>> {
    // Iterate through all the OBSTs and VENTs and determine which ones are
    // extracts.
    let mut extracts = Vec::new();
    for obst in fds_data.obsts.iter() {
        if obst.is_extract(fds_data) {
            extracts.push(SimpleFlow::from_obst(fds_data, obst))
        }
    }
    for vent in fds_data.vents.iter() {
        if vent.is_extract(fds_data) {
            extracts.push(SimpleFlow::from_vent(fds_data, vent))
        }
    }
    extracts
}
