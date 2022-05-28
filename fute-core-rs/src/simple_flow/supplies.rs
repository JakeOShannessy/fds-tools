use fds_input_parser::{
    decode::Mesh,
    decode::Obst,
    decode::Resolution,
    decode::Surf,
    decode::Vent,
    xb::{HasXB, MightHaveXB},
    FdsFile,
};

use super::SimpleFlow;

pub fn supplies(fds_data: &FdsFile) -> Vec<SimpleFlow<'_>> {
    // Iterate through all the OBSTs and VENTs and determine which ones are
    // extracts.
    let mut extracts = Vec::new();
    for obst in fds_data.obst.iter() {
        if obst.is_supply(fds_data) {
            extracts.push(SimpleFlow::from_obst(fds_data, obst))
        }
    }
    for vent in fds_data.vent.iter() {
        if vent.is_supply(fds_data) {
            extracts.push(SimpleFlow::from_vent(fds_data, vent))
        }
    }
    extracts
}
