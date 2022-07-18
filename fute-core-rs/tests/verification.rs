use fds_input_parser::parse_and_decode_fds_input;
use fute_core::verification_tests::{
    devicesTest, hasFlowDevc, spkDetCeilingTest, TestResult, VerificationResult,
};
use std::path::PathBuf;

fn should_fail(tree: VerificationResult) {
    assert!(tree.has_failures())
}

fn should_succeed(tree: VerificationResult) {
    assert!(!tree.has_failures())
}

/// Unmetered flow device
#[test]
fn test_flow_devc1() {
    let file = r#"
&VENT XB=110,200,-225,-175,0,50 /
&SURF ID='Ex' VOLUME_FLOQ=5.0 /
"#;
    let fds_data = parse_and_decode_fds_input(file);
    assert!(!hasFlowDevc(
        &fds_data,
        fds_data.vent.first().as_ref().unwrap()
    ),);
}

/// Metered flow device
#[test]
fn test_flow_devc2() {
    let file = r#"
&VENT XB=110,200,-225,-175,0,50 /
&SURF ID='Ex' VOLUME_FLOQ=5.0 /
&DEVC XB=110,200,-225,-175,0,50, QUANTITY='VOLUME FLOW' /
"#;
    let fds_data = parse_and_decode_fds_input(file);
    assert!(hasFlowDevc(
        &fds_data,
        fds_data.vent.first().as_ref().unwrap()
    ),);
}

/// Incorrectly metered flow device (small device)
#[test]
fn test_flow_devc3() {
    let file = r#"
&VENT XB=110,200,-225,-175,0,50 /
&SURF ID='Ex' VOLUME_FLOQ=5.0 /
&DEVC XB=110,150,-225,-175,0,50, QUANTITY='VOLUME FLOW' /
"#;
    let fds_data = parse_and_decode_fds_input(file);
    assert!(!hasFlowDevc(
        &fds_data,
        fds_data.vent.first().as_ref().unwrap()
    ),);
}

/// Incorrectly metered flow device (wrong QUANTITY)
#[test]
fn test_flow_devc4() {
    let file = r#"
&VENT XB=110,200,-225,-175,0,50 /
&SURF ID='Ex' VOLUME_FLOQ=5.0 /
&DEVC XB=110,200,-225,-175,0,50, QUANTITY='TEMPERATURE' /
"#;
    let fds_data = parse_and_decode_fds_input(file);
    assert!(!hasFlowDevc(
        &fds_data,
        fds_data.vent.first().as_ref().unwrap()
    ),);
}

/// Device correctly identified as being within obstruction
#[test]
fn device_within_obstruction() {
    let path: PathBuf = "test-data/device-within-obstruction/detector-inside-block.fds"
        .parse()
        .unwrap();
    let fds_data = fds_input_parser::parse_and_decode_fds_input_file(&path).unwrap();
    let tree = devicesTest(&fds_data);
    // Tree should be a single test with a failure result
    should_fail(tree);
}

/// Device correctly identified as not being within obstruction
#[test]
fn device_not_within_obstruction() {
    let path: PathBuf = "test-data/device-within-obstruction/detector-not-inside-block.fds"
        .parse()
        .unwrap();
    let fds_data = fds_input_parser::parse_and_decode_fds_input_file(&path).unwrap();
    let tree = devicesTest(&fds_data);
    // Tree should be a single test with a failure result
    match tree {
        VerificationResult::Result(_, TestResult::Failure(_)) => {
            panic!("Verification test incorrectly failed")
        }
        VerificationResult::Result(_, TestResult::Success(_)) => (),
        _ => panic!("Unexpected tree"),
    }
}

/// Device correctly identified as being beneath an obstruction
#[test]
fn device_below_obstruction() {
    let path: PathBuf = "test-data/device-beneath-ceiling/sprinkler-beneath-ceiling.fds"
        .parse()
        .unwrap();
    let fds_data = fds_input_parser::parse_and_decode_fds_input_file(&path).unwrap();
    let tree = spkDetCeilingTest(&fds_data);
    // Tree should be a single test with a failure result
    should_succeed(tree);
}

/// Device correctly identified as being beneath an obstruction #2
#[test]
fn device_below_obstruction2() {
    let path: PathBuf = "test-data/device-beneath-ceiling/sprinkler-beneath-ceiling2.fds"
        .parse()
        .unwrap();
    let fds_data = fds_input_parser::parse_and_decode_fds_input_file(&path).unwrap();
    let tree = spkDetCeilingTest(&fds_data);
    // Tree should be a single test with a failure result
    should_succeed(tree);
}

/// Device correctly identified as not being beneath an obstruction
#[test]
fn device_not_below_obstruction() {
    let path: PathBuf = "test-data/device-beneath-ceiling/sprinkler-not-beneath-ceiling.fds"
        .parse()
        .unwrap();
    let fds_data = fds_input_parser::parse_and_decode_fds_input_file(&path).unwrap();
    let tree = spkDetCeilingTest(&fds_data);
    // Tree should be a single test with a failure result
    should_fail(tree);
}

/// Device correctly identified as being beneath a solid mesh boundary
#[test]
fn device_below_solid_mesh_boundary() {
    let path: PathBuf =
        "test-data/device-beneath-ceiling/sprinkler-beneath-solid-mesh-boundary.fds"
            .parse()
            .unwrap();
    let fds_data = fds_input_parser::parse_and_decode_fds_input_file(&path).unwrap();
    let tree = spkDetCeilingTest(&fds_data);
    // Tree should be a single test with a failure result
    should_fail(tree);
}

/// Device correctly identified as being beneath an open mesh boundary
#[test]
fn device_below_open_mesh_boundary() {
    let path: PathBuf = "test-data/device-beneath-ceiling/sprinkler-beneath-open-mesh-boundary.fds"
        .parse()
        .unwrap();
    let fds_data = fds_input_parser::parse_and_decode_fds_input_file(&path).unwrap();
    let tree = spkDetCeilingTest(&fds_data);
    // Tree should be a single test with a failure result
    should_fail(tree);
}

/// Device correctly identified as being beneath a shared mesh boundary
#[test]
fn device_below_shared_mesh_boundary() {
    let path: PathBuf =
        "test-data/device-beneath-ceiling/sprinkler-beneath-shared-mesh-boundary.fds"
            .parse()
            .unwrap();
    let fds_data = fds_input_parser::parse_and_decode_fds_input_file(&path).unwrap();
    let tree = spkDetCeilingTest(&fds_data);
    // Tree should be a single test with a failure result
    should_fail(tree);
}
