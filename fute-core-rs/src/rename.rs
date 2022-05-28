use chid::Chid;
use std::path::Path;
use std::path::PathBuf;

pub fn rename_simulation(path: &Path, new_chid: Chid) -> Result<(), Box<dyn std::error::Error>> {
    // Check if path is a directory, if so, we assume that the SMV file is name
    // the same within this directory.
    if path.is_dir() {
        // First we rename the directory. We check that the new directory
        // doesn't already exist.
        let old_chid = path
            .file_name()
            .expect("no file name")
            .to_os_string()
            .into_string()
            .expect("bad file name");
        let new_dir_path: PathBuf = path
            .parent()
            .expect("path has no parent")
            .join(new_chid.as_str());
        if new_dir_path.exists() {
            panic!("path already exists");
        } else {
            std::fs::rename(path, &new_dir_path)?;
        }
        // Then we rename based on the SMV file.
        rename_smv(&new_dir_path.join(format!("{}.smv", old_chid)), new_chid)
    } else {
        rename_smv(path, new_chid)
    }
}

fn rename_smv(smv_path: &Path, new_chid: Chid) -> Result<(), Box<dyn std::error::Error>> {
    // Rename all the references within the SMV file.
    rename_smv_internal(smv_path, new_chid)?;
    // Rename all the files within the folder that match
    //
    // TODO: we are just
    // assuming that the .fds file is named appropriately. That is not
    // necessarily the case.
    let dir_name = smv_path.parent().expect("no parent");
    let old_chid = smv_path
        .file_stem()
        .expect("no file stem")
        .to_os_string()
        .into_string()
        .expect("bad file stem");
    for entry in dir_name.read_dir().expect("read_dir call failed").flatten() {
        let file_name = entry
            .file_name()
            .to_os_string()
            .into_string()
            .expect("bad file name");
        if file_name.starts_with(&old_chid) {
            let new_file_name = file_name.replace(&old_chid, new_chid.as_str());
            let new_path = dir_name.join(new_file_name);
            println!(
                "{} -> {}",
                file_name,
                new_path.file_name().unwrap().to_str().unwrap()
            );
            std::fs::rename(entry.path(), &new_path).unwrap();
        } else {
            println!("{} unchanged", file_name);
        }
    }
    // Modify the CHID in the FDS file.

    Ok(())
}

// Rename all the entries in the SMV file.
fn rename_smv_internal(smv_path: &Path, new_chid: Chid) -> Result<(), Box<dyn std::error::Error>> {
    use std::io::Read;
    use std::io::Write;
    {
        // Read in the old SMV file.
        let mut old_smv_file = std::fs::File::open(smv_path)?;
        let mut old_smv_text = String::new();
        old_smv_file.read_to_string(&mut old_smv_text)?;
        // Take the old CHID from the file name
        let old_chid = smv_path
            .file_stem()
            .expect("no file stem")
            .to_os_string()
            .into_string()
            .expect("bad file name");
        // Find and replace all instances of the old CHID with the new CHID.
        let new_smv_text = old_smv_text.replace(&old_chid, new_chid.as_str());
        let new_smv_path = smv_path
            .parent()
            .expect("no parent")
            .join(format!("{}.smv", new_chid));
        let mut new_smv_file = std::fs::File::create(new_smv_path)?;
        new_smv_file.write_all(new_smv_text.as_bytes())?;
    }
    // Delete the old file.
    std::fs::remove_file(smv_path)?;
    Ok(())
}
