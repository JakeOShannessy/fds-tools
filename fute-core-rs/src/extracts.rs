use fds_input_parser::{
    decode::Mesh, decode::Obst, decode::Resolution, decode::Surf, decode::Vent, xb::HasXB, FDSFile,
};

pub fn extracts<'a>(fds_data: &'a FDSFile) -> Vec<Extract<'a>> {
    // Iterate through all the OBSTs and VENTs and determine which ones are
    // burners.
    let mut burners = Vec::new();
    for obst in fds_data.obsts.iter() {
        if obst.is_burner(fds_data) {
            burners.push(Extract::from_obst(fds_data, obst))
        }
    }
    for vent in fds_data.vents.iter() {
        if vent.is_burner(fds_data) {
            burners.push(Extract::from_vent(fds_data, vent))
        }
    }
    burners
}

/// A burner is a VENT or OBST that has a HRRPUA or an MLRPUA. That is, it
/// produces fuel. Each Burner is a collection of panels. For example, an OBST
/// which has a burner surface applied to its top and sides (but not bottom)
/// would be comprised of 5 panels. Each of the panels potentially has a
/// different surface.
#[derive(Clone, Debug)]
pub struct Extract<'a> {
    pub panels: Vec<ExtractPanel<'a>>,
}

impl<'a> Extract<'a> {
    /// Return the maximum flow rate of the extract.
    pub fn flow_rate(&self) -> f64 {
        self.panels.iter().map(|panel| panel.flow_rate()).sum()
    }

    pub fn from_obst(fds_data: &'a FDSFile, obst: &'a Obst) -> Self {
        let mut panels = Vec::new();
        if let Some(surf_id) = &obst.surf_id {
            // The OBST has been given a single surf_id, therefore this value
            // will be applied to all surfaces.
            if let Some(surf) = fds_data
                .surfs
                .iter()
                .find(|surf| surf.id.as_ref() == Some(surf_id))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstNegI(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect::<Vec<_>>(),
                    });
                    // Max X
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstPosI(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Min Y
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstNegJ(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Max Y
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstPosJ(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Min Z
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstNegK(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Max Z
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstPosK(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }
        } else if let Some((surf_id_top, surf_id_sides, surf_id_bottom)) = &obst.surf_ids {
            // The OBST has been given three surf_ids, on for top, one for
            // sides, and one for bottom.
            if let Some(surf) = fds_data
                .surfs
                .iter()
                .find(|surf| surf.id.as_ref() == Some(surf_id_top))
            {
                if surf.is_burner() {
                    // Top
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstPosK(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }
            if let Some(surf) = fds_data
                .surfs
                .iter()
                .find(|surf| surf.id.as_ref() == Some(surf_id_sides))
            {
                if surf.is_burner() {
                    // Sides
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstNegI(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstPosI(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstNegJ(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstPosJ(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surfs
                .iter()
                .find(|surf| surf.id.as_ref() == Some(surf_id_bottom))
            {
                if surf.is_burner() {
                    // Top
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstNegK(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }
        } else if let Some((min_x, max_x, min_y, max_y, min_z, max_z)) = &obst.surf_id6 {
            // The OBST has been given a separate surf_id for each side.
            if let Some(surf) = fds_data
                .surfs
                .iter()
                .find(|surf| surf.id.as_ref() == Some(min_x))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstNegI(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surfs
                .iter()
                .find(|surf| surf.id.as_ref() == Some(max_x))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstPosI(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surfs
                .iter()
                .find(|surf| surf.id.as_ref() == Some(min_y))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstNegJ(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surfs
                .iter()
                .find(|surf| surf.id.as_ref() == Some(max_y))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstPosJ(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surfs
                .iter()
                .find(|surf| surf.id.as_ref() == Some(min_z))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstNegK(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surfs
                .iter()
                .find(|surf| surf.id.as_ref() == Some(max_z))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(ExtractPanel {
                        object: ExtractObject::ObstPosK(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }
        }
        Extract { panels }
    }

    pub fn from_vent(fds_data: &'a FDSFile, vent: &'a Vent) -> Self {
        let mut panels = Vec::new();
        if let Some(surf_id) = &vent.surf_id {
            // The OBST has been given a single surf_id, therefore this value
            // will be applied to all surfaces.
            if let Some(surf) = fds_data
                .surfs
                .iter()
                .find(|surf| surf.id.as_ref() == Some(surf_id))
            {
                if surf.is_burner() {
                    todo!("Not sure how to deal with vent burner direction yet")
                    // // Min X
                    // panels.push(ExtractPanel {
                    //     object: ExtractObject::ObstNegI(obst),
                    //     surf,
                    //     meshes: fds_data.meshes.iter().filter(|mesh| mesh.intersect(obst)).collect(),
                    // });
                    // // Max X
                    // panels.push(ExtractPanel {
                    //     object: BurnerObject::ObstPosI(obst),
                    //     surf,
                    //     meshes: fds_data.meshes.iter().filter(|mesh| mesh.intersect(obst)).collect(),
                    // });
                }
            }
        }
        Extract { panels }
    }
}

#[derive(Clone, Debug)]
pub struct ExtractPanel<'a> {
    /// The object on which this panel is based.
    pub object: ExtractObject<'a>,
    /// The surface of this panel. TODO: we could probably compute this on
    /// demand.
    pub surf: &'a Surf,
    /// References to the meshes the burner lies within.
    pub meshes: Vec<&'a Mesh>,
}

impl<'a> ExtractPanel<'a> {
    /// Return the maximum HRR of the burner, i.e. once any ramp up time has
    /// completed. TODO: does not currently account for ramps.
    pub fn flow_rate(&self) -> f64 {
        self.flow_area() * self.vel()
    }

    /// Return the velocity of the surface.
    pub fn vel(&self) -> f64 {
        if let Some(volume_flow) = self.surf.volume_flow {
            volume_flow/self.flow_area()
        } else if let Some(vel) = self.surf.vel {
            vel
        } else {
            0.0
        }
    }

    /// Return the fuel area of the burner panel.
    pub fn flow_area(&self) -> f64 {
        self.object.area().unwrap_or(0.0)
    }
}

#[derive(Clone, Debug)]
pub enum ExtractObject<'a> {
    Vent(&'a Vent),
    ObstNegI(&'a Obst),
    ObstPosI(&'a Obst),
    ObstNegJ(&'a Obst),
    ObstPosJ(&'a Obst),
    ObstNegK(&'a Obst),
    ObstPosK(&'a Obst),
}

impl<'a> ExtractObject<'a> {
    pub fn area(&self) -> Option<f64> {
        match self {
            ExtractObject::Vent(vent) => vent.area(),
            ExtractObject::ObstNegI(obst) => obst.area_x(),
            ExtractObject::ObstPosI(obst) => obst.area_x(),
            ExtractObject::ObstNegJ(obst) => obst.area_y(),
            ExtractObject::ObstPosJ(obst) => obst.area_y(),
            ExtractObject::ObstNegK(obst) => obst.area_z(),
            ExtractObject::ObstPosK(obst) => obst.area_z(),
        }
    }
}
