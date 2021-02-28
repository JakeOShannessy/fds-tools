use fds_input_parser::{FDSFile, decode::{Mesh, Obst, Surf, Vent}, xb::MightHaveXB};

pub mod extracts;
pub mod supplies;


/// A burner is a VENT or OBST that has a HRRPUA or an MLRPUA. That is, it
/// produces fuel. Each Burner is a collection of panels. For example, an OBST
/// which has a burner surface applied to its top and sides (but not bottom)
/// would be comprised of 5 panels. Each of the panels potentially has a
/// different surface.
#[derive(Clone, Debug)]
pub struct SimpleFlow<'a> {
    pub panels: Vec<SimpleFlowPanel<'a>>,
}

impl<'a> SimpleFlow<'a> {
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
                if surf.is_extract() {
                    // Min X
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstNegI(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect::<Vec<_>>(),
                    });
                    // Max X
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstPosI(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Min Y
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstNegJ(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Max Y
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstPosJ(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Min Z
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstNegK(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Max Z
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstPosK(obst),
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
                if surf.is_extract() {
                    // Top
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstPosK(obst),
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
                if surf.is_extract() {
                    // Sides
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstNegI(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstPosI(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstNegJ(obst),
                        surf,
                        meshes: fds_data
                            .meshes
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstPosJ(obst),
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
                if surf.is_extract() {
                    // Top
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstNegK(obst),
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
                if surf.is_extract() {
                    // Min X
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstNegI(obst),
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
                if surf.is_extract() {
                    // Min X
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstPosI(obst),
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
                if surf.is_extract() {
                    // Min X
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstNegJ(obst),
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
                if surf.is_extract() {
                    // Min X
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstPosJ(obst),
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
                if surf.is_extract() {
                    // Min X
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstNegK(obst),
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
                if surf.is_extract() {
                    // Min X
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::ObstPosK(obst),
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
        SimpleFlow { panels }
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
                if surf.is_extract() {
                    panels.push(SimpleFlowPanel {
                        object: SimpleFlowObject::Vent(vent),
                        surf,
                        meshes: fds_data.meshes.iter().filter(|mesh| mesh.intersect(vent)).collect(),
                    });
                    // todo!("Not sure how to deal with vent burner direction yet")
                    // // Min X
                    // panels.push(SimpleFlowPanel {
                    //     object: SimpleFlowObject::ObstNegI(obst),
                    //     surf,
                    //     meshes: fds_data.meshes.iter().filter(|mesh| mesh.intersect(obst)).collect(),
                    // });
                    // // Max X
                    // panels.push(SimpleFlowPanel {
                    //     object: BurnerObject::ObstPosI(obst),
                    //     surf,
                    //     meshes: fds_data.meshes.iter().filter(|mesh| mesh.intersect(obst)).collect(),
                    // });
                }
            }
        }
        SimpleFlow { panels }
    }
}

#[derive(Clone, Debug)]
pub struct SimpleFlowPanel<'a> {
    /// The object on which this panel is based.
    pub object: SimpleFlowObject<'a>,
    /// The surface of this panel. TODO: we could probably compute this on
    /// demand.
    pub surf: &'a Surf,
    /// References to the meshes the burner lies within.
    pub meshes: Vec<&'a Mesh>,
}

impl<'a> SimpleFlowPanel<'a> {
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
pub enum SimpleFlowObject<'a> {
    Vent(&'a Vent),
    ObstNegI(&'a Obst),
    ObstPosI(&'a Obst),
    ObstNegJ(&'a Obst),
    ObstPosJ(&'a Obst),
    ObstNegK(&'a Obst),
    ObstPosK(&'a Obst),
}

impl<'a> SimpleFlowObject<'a> {
    pub fn area(&self) -> Option<f64> {
        match self {
            SimpleFlowObject::Vent(vent) => vent.area(),
            SimpleFlowObject::ObstNegI(obst) => obst.area_x(),
            SimpleFlowObject::ObstPosI(obst) => obst.area_x(),
            SimpleFlowObject::ObstNegJ(obst) => obst.area_y(),
            SimpleFlowObject::ObstPosJ(obst) => obst.area_y(),
            SimpleFlowObject::ObstNegK(obst) => obst.area_z(),
            SimpleFlowObject::ObstPosK(obst) => obst.area_z(),
        }
    }
}
