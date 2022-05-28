use fds_input_parser::{
    decode::Mesh,
    decode::Obst,
    decode::Resolution,
    decode::Surf,
    decode::Vent,
    xb::{MightHaveXB}, FdsFile,
};

pub fn burners<'a>(fds_data: &'a FdsFile) -> Vec<Burner<'a>> {
    // Iterate through all the OBSTs and VENTs and determine which ones are
    // burners.
    let mut burners = Vec::new();
    for obst in fds_data.obst.iter() {
        if obst.is_burner(fds_data) {
            burners.push(Burner::from_obst(fds_data, obst))
        }
    }
    for vent in fds_data.vent.iter() {
        if vent.is_burner(fds_data) {
            burners.push(Burner::from_vent(fds_data, vent))
        }
    }
    burners
}

fn source_froude(q: f64, fuel_area: f64) -> f64 {
    let ambient_density = 1.205;
    let ambient_specific_heat = 1.005;
    let ambient_temperature = 293.15;
    let g = 9.81;
    let fuel_diameter = ((4.0 * fuel_area) / std::f64::consts::PI).sqrt();
    q / (ambient_density
        * ambient_specific_heat
        * ambient_temperature
        * (fuel_diameter.powi(2))
        * ((g * fuel_diameter).sqrt()))
}

/// A burner is a VENT or OBST that has a HRRPUA or an MLRPUA. That is, it
/// produces fuel. Each Burner is a collection of panels. For example, an OBST
/// which has a burner surface applied to its top and sides (but not bottom)
/// would be comprised of 5 panels. Each of the panels potentially has a
/// different surface.
#[derive(Clone, Debug)]
pub struct Burner<'a> {
    pub panels: Vec<BurnerPanel<'a>>,
    pub name: Option<String>,
}

impl<'a> Burner<'a> {
    /// Return the maximum HRR of the burner.
    pub fn max_hrr(&self) -> f64 {
        self.panels.iter().map(|panel| panel.max_hrr()).sum()
    }

    /// Return the source froude number of the burner.
    pub fn source_froude(&self) -> f64 {
        let max_hrr = self.max_hrr();
        let fuel_area = self.fuel_area();
        source_froude(max_hrr, fuel_area)
    }

    /// Return the fuel area of the burner.
    pub fn fuel_area(&self) -> f64 {
        self.panels.iter().map(|panel| panel.fuel_area()).sum()
    }

    /// Return the non-dimensionalised ratio of the burner.
    pub fn ndr(&self) -> Vec<f64> {
        self.panels.iter().map(|panel| panel.ndr()).collect()
    }

    pub fn from_obst(fds_data: &'a FdsFile, obst: &'a Obst) -> Self {
        let mut panels = Vec::new();
        if let Some(surf_id) = &obst.surf_id {
            // The OBST has been given a single surf_id, therefore this value
            // will be applied to all surfaces.
            if let Some(surf) = fds_data
                .surf
                .iter()
                .find(|surf| surf.id.as_ref() == Some(surf_id))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstNegI(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect::<Vec<_>>(),
                    });
                    // Max X
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstPosI(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Min Y
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstNegJ(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Max Y
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstPosJ(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Min Z
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstNegK(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    // Max Z
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstPosK(obst),
                        surf,
                        meshes: fds_data
                            .mesh
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
                .surf
                .iter()
                .find(|surf| surf.id.as_ref() == Some(surf_id_top))
            {
                if surf.is_burner() {
                    // Top
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstPosK(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }
            if let Some(surf) = fds_data
                .surf
                .iter()
                .find(|surf| surf.id.as_ref() == Some(surf_id_sides))
            {
                if surf.is_burner() {
                    // Sides
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstNegI(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstPosI(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstNegJ(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstPosJ(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surf
                .iter()
                .find(|surf| surf.id.as_ref() == Some(surf_id_bottom))
            {
                if surf.is_burner() {
                    // Top
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstNegK(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }
        } else if let Some((min_x, max_x, min_y, max_y, min_z, max_z)) = &obst.surf_id6 {
            // The OBST has been given a separate surf_id for each side.
            if let Some(surf) = fds_data
                .surf
                .iter()
                .find(|surf| surf.id.as_ref() == Some(min_x))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstNegI(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surf
                .iter()
                .find(|surf| surf.id.as_ref() == Some(max_x))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstPosI(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surf
                .iter()
                .find(|surf| surf.id.as_ref() == Some(min_y))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstNegJ(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surf
                .iter()
                .find(|surf| surf.id.as_ref() == Some(max_y))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstPosJ(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surf
                .iter()
                .find(|surf| surf.id.as_ref() == Some(min_z))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstNegK(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }

            if let Some(surf) = fds_data
                .surf
                .iter()
                .find(|surf| surf.id.as_ref() == Some(max_z))
            {
                if surf.is_burner() {
                    // Min X
                    panels.push(BurnerPanel {
                        object: BurnerObject::ObstPosK(obst),
                        surf,
                        meshes: fds_data
                            .mesh
                            .iter()
                            .filter(|mesh| mesh.intersect(obst))
                            .collect(),
                    });
                }
            }
        }
        let name = obst.id.clone();
        Burner { panels, name }
    }

    pub fn from_vent(fds_data: &'a FdsFile, vent: &'a Vent) -> Self {
        let mut panels = Vec::new();
        if let Some(surf_id) = &vent.surf_id {
            // The OBST has been given a single surf_id, therefore this value
            // will be applied to all surfaces.
            if let Some(surf) = fds_data
                .surf
                .iter()
                .find(|surf| surf.id.as_ref() == Some(surf_id))
            {
                if surf.is_burner() {
                    panels.push(BurnerPanel {
                        object: BurnerObject::Vent(vent),
                        surf,
                        // TODO: we should 'resolve' the mesh before calculating this (i.e. creating an integer solution).
                        // meshes: fds_data
                        //     .mesh
                        //     .iter()
                        //     .filter(|mesh| MightHaveXB::intersect(mesh,vent))
                        //     .collect(),
                        meshes: fds_data.mesh.iter().map(|x| x).collect(),
                    });
                }
            }
        }
        let name = vent.id.clone();
        Burner { panels, name }
    }
}

#[derive(Clone, Debug)]
pub struct BurnerPanel<'a> {
    /// The object on which this panel is based.
    pub object: BurnerObject<'a>,
    /// The surface of this panel. TODO: we could probably compute this on
    /// demand.
    pub surf: &'a Surf,
    /// References to the meshes the burner lies within.
    pub meshes: Vec<&'a Mesh>,
}

impl<'a> BurnerPanel<'a> {
    /// Return the maximum HRR of the burner, i.e. once any ramp up time has
    /// completed. TODO: does not currently account for ramps.
    pub fn max_hrr(&self) -> f64 {
        self.fuel_area() * self.hrrpua()
    }

    pub fn hrrpua(&self) -> f64 {
        if let Some(hrrpua) = self.surf.hrrpua {
            hrrpua
        } else if let Some(_mlrpua) = self.surf.mlrpua {
            todo!("Cannot handle MLRPUA")
        } else {
            0.0
        }
    }

    /// Determine the alpha value used by TAU_Q. This does not currently
    /// consider custom ramps etc.
    pub fn tau_q(&self) -> Option<f64> {
        // First we see if the SURF has a TAU_Q value, otherwise we skip to None.
        self.surf.tau_q
    }

    /// Return the source froude number of the burner.
    pub fn source_froude(&self) -> f64 {
        let max_hrr = self.max_hrr();
        let fuel_area = self.fuel_area();
        unimplemented!()
    }

    /// Return the fuel area of the burner panel.
    pub fn fuel_area(&self) -> f64 {
        self.object.area().unwrap_or(0.0)
    }

    /// Return the non-dimensionalised ratio of the burner.
    pub fn ndr(&self) -> f64 {
        let ambient_density = 1.205_f64;
        let ambient_specific_heat = 1.005_f64;
        let ambient_temperature = 293.15_f64;
        let g = 9.81_f64;
        let max_hrr = self.max_hrr();
        let resolutions: Vec<Resolution> = self.meshes.iter().map(|m| m.resolution()).collect();
        let nominal_cell_sizes: Vec<f64> =
            resolutions.into_iter().map(|res| res.max_side()).collect();
        let max_nominal_cell_size = if nominal_cell_sizes.len() == 0 {
            panic!("no cell dimensions for burner")
        } else {
            let mut m = nominal_cell_sizes[0];
            for s in nominal_cell_sizes {
                if s > m {
                    m = s
                }
            }
            m
        };
        let char_fire_diameter = (max_hrr
            / (ambient_density * ambient_specific_heat * ambient_temperature * (g.sqrt())))
        .powf(2_f64 / 5_f64);
        // Non-Dimensionalised Ratio
        let ndr = char_fire_diameter / max_nominal_cell_size;
        ndr
    }
}

#[derive(Clone, Debug)]
pub enum BurnerObject<'a> {
    Vent(&'a Vent),
    ObstNegI(&'a Obst),
    ObstPosI(&'a Obst),
    ObstNegJ(&'a Obst),
    ObstPosJ(&'a Obst),
    ObstNegK(&'a Obst),
    ObstPosK(&'a Obst),
}

impl<'a> BurnerObject<'a> {
    pub fn area(&self) -> Option<f64> {
        match self {
            BurnerObject::Vent(vent) => vent.area(),
            BurnerObject::ObstNegI(obst) => obst.area_x(),
            BurnerObject::ObstPosI(obst) => obst.area_x(),
            BurnerObject::ObstNegJ(obst) => obst.area_y(),
            BurnerObject::ObstPosJ(obst) => obst.area_y(),
            BurnerObject::ObstNegK(obst) => obst.area_z(),
            BurnerObject::ObstPosK(obst) => obst.area_z(),
        }
    }
}
