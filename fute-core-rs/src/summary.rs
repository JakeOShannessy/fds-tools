use crate::{
    html::{self, HtmlChild},
    FDSFileExt,
};
use fds_input_parser::{decode::Resolution, FDSFile};
use html::HtmlElement;

pub fn summarise_input(fds_data: &FDSFile) -> InputSummary {
    // println!("{:?}", fds_data);
    let chid = fds_data
        .head
        .as_ref()
        .expect("No HEAD timelist")
        .chid
        .as_ref()
        .unwrap()
        .clone();
    let time = fds_data.time.as_ref().expect("No TIME namelist");
    let t_start = time.t_begin;
    let t_end = time.t_end;
    let simulation_length = t_end.expect("No T_END value") - t_start.unwrap_or(0.0);
    let burners = fds_data.burners();
    let n_burners = burners.len();
    let total_max_hrr: f64 = burners.iter().map(|burner| burner.max_hrr()).sum();
    let ndrs: Vec<Vec<_>> = burners.iter().map(|burner| burner.ndr()).collect();
    let extracts = fds_data.extracts();
    let n_extract_vents = extracts.len();
    for extract in &extracts {
        println!("Extract: {:?}", extract);
    }
    let total_extract_rate = extracts
        .iter()
        .map(|extract| extract.flow_rate().abs())
        .sum();

    let supplies = fds_data.supplies();
    let n_supply_vents = supplies.len();
    let total_supply_rate = supplies.iter().map(|supply| supply.flow_rate().abs()).sum();

    let meshes = &fds_data.meshes;
    let mesh_resolutions: Vec<_> = meshes.iter().map(|mesh| mesh.resolution()).collect();
    let n_meshes = meshes.len();
    let n_cells: u64 = meshes.iter().map(|mesh| mesh.n_cells()).sum();
    // let (sX, sY, sZ) = fds_data.smallest_resolution();

    // There are multiple ways to order resolutions. Here we order by volume of the cells.
    let ordered_resolutions = {
        let mut x = mesh_resolutions.clone();
        x.sort_by(|a, b| a.volume().partial_cmp(&b.volume()).unwrap());
        x
    };

    let sprinklers = fds_data.sprinklers();
    let n_sprinklers = sprinklers.len();
    let sprinkler_activation_temperatures = sprinklers
        .iter()
        .flat_map(|sprinkler| sprinkler.activation_temperature())
        .collect();
    let smoke_detectors = fds_data.smoke_detectors();
    let n_smoke_detectors = smoke_detectors.len();
    let smoke_detector_obscurations: Vec<f64> = smoke_detectors
        .iter()
        .map(|detector| detector.obscuration())
        .collect();

    let n_thermal_detectors = fds_data.thermal_detectors().len();

    let heat_of_combustion = fds_data.heat_of_combustion();
    let total_soot_production = fds_data.soot_production_rate();

    InputSummary {
        chid,
        simulation_length,
        n_burners,
        total_max_hrr,
        heat_of_combustion,
        total_soot_production,
        n_sprinklers,
        sprinkler_activation_temperatures,
        n_smoke_detectors,
        smoke_detector_obscurations,
        n_extract_vents,
        total_extract_rate,
        n_supply_vents,
        total_supply_rate,
        n_meshes,
        n_cells,
        mesh_resolutions,
        ndrs,
    }
}

pub struct InputSummary {
    pub chid: String,
    pub simulation_length: f64,
    pub n_burners: usize,
    pub total_max_hrr: f64,
    pub heat_of_combustion: f64,
    pub total_soot_production: f64,
    pub n_sprinklers: usize,
    pub sprinkler_activation_temperatures: Vec<f64>,
    pub n_smoke_detectors: usize,
    pub smoke_detector_obscurations: Vec<f64>,
    pub n_extract_vents: usize,
    pub total_extract_rate: f64,
    pub n_supply_vents: usize,
    pub total_supply_rate: f64,
    pub n_meshes: usize,
    pub n_cells: u64,
    pub mesh_resolutions: Vec<Resolution>,
    pub ndrs: Vec<Vec<f64>>,
}

impl InputSummary {
    pub fn to_html(&self) -> HtmlElement {
        let mut table = HtmlElement::new("table".to_string());
        table
            .attributes
            .insert("class".to_string(), "summary-table".to_string());
        // General section
        {
            let mut general_head = HtmlElement::new("head".to_string());
            let mut general_head_row = HtmlElement::new("tr".to_string());
            let mut general_head_cell = HtmlElement::new("th".to_string());
            general_head_cell
                .children
                .push(HtmlChild::String("General".to_string()));
            general_head_row
                .children
                .push(HtmlChild::Element(general_head_cell));
            general_head
                .children
                .push(HtmlChild::Element(general_head_row));
            let mut body = HtmlElement::new("tbody".to_string());
            // CHID
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("CHID".to_string()));
                chid_cell
                    .children
                    .push(HtmlChild::String(self.chid.clone()));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // Simulation Time
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("Simulation Time".to_string()));
                chid_cell
                    .children
                    .push(HtmlChild::String(format!("{} s", self.simulation_length)));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            table.children.push(HtmlChild::Element(general_head));
            table.children.push(HtmlChild::Element(body));
        }
        // Fire section
        {
            let mut head = HtmlElement::new("head".to_string());
            let mut head_row = HtmlElement::new("tr".to_string());
            let mut head_cell = HtmlElement::new("th".to_string());
            head_cell
                .children
                .push(HtmlChild::String("Fire".to_string()));
            head_row.children.push(HtmlChild::Element(head_cell));
            head.children.push(HtmlChild::Element(head_row));
            table.children.push(HtmlChild::Element(head));
            let mut body = HtmlElement::new("tbody".to_string());
            // # Burners
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("# Burners".to_string()));
                chid_cell
                    .children
                    .push(HtmlChild::String(format!("{}", self.n_burners)));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // Total Max. HRR
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("Total Max. HRR".to_string()));
                chid_cell
                    .children
                    .push(HtmlChild::String(format!("{:.2} kW", self.total_max_hrr)));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // Heat of Combustion
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("Heat of Combustion".to_string()));
                chid_cell.children.push(HtmlChild::String(format!(
                    "{:.2} kJ/kg",
                    self.heat_of_combustion
                )));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // Heat of Combustion
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell.children.push(HtmlChild::String(
                    "Total Max. Soot Production Rate".to_string(),
                ));
                chid_cell.children.push(HtmlChild::String(format!(
                    "{:.2} kg/s",
                    self.total_soot_production
                )));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // Non-Dimensionalized Ratios
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("Non-Dimensionalized Ratios".to_string()));
                if self.ndrs.is_empty() {
                    chid_cell
                        .children
                        .push(HtmlChild::String("No Burners".to_string()));
                } else {
                    let mut list = HtmlElement::new("ul".to_string());
                    for item in self.ndrs.iter() {
                        let mut li = HtmlElement::new("li".to_string());
                        li.children.push(HtmlChild::String(format!("{:?}", item)));
                        list.children.push(HtmlChild::Element(li));
                    }
                    chid_cell.children.push(HtmlChild::Element(list));
                }
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            table.children.push(HtmlChild::Element(body));
        }
        // Sprinklers section
        {
            let mut head = HtmlElement::new("head".to_string());
            let mut head_row = HtmlElement::new("tr".to_string());
            let mut head_cell = HtmlElement::new("th".to_string());
            head_cell
                .children
                .push(HtmlChild::String("Sprinklers".to_string()));
            head_row.children.push(HtmlChild::Element(head_cell));
            head.children.push(HtmlChild::Element(head_row));
            table.children.push(HtmlChild::Element(head));
            let mut body = HtmlElement::new("tbody".to_string());
            // # Sprinklers
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("# Sprinklers".to_string()));
                chid_cell
                    .children
                    .push(HtmlChild::String(format!("{}", self.n_sprinklers)));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // Sprinker Activation Temperature
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell.children.push(HtmlChild::String(
                    "Sprinkler Activation Temperatures".to_string(),
                ));
                if self.sprinkler_activation_temperatures.is_empty() {
                    chid_cell
                        .children
                        .push(HtmlChild::String("No Sprinklers".to_string()));
                } else {
                    let mut list = HtmlElement::new("ul".to_string());
                    for item in self.sprinkler_activation_temperatures.iter() {
                        let mut li = HtmlElement::new("li".to_string());
                        li.children
                            .push(HtmlChild::String(format!("{:.2} °C", item)));
                        list.children.push(HtmlChild::Element(li));
                    }
                    chid_cell.children.push(HtmlChild::Element(list));
                }
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            table.children.push(HtmlChild::Element(body));
        }
        // Detection section
        {
            let mut head = HtmlElement::new("head".to_string());
            let mut head_row = HtmlElement::new("tr".to_string());
            let mut head_cell = HtmlElement::new("th".to_string());
            head_cell
                .children
                .push(HtmlChild::String("Detection".to_string()));
            head_row.children.push(HtmlChild::Element(head_cell));
            head.children.push(HtmlChild::Element(head_row));
            table.children.push(HtmlChild::Element(head));
            let mut body = HtmlElement::new("tbody".to_string());
            // # Sprinklers
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("# Detectors".to_string()));
                chid_cell
                    .children
                    .push(HtmlChild::String(format!("{}", self.n_sprinklers)));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // Detector Obscurations
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("Smoke Detector Obscurations".to_string()));
                if self.smoke_detector_obscurations.is_empty() {
                    chid_cell
                        .children
                        .push(HtmlChild::String("No Obscuration Levels".to_string()));
                } else {
                    let mut list = HtmlElement::new("ul".to_string());
                    for item in self.smoke_detector_obscurations.iter() {
                        let mut li = HtmlElement::new("li".to_string());
                        li.children
                            .push(HtmlChild::String(format!("{:.2} %/m", item)));
                        list.children.push(HtmlChild::Element(li));
                    }
                    chid_cell.children.push(HtmlChild::Element(list));
                }
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            table.children.push(HtmlChild::Element(body));
        }
        // Ventilation section
        {
            let mut head = HtmlElement::new("head".to_string());
            let mut head_row = HtmlElement::new("tr".to_string());
            let mut head_cell = HtmlElement::new("th".to_string());
            head_cell
                .children
                .push(HtmlChild::String("Ventilation".to_string()));
            head_row.children.push(HtmlChild::Element(head_cell));
            head.children.push(HtmlChild::Element(head_row));
            table.children.push(HtmlChild::Element(head));
            let mut body = HtmlElement::new("tbody".to_string());
            // # Extracts
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("# Extracts".to_string()));
                chid_cell
                    .children
                    .push(HtmlChild::String(format!("{}", self.n_extract_vents)));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // Extract Rate
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("Total Extract Rate".to_string()));
                chid_cell.children.push(HtmlChild::String(format!(
                    "{:.2} m³/s",
                    self.total_extract_rate
                )));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // # Supplies
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("# Supplies".to_string()));
                chid_cell
                    .children
                    .push(HtmlChild::String(format!("{}", self.n_supply_vents)));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // Supply Rate
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("Total Supply Rate".to_string()));
                chid_cell.children.push(HtmlChild::String(format!(
                    "{:.2} m³/s",
                    self.total_supply_rate
                )));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            table.children.push(HtmlChild::Element(body));
        }
        // Domain section
        {
            let mut head = HtmlElement::new("head".to_string());
            let mut head_row = HtmlElement::new("tr".to_string());
            let mut head_cell = HtmlElement::new("th".to_string());
            head_cell
                .children
                .push(HtmlChild::String("Domain".to_string()));
            head_row.children.push(HtmlChild::Element(head_cell));
            head.children.push(HtmlChild::Element(head_row));
            table.children.push(HtmlChild::Element(head));
            let mut body = HtmlElement::new("tbody".to_string());
            // # Meshes
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("# Meshes".to_string()));
                chid_cell
                    .children
                    .push(HtmlChild::String(format!("{}", self.n_meshes)));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // # Cells
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("# Cells".to_string()));
                chid_cell
                    .children
                    .push(HtmlChild::String(format!("{}", self.n_cells)));
                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            // Mesh Resolutions
            {
                let mut chid_row = HtmlElement::new("tr".to_string());
                let mut label_cell = HtmlElement::new("td".to_string());
                let mut chid_cell = HtmlElement::new("td".to_string());
                label_cell
                    .children
                    .push(HtmlChild::String("Mesh Resolutions".to_string()));

                let mut list = HtmlElement::new("ul".to_string());
                for mesh_resolution in &self.mesh_resolutions {
                    let mut li = HtmlElement::new("li".to_string());
                    li.children.push(HtmlChild::String(format!(
                        "{:.2}m × {:.2} m × {:.2} m",
                        mesh_resolution.x, mesh_resolution.y, mesh_resolution.z
                    )));
                    list.children.push(HtmlChild::Element(li));
                }
                chid_cell.children.push(HtmlChild::Element(list));

                chid_row.children.push(HtmlChild::Element(label_cell));
                chid_row.children.push(HtmlChild::Element(chid_cell));
                body.children.push(HtmlChild::Element(chid_row));
            }
            table.children.push(HtmlChild::Element(body));
        }
        table
    }
}

// impl InputSummary {
//     pub fn to_tree(&self) {
//         vec![ ("General",
//         vec![ ("CHID", InputSummaryMarkup $ chid)
//           , ("Simulation Time", InputSummaryMarkup $ format (show simulationInterval) <> " s")
//           ])
//         , ("Fire",
//         vec![ ("# Burners", InputSummaryMarkup $ show nBurners)
//           , ("Total Max. HRR", InputSummaryMarkup $  (format $ printf "%.2f" totalMaxHRR) <> " kW")
//           , ("Heat of Combustion", InputSummaryMarkup $ (format $ printf "%.2f" hoC) <> (" kJ/kg" :: String))
//           , ("Total Max. Soot Production Rate", InputSummaryMarkup $ printf "%.2e" sootRate <> (" kg/s" ::String))
//           ])
//         , ("Sprinklers",
//         vec![ ("# Sprinklers", InputSummaryMarkup $ show nSprinklers)
//           , ("Sprinkler Activation Temperatures", InputSummaryMarkup $ if nSprinklers == 0 then "N/A" else intercalate "," $ map (\x->show x <> " C") sprinklerActivationTemperatures) -- TODO: sort in order of proximity to fire.
//           ])
//         , ("Detection",
//         vec![ ("# Smoke Detectors", InputSummaryMarkup $ show nSmokeDetectors)
//           , ("Smoke Detector Obscurations", InputSummaryMarkup $ if nSmokeDetectors == 0 then "N/A" else  intercalate "," $ map (\x->show x <> " %Obs/m") smokeDetectorObscurations)
//           ])
//         , ("Ventilation",
//         vec![ ("# Exhaust Vents", InputSummaryMarkup $ show nExhausts)
//           , ("Total Exhaust Rate", InputSummaryMarkup $ (show exhaustRate) <> (" m^3/s" :: String))
//           , ("# Supply Vents", InputSummaryMarkup $ show nSupplies)
//           , ("Total Supply Rate", InputSummaryMarkup $ (show supplyRate) <> (" m^3/s" :: String))
//           ])
//         , ("Domain",
//         vec![ ("# Meshes", InputSummaryMarkup $ show nMeshes)
//           , ("# Cells", InputSummaryMarkup $ format $ show nCells)
//           , ("Mesh Resolutions", InputSummaryMarkup $ mconcat $ intersperse "\n" $ map htmlResolution resolutions)
//           , ("Non-Dimensionalised Ratio", InputSummaryMarkup $ mconcat $ intersperse "\n" $ map (printf "%.2f") ndrs)
//           ])
//         ]
//     }
// }
