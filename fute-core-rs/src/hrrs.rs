/// Growth rates for fire curves.// TODO: custom ord
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum GrowthRate {
    NFPASlow,
    NFPAFast,
    NFPAMedium,
    NFPAUltrafast,
    EurocodeSlow,
    EurocodeMedium,
    EurocodeFast,
    EurocodeUltrafast,
    Custom(f64),
}

impl GrowthRate {
    pub fn alpha(&self) -> f64 {
        // TODO: make these const when available.
        let nfpa_slow_alpha: f64 = 1055.0 / 600_f64.powi(2);
        let nfpa_medium_alpha: f64 = 1055.0 / 300_f64.powi(2);
        let nfpa_fast_alpha: f64 = 1055.0 / 150_f64.powi(2);
        let nfpa_ultrafast_alpha: f64 = 1055.0 / 75_f64.powi(2);
        let eurocode_slow_alpha: f64 = 1000.0 / 600_f64.powi(2);
        let eurocode_medium_alpha: f64 = 1000.0 / 300_f64.powi(2);
        let eurocode_fast_alpha: f64 = 1000.0 / 150_f64.powi(2);
        let eurocode_ultrafast_alpha: f64 = 1000.0 / 75_f64.powi(2);

        match self {
            Self::NFPASlow => nfpa_slow_alpha,
            Self::NFPAMedium => nfpa_medium_alpha,
            Self::NFPAFast => nfpa_fast_alpha,
            Self::NFPAUltrafast => nfpa_ultrafast_alpha,
            Self::EurocodeSlow => eurocode_slow_alpha,
            Self::EurocodeMedium => eurocode_medium_alpha,
            Self::EurocodeFast => eurocode_fast_alpha,
            Self::EurocodeUltrafast => eurocode_ultrafast_alpha,
            Self::Custom(alpha) => *alpha,
        }
    }
    pub fn hrr(&self, time: f64) -> f64 {
        self.alpha() * time.powi(2)
    }
    pub fn hrr_capped(&self, cap_time: f64, time: f64) -> f64 {
        let time = if time <= cap_time {
            self.alpha() * time.powi(2)
        } else {
            cap_time
        };
        self.hrr(time)
    }
}
