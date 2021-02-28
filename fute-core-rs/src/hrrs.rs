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
        let nfpaSlowAlpha: f64 = 1055.0 / 600_f64.powi(2);
        let nfpaMediumAlpha: f64 = 1055.0 / 300_f64.powi(2);
        let nfpaFastAlpha: f64 = 1055.0 / 150_f64.powi(2);
        let nfpaUltrafastAlpha: f64 = 1055.0 / 75_f64.powi(2);
        let eurocodeSlowAlpha: f64 = 1000.0 / 600_f64.powi(2);
        let eurocodeMediumAlpha: f64 = 1000.0 / 300_f64.powi(2);
        let eurocodeFastAlpha: f64 = 1000.0 / 150_f64.powi(2);
        let eurocodeUltrafastAlpha: f64 = 1000.0 / 75_f64.powi(2);

        match self {
            Self::NFPASlow => nfpaSlowAlpha,
            Self::NFPAMedium => nfpaMediumAlpha,
            Self::NFPAFast => nfpaFastAlpha,
            Self::NFPAUltrafast => nfpaUltrafastAlpha,
            Self::EurocodeSlow => eurocodeSlowAlpha,
            Self::EurocodeMedium => eurocodeMediumAlpha,
            Self::EurocodeFast => eurocodeFastAlpha,
            Self::EurocodeUltrafast => eurocodeUltrafastAlpha,
            Self::Custom(alpha) => *alpha,
        }
    }
    pub fn hrr(&self, time:f64) -> f64 {
        self.alpha()*time.powi(2)
    }
    pub fn hrr_capped(&self, cap_time: f64, time:f64) -> f64 {
        let time = if time <= cap_time {
            self.alpha()*time.powi(2)
        } else {
            cap_time
        };
        self.hrr(time)
    }
}

