module FDSUtilities.Types.Fire where

-- |Growth rates for fire curves.
data GrowthRate = NFPASlow | NFPAFast | NFPAMedium | NFPAUltrafast | EurocodeSlow | EurocodeMedium | EurocodeFast | EurocodeUltrafast | Custom Double deriving (Eq, Ord, Show, Read)

-- |Extract the raw alpha value from @GrowthRate@.
growthRateToAlpha :: GrowthRate -> Double
growthRateToAlpha x = case x of
    NFPASlow            -> nfpaSlowAlpha
    NFPAMedium          -> nfpaMediumAlpha
    NFPAFast            -> nfpaFastAlpha
    NFPAUltrafast       -> nfpaUltrafastAlpha
    EurocodeSlow        -> eurocodeSlowAlpha
    EurocodeMedium      -> eurocodeMediumAlpha
    EurocodeFast        -> eurocodeFastAlpha
    EurocodeUltrafast   -> eurocodeUltrafastAlpha
    Custom alpha        -> alpha

-- alpha2GrowthRate :: Double -> GrowthRate
-- alpha2GrowthRate x = case x of
    -- NFPASlow            -> NFPASlow
    -- NFPAMedium          -> NFPAMedium
    -- NFPAFast            -> NFPAFast
    -- NFPAUltrafast       -> NFPAUltrafast
    -- EurocodeSlow        -> EurocodeSlow
    -- EurocodeMedium      -> EurocodeMedium
    -- EurocodeFast        -> EurocodeFast
    -- EurocodeUltrafast   -> EurocodeUltrafast
    -- Custom alpha        -> Custom alpha

nfpaSlowAlpha :: Double
nfpaSlowAlpha           = 1055/600**2

nfpaMediumAlpha :: Double
nfpaMediumAlpha         = 1055/300**2

nfpaFastAlpha :: Double
nfpaFastAlpha           = 1055/150**2

nfpaUltrafastAlpha :: Double
nfpaUltrafastAlpha      = 1055/75**2

eurocodeSlowAlpha :: Double
eurocodeSlowAlpha       = 1000/600**2

eurocodeMediumAlpha :: Double
eurocodeMediumAlpha     = 1000/300**2

eurocodeFastAlpha :: Double
eurocodeFastAlpha       = 1000/150**2

eurocodeUltrafastAlpha :: Double
eurocodeUltrafastAlpha  = 1000/75**2

hrr :: GrowthRate -> Double -> Double
hrr ramp time = alpha*time**2
    where
        alpha = growthRateToAlpha ramp

hrrCapped :: GrowthRate -> Double -> Double -> Double
hrrCapped ramp cappedTime time | time <= cappedTime = hrr ramp time
                               | otherwise = hrr ramp cappedTime
