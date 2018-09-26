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
    
nfpaSlowAlpha           = 1055/600**2
nfpaMediumAlpha         = 1055/300**2
nfpaFastAlpha           = 1055/150**2
nfpaUltrafastAlpha      = 1055/75**2
eurocodeSlowAlpha       = 1000/600**2
eurocodeMediumAlpha     = 1000/300**2
eurocodeFastAlpha       = 1000/150**2
eurocodeUltrafastAlpha  = 1000/75**2

hrr ramp time = alpha*time**2
    where
        alpha = growthRateToAlpha ramp

hrrCapped ramp cappedTime time | time <= cappedTime = hrr ramp time
                               | otherwise = hrr ramp cappedTime