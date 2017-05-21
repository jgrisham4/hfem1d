module ShapeFcns
( psi
, dpsi
) where

-- Function for the basis (only linear)
psi :: Double -> Int -> Double
psi xi i | i == 0 = 0.5*(1.0 - xi)
         | i == 1 = 0.5*(1.0 + xi)
         | otherwise = error "Input i must be 0 or 1."

-- Function for derivative of the basis
dpsi :: Double -> Int -> Double
dpsi xi i | i == 0 = -0.5
          | i == 1 = 0.5
          | otherwise = error "Input i must be 0 or 1."
