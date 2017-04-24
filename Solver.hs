module Solver
where

import Numeric.LinearAlgebra.Data (Matrix,matrix)
import Mesh (Mesh, Element)
import ShapeFcns (psi, dpsi)

-- Function for getting Gauss points for integration
-- Given the number of points it returns a list of the points and the weights
getGaussPoints :: Int -> ([Double], [Double])
getGaussPoints 1 = ([0.0], [2.0])
getGaussPoints 2 = ([-0.577350269189626, -0.577350269189626], [1.0, 1.0])
getGaussPoints _ = error "Gauss points only up to 2 point quadrature available."

-- Function for interpolating source term
--interp :: (Double -> Double) -> Element -> Double

-- This function samples an element stiffness integrand
-- It returns a matrix which represents the contribution of the
-- present element to the global stiffness matrix
elemStiffnessIntegrand :: Element -> Double -> Matrix
elemStiffnessIntegrand elem xi = [[i | i <- [0..1]]]

integrateElement :: Element -> Matrix

addElemContribution :: Matrix -> Element -> Matrix
