module Solver
where

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix
import Mesh
import ShapeFcns

-- Function for getting Gauss points for integration
-- Given the number of points it returns a list of the points and the weights
getGaussPoints :: Int -> ([Double], [Double])
getGaussPoints 1 = ([0.0], [2.0])
getGaussPoints 2 = ([-0.577350269189626, -0.577350269189626], [1.0, 1.0])
getGaussPoints _ = error "Gauss points only up to 2 point quadrature available."

-- Definition of source term
sourceTerm :: Double -> Double
sourceTerm x = x * x

-- Function for interpolating a function using the linear basis
interp :: (Double -> Double) -> Mesh.Element -> Double -> Double
interp f elem xi = sum [f (nodalCoords !! i) * psi xi i | i <- [0..1]]
  where
    nodalCoords = map (head . coordinates) $ nodes elem

-- This function samples an element stiffness integrand
-- It returns a matrix which represents the contribution of the
-- present element to the global stiffness matrix
-- Only works for linear elements.  i and j should go from 0 to order
elemStiffnessIntegrand :: Mesh.Element -> Double -> Matrix Double
elemStiffnessIntegrand elem xi = fromLists [[psi xi i * psi xi j - dpsi xi i * dpsi xi j | i <- [0..1]] | j <- [0..1]]

-- This function samples the element load integrand
elemLoadIntegrand :: Mesh.Element -> Double -> Vector Double
elemLoadIntegrand elem xi = fromList [interp sourceTerm elem xi | i <- [0..1]]

-- This function integrates an element when provided with the element and the
-- number of Gauss points.
integrateElement :: Mesh.Element -> Int -> (Matrix Double, Vector Double)
integrateElement elem npts = (stiffnessMat, loadVec)
  where
    gaussData    = getGaussPoints npts
    gPoints      = map fst gaussData
    gWeights     = map snd gaussData
    detJ         = computeJacobianDet elem
    stiffnessMat = detJ * map (+) [(gWeights !! i) * elemStiffnessIntegrand elem (gPoints !! i) | i <- [0..(npts-1)]]
    loadVec      = detJ * map (+) [(gWeights !! i) * elemLoadIntegrand elem (gPoints !! i) | i <- [0..(npts-1)]]

