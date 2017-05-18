module Solver
where

import Data.List
import System.IO
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
integrateElement :: Int -> Mesh.Element -> (Matrix Double, Vector Double)
integrateElement npts elem = (stiffnessMat, loadVec)
  where
    gaussData    = getGaussPoints npts
    gPoints      = fst gaussData
    gWeights     = snd gaussData
    detJ         = computeJacobianDet elem
    zmat         = fromLists [[0.0, 0.0], [0.0, 0.0]] :: Matrix Double
    zvec         = fromList [0.0, 0.0] :: Vector Double
    stiffnessMat = scalar detJ * foldl (+) zmat [scalar (gWeights !! i) * elemStiffnessIntegrand elem (gPoints !! i) | i <- [0..(npts-1)]]
    loadVec      = scalar detJ * foldl (+) zvec [scalar (gWeights !! i) * elemLoadIntegrand elem (gPoints !! i) | i <- [0..(npts-1)]]

-- This function takes an entry in a matrix or vector and sums duplicate entries
computeUnique :: (Ord t) => [(t, Double)] -> [(t, Double)]
computeUnique = map (foldr1 (\(coord, v1) (_, v2) -> (coord, v1 + v2))) . groupBy (\(a,_) (b,_) -> a == b) . sort

-- Function for assembling the linear system of equations
assembleLinearSystem :: [(Matrix Double, Vector Double)] -> Mesh -> (AssocMatrix, [(Int, Double)])
assembleLinearSystem elemData grid = (globalK, globalF)
  where
    elemCon   = map getNodeNumbers $ elements grid
    elemOrder = order grid
    nelem     = length $ elements grid
    elemK     = map fst elemData
    elemF     = map snd elemData
    globalK   = computeUnique $ concat [[(((elemCon !! en) !! i, (elemCon !! en) !! j), atIndex (elemK !! en) (i,j)) | i <- [0..elemOrder], j <- [0..elemOrder]] | en <- [0..(nelem - 1)]]
    globalF   = sortOn fst $ computeUnique $ concat [[(elemCon !! en !! i, atIndex (elemF !! en) i) | i <- [0..elemOrder]] | en <- [0..(nelem - 1)]]

applyBoundaryConditions :: (AssocMatrix, [(Int, Double)]) -> (GMatrix, Vector Double)
applyBoundaryConditions unconstData = (newMat, newVec)
  where
    unconstMat = fst unconstData
    unconstVec = snd unconstData
    numNodes = length unconstVec
    zeroedMat = filter (\ entry -> fst (fst entry) /= 0 && fst (fst entry) /= (numNodes-1)) unconstMat
    zeroedVec = filter(\ entry -> fst entry /= 0 && fst entry /= (numNodes-1)) unconstVec
    newMat = mkSparse $ ((0,0), 1.0) : ((numNodes-1,numNodes-1), 1.0) : zeroedMat
    newVec = fromList $ map snd $ sortOn fst $ (0, 0.0) : (numNodes-1, 0.0) : zeroedVec

-- Function for solving the problem
femSolve :: Mesh -> Int -> Vector Double
femSolve grid ngpts = cgSolve False cStiffnessMat cLoadVec
  where
    elemData = map (integrateElement ngpts) (elements grid)
    globalData = assembleLinearSystem elemData grid
    stiffnessMat = fst globalData
    loadVec = snd globalData
    constrainedData = applyBoundaryConditions (stiffnessMat, loadVec)
    cStiffnessMat = fst constrainedData
    cLoadVec = snd constrainedData

-- Function for writing solution to file for plotting
writeSolution :: String -> Mesh -> Vector Double -> IO()
writeSolution fileName grid soln = writeFile fileName fileStr
  where
    uniqueNodes = nub $ concatMap nodes $ elements grid
    nNodes = length uniqueNodes
    nodeCoords = map coordinates uniqueNodes
    lines = [show (head (nodeCoords !! nn)) ++ " " ++ show (atIndex soln nn) ++ "\n" | nn <- [0..(nNodes-1)]]
    fileStr = concat lines

-- fromList [((1,2),"hello")] :: Data.Map.Map (Int,Int) String
-- Strict map.
-- Data.Map.unionWith mappend (fromList [((1,2),"hello")]) (fromList [((1,2)," world")]) :: Data.Map.Map (Int,Int) String
