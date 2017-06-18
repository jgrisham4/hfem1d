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
getGaussPoints 2 = ([-0.577350269189626, 0.577350269189626], [1.0, 1.0])
getGaussPoints 3 = ([-0.774596669241483, 0.0, 0.774596669241483], [0.555555555555555, 0.888888888888889, 0.555555555555555])
getGaussPoints _ = error "Gauss points only up to 3 point quadrature available."

-- Definition of source term
sourceTerm :: Double -> Double
sourceTerm x = x * x

-- Function for interpolating a function using the basis
interp :: (Double -> Double) -> Mesh.Element -> Double -> Double
interp f elem xi = sum [sourceTerm (sum [nodalCoords !! j * psi xi j | j <- [0..(order elem)]]) * psi xi i | i <- [0..(order elem)]]
  where
    nodalCoords = map (head . coordinates) $ nodes elem

-- This function samples an element stiffness integrand
-- It returns a matrix which represents the contribution of the
-- present element to the global stiffness matrix
elemStiffnessIntegrand :: Mesh.Element -> Double -> Matrix Double
elemStiffnessIntegrand elem xi = fromLists [[detJ * psi xi i * psi xi j + 1.0/detJ * dpsi xi i * dpsi xi j | i <- [0..(order elem)]] | j <- [0..(order elem)]]
  where
    detJ = computeJacobianDet elem

-- This function samples the element load integrand
elemLoadIntegrand :: Mesh.Element -> Double -> Vector Double
--elemLoadIntegrand elem xi = scalar (interp sourceTerm elem xi * detJ) * fromList [psi xi j | j <- [0..(order elem)]]
elemLoadIntegrand elem xi = scalar detJ * fromList [sourceTerm (sum [nodalCoords !! j * psi xi j | j <- [0..(order elem)]]) * psi xi j | j <- [0..(order elem)]]
  where
    nodalCoords = map (head . coordinates) $ nodes elem
    detJ = computeJacobianDet elem

-- This function integrates an element when provided with the element and the
-- number of Gauss points.
integrateElement :: Int -> Mesh.Element -> (Matrix Double, Vector Double)
integrateElement npts elem = (stiffnessMat, loadVec)
  where
    gaussData    = getGaussPoints npts
    gPoints      = fst gaussData
    gWeights     = snd gaussData
    zmat         = fromLists [[0.0, 0.0], [0.0, 0.0]] :: Matrix Double
    zvec         = fromList [0.0, 0.0] :: Vector Double
    stiffnessMat = foldl' (+) zmat [scalar (gWeights !! i) * elemStiffnessIntegrand elem (gPoints !! i) | i <- [0..(npts-1)]]
    loadVec      = foldl' (+) zvec [scalar (gWeights !! i) * elemLoadIntegrand elem (gPoints !! i) | i <- [0..(npts-1)]]

-- This function takes an entry in a matrix or vector and sums duplicate entries
computeUnique :: (Ord t) => [(t, Double)] -> [(t, Double)]
computeUnique = map (foldr1 (\(coord, v1) (_, v2) -> (coord, v1 + v2))) . groupBy (\(a,_) (b,_) -> a == b) . sort

-- Function for assembling the linear system of equations
assembleLinearSystem :: [(Matrix Double, Vector Double)] -> Mesh -> (AssocMatrix, [(Int, Double)])
assembleLinearSystem elemData grid = (globalK, globalF)
  where
    elems     = elements grid
    elemCon   = map getNodeNumbers elems
    nelem     = length $ elements grid
    elemK     = map fst elemData
    elemF     = map snd elemData
    globalK   = computeUnique $ concat [[(((elemCon !! en) !! i, (elemCon !! en) !! j), atIndex (elemK !! en) (i,j)) | i <- [0..(order $ elems !! en)], j <- [0..(order $ elems !! en)]] | en <- [0..(nelem - 1)]]
    globalF   = sortOn fst $ computeUnique $ concat [[(elemCon !! en !! i, atIndex (elemF !! en) i) | i <- [0..(order $ elems !! en)]] | en <- [0..(nelem - 1)]]

--applyBoundaryConditions :: (AssocMatrix, [(Int, Double)]) -> (GMatrix, Vector Double)
applyBoundaryConditions :: (AssocMatrix, [(Int, Double)]) -> (Matrix Double, Vector Double)
applyBoundaryConditions unconstData = (newMat, newVec)
  where
    unconstMat = fst unconstData
    unconstVec = snd unconstData
    numNodes   = length unconstVec
    zeroedMat  = filter (\ entry -> fst (fst entry) /= 0 && fst (fst entry) /= (numNodes-1)) unconstMat
    zeroedVec  = filter(\ entry -> fst entry /= 0 && fst entry /= (numNodes-1)) unconstVec
    newMat     = toDense $ ((0, 0), 1.0) : ((numNodes-1,numNodes-1), 1.0) : zeroedMat
    newVec     = fromList $ map snd $ sortOn fst $ (0, 0.0) : (numNodes-1, 0.0) : zeroedVec

-- Function for solving the problem
femSolve :: Mesh -> Int -> Vector Double
--femSolve grid ngpts = cgSolve True cStiffnessMat cLoadVec
femSolve grid ngpts = cStiffnessMat <\> cLoadVec
  where
    elemData        = map (integrateElement ngpts) (elements grid)
    globalData      = assembleLinearSystem elemData grid
    stiffnessMat    = fst globalData
    loadVec         = snd globalData
    constrainedData = applyBoundaryConditions (stiffnessMat, loadVec)
    cStiffnessMat   = fst constrainedData
    cLoadVec        = snd constrainedData

-- Function for writing solution to file for plotting
writeSolution :: String -> Mesh -> Vector Double -> IO()
writeSolution fileName grid soln = writeFile fileName fileStr
  where
    uniqueNodes = nub $ concatMap nodes $ elements grid
    nNodes      = length uniqueNodes
    nodeCoords  = map coordinates uniqueNodes
    lines       = [show (head (nodeCoords !! nn)) ++ " " ++ show (atIndex soln nn) ++ "\n" | nn <- [0..(nNodes-1)]]
    fileStr     = concat lines
