{-|

 This code uses the Galerkin finite element method to solve
 the following differential equation:
 -u''(x) + u - x^2 = 0
 over the domain 0 <= x <= 1, subject to u(0) = u(1) = 0.

 Author: James Grisham
 Date: 04/05/2017

|-}

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix
import Mesh
import Solver

main = do
  let ngpts = 3
  let grid = generateMesh 0.0 1.0 5 1
  let elemData = map (integrateElement ngpts) (elements grid)
  print $ fst $ elemData !! 1
  --let globalData = assembleLinearSystem elemData grid
  --let stiffnessMat = fst globalData
  --let loadVec = snd globalData
  --saveMatrix "stiffness.dat" "%.4f" $ toDense stiffnessMat
