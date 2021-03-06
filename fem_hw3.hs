{-|

 This code uses the Galerkin finite element method to solve
 the following differential equation:
 -u''(x) + u - x^2 = 0
 over the domain 0 <= x <= 1, subject to u(0) = u(1) = 0.

 Author: James Grisham
 Date: 04/05/2017

|-}

import Numeric.LinearAlgebra.Data
import Mesh
import Solver

main = do
  let ngpts = 3
  let grid = generateMesh 0.0 1.0 50 1
  let soln = femSolve grid ngpts
  writeSolution "linear.dat" grid soln
