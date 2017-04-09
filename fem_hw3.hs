{-|

 This code uses the Galerkin finite element method to solve
 the following differential equation:
 -u''(x) + u - x^2 = 0
 over the domain 0 <= x <= 1, subject to u(0) = u(1) = 0.

 Author: James Grisham
 Date: 04/05/2017

|-}

-- Type for node
-- The first field is the global node number
-- The second field is a list of coordinates in n-dimensional space
data Node = Node {
  nodeNumber :: Int,
  coordinates :: [Double]
} deriving (Show)

-- Type for element - holds a list of nodes
newtype Element = Element {nodes :: [Node]} deriving (Show)

-- Function for getting node numbers for the given element
getNodeNumbers :: Element -> [Int]
getNodeNumbers elem = map nodeNumber $ nodes elem

-- Function for the basis (only linear)
psi :: Double -> Int -> Double
psi xi i | i == 0 = 0.5*(1.0 - 1.0*xi)
         | i == 1 = 0.5*(1.0 + xi)
         | otherwise = error "Input i must be 0 or 1."

-- Function for derivative of the basis
dpsi :: Double -> Int -> Double
dpsi xi i | i == 0 = -0.5
          | i == 1 = 0.5
          | otherwise = error "Input i must be 0 or 1."

-- Function for getting Gauss points for integration
-- Given the number of points it returns a list of the points and the weights
getGaussPoints :: Int -> ([Double], [Double])
getGaussPoints 1 = ([0.0], [2.0])
getGaussPoints 2 = ([-0.577350269189626, -0.577350269189626], [1.0, 1.0])
getGaussPoints _ = error "Gauss points only up to 2 point quadrature available."

-- Function for number of nodes in a mesh
--numNodesMesh :: Int -> Int -> Int
--numNodesMesh nElem Order = 

-- Function for generating a mesh
--generateMesh :: Double -> Double -> Int -> Int -> [Element]
--generateMesh xMin xMax nElem order = 

