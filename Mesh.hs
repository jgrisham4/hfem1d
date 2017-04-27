module Mesh
( Node
, Element
, Mesh
, generateMesh
, coordinates
, nodes
, computeJacobianDet
) where

-- Type for node
-- The first field is the global node number
-- The second field is a list of coordinates in n-dimensional space
data Node = Node {
  nodeNumber :: Int,
  coordinates :: [Double]
} deriving (Show)

-- Type for element - holds a list of nodes
newtype Element = Element {nodes :: [Node]} deriving (Show)

-- Type for mesh
newtype Mesh = Mesh {elements :: [Element]} deriving (Show)

-- Function for computing the Jacobian
computeJacobianDet :: Element -> Double
computeJacobianDet elem = (head (coordinates firstNode) - head (coordinates lastNode)) / 2.0
  where
    elemNodes = nodes elem
    firstNode = head elemNodes
    lastNode = last elemNodes


-- Function for getting node numbers for the given element
getNodeNumbers :: Element -> [Int]
getNodeNumbers elem = map nodeNumber $ nodes elem

-- Function for number of nodes in an element
numNodesElem :: Int -> Int
numNodesElem order = 1 + order

-- Function for number of nodes in a mesh
numNodesMesh :: Int -> Int -> Int
numNodesMesh nElem order = nElem * order + 1

-- Function for discretizing an interval
linspace :: Double -> Double -> Int -> [Double]
linspace xMin xMax nIncr = [xMin + fromIntegral i * dx | i <- [0..nIncr-1]]
  where
    dx = (xMax - xMin)/(fromIntegral nIncr - 1.0)

-- This is a function for generating connectivity info
generateConnectivity :: Int -> Int -> [[Int]]
generateConnectivity order nelem = [map (\ x -> x + (length localConnectivity - 1)*elemNum) localConnectivity | elemNum <- [0..(nelem-1)]]
  where
    localConnectivity = [0..order]

-- Function for generating a mesh
generateMesh :: Double -> Double -> Int -> Int -> Mesh
generateMesh xMin xMax nElem order = Mesh [Element [nodes !! i | i <- connectivity !! elemNum] | elemNum <- [0..(nElem-1)]]
  where
    nNodesMesh = numNodesMesh nElem order
    node_coords = linspace xMin xMax nNodesMesh
    nodes = [Node i [node_coords !! i] | i <- [0..length node_coords-1]]
    connectivity = generateConnectivity order nElem
