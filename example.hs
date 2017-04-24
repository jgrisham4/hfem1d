-- I need something like [[0, 1], [1, 2], [2, 3], ... , [n-2, n-1]] for linear.
-- I need something like [[0, 1, 2], [2, 3, 4], [4, 5, 6], ... , [n-3, n-2, n-1]] for quadratic.

generateConnectivity :: Int -> Int -> [[Int]]
generateConnectivity order nelem = [map (\ x -> x + (length localConnectivity - 1)*elemNum) localConnectivity | elemNum <- [0..(nelem-1)]]
  where
    localConnectivity = [0..order]
