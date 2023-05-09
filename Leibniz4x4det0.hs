import Data.List (permutations)

determinant_leibniz :: [[Int]] -> Int
determinant_leibniz matrix =
  if length matrix == length (head matrix)
    then det_leibniz (permutations [0..n-1]) matrix
    else error "Matriz não é quadrada"
  where
    n = length matrix
    det_leibniz [] _ = 0
    det_leibniz (perm:perms) mat = ((-1) ^ sgn) * prod + det_leibniz perms mat
      where
        sgn = sum [if perm !! j > perm !! i then 1 else 0 | i <- [0..n-2], j <- [i+1..n-1]]
        prod = product [mat !! i !! (perm !! i) | i <- [0..n-1]]

matrix = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]
main = print (determinant_leibniz matrix)