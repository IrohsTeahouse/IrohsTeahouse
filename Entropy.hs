import Numeric (logBase)
calcularEntropia :: [Double] -> Double
calcularEntropia probabilidades = negate $ sum $ map calcularTermo probabilidades
  where
    calcularTermo probabilidade
      | probabilidade /= 0 = probabilidade * logBase 2 probabilidade
      | otherwise = 0
calcularEntropiaMaxima :: Int -> Double
calcularEntropiaMaxima numClasses = logBase 2 (fromIntegral numClasses)
probabilidades :: [Double]
probabilidades = [0.032258, 0.016129, 0.016129, 0.048387, 0.129032, 0.306452, 0.290323, 0.161290]

main :: IO ()
main = do
  let entropia = calcularEntropia probabilidades
  putStrLn $ "Entropia: " ++ show entropia
  let entropiaMaxima = calcularEntropiaMaxima (length probabilidades)
  putStrLn $ "Entropia máxima: " ++ show entropiaMaxima
