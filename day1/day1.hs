-- day 1
main = do
    modules <- readFile("./input.txt")
    let fuelForModules = map (\m -> (div m 3) - 2) (map (read :: String -> Integer) . lines $ modules)
    let totalFuel = sum $ zipWith (+) fuelForModules (map fuelception fuelForModules)
    print (totalFuel)

fuelception :: Integer -> Integer
fuelception fuel
    | (div fuel 3) - 2 <= 0 = 0
    | otherwise = ((div fuel 3) - 2) + fuelception(((div fuel 3) - 2))