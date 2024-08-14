import Data.List.Split (chunksOf) --    cabal install --lib split

generateTuples n m = [(x, y) | y <- [0..m-1], x <- [0..n-1]]  -- create list of tuples like [(0,0),(1,0),(2,0),(0,1)...] to use as coordinates
--testlist = generateTuples 9 9
alphabet = ['a'..'z']
binary = [' ','█']                           
printStaggered list n = mapM_ (\x -> putStrLn x) (chunksOf n list)   --prints out a string but creating a new line every n character

normalize tuple n = (fst tuple - n/2, snd tuple - n/2)
normalizeMandel tuple x y = (fst tuple * 2.5 / y - 2, snd tuple * 2 / x - 1)  --scales and transforms coordinates to the bounds of the set TODO: zooms
iteration z c = (fst z * fst z - snd z * snd z + fst c, 2 * fst z * snd z + snd c)      -- complex square of z, add c
                                                      -- at first i missed one multiplication and it made it look like the map of yugoslavia
mandel z c 0 = 1
mandel z c itercount = if (getDistanceToZero (z) > 2)                
                       then 0                                        --the generating function of the set
                       else mandel (iteration z c) c (itercount-1)   

getDistanceToZero tuple = sqrt((fst tuple)**2 + (snd tuple)**2)   
circleSDF distance radius = fromEnum(distance>radius)
toBinary x = binary !! x
toBinaryInverted x = binary !! (1-x)

circle radius matsize = map (\x ->  toBinary (circleSDF (getDistanceToZero(normalize x matsize)) radius)) (generateTuples matsize matsize) --testing purposes circle
mandelbrot n m = map(\x -> toBinary (mandel (normalizeMandel x m n) (normalizeMandel x m n) 200))  (generateTuples n m)



main = do
   putStrLn "enter x"
   a <- getLine
   let x = read a :: Float
   putStrLn "enter y"
   b <- getLine
   let y = read b :: Float
   let xint = round(x) :: Int  --hacky but idrc
   printStaggered (mandelbrot x y) xint   -- todo: remake once i get to the io part