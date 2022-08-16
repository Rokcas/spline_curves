import Data.List
import Graphics.UI.GLUT
import System.Random  


createCurvePoints :: [Float] -> [Float]
createCurvePoints points =
  concat $ map (createCurve . createCurveOps . createCoefs) ppairs
  where
    ppairs = zip points $ tail points
    s0 = 0.6
    s1 = s0
    createCoefs (x0, x1) = 
      [ x0
      , s0
      , 3 * (x1 - x0) - 2 * s0 - s1
      , 2 * (x0 - x1) + s0 + s1 ]
    pows = [ (^x) | x <- [0..] ]
    createCurveOps coefs = zipWith ($) ((.) <$> (*) <$> coefs) (pows)
    createPoint ops = sum . zipWith ($) ops . repeat
    createCurve = flip map [0, 1 / 1000..1] . createPoint


createCurveVertices :: [(Float, Float)] -> [Vertex2 Float]
createCurveVertices points = zipWith Vertex2 x' y'
  where 
    (x, y) = unzip points
    x' = createCurvePoints x
    y' = createCurvePoints y


generateRandomInput :: IO [(Float, Float)]
generateRandomInput = do
  let pointCount = 8
      generateSequence = take (pointCount * 2) . randomRs (-1, 1)
      splitZip points = zip x y
        where (x, y) = splitAt pointCount points
      generatePoints = splitZip . generateSequence
  gen <- getStdGen
  return $ generatePoints gen


readFileInput :: String -> IO [(Float, Float)]
readFileInput filename = do
  contents <- readFile filename
  return $ read contents


getInput :: [String] -> IO [(Float, Float)]
getInput [] = generateRandomInput
getInput [filename] = readFileInput filename


main :: IO ()
main = do
  (_, args) <- getArgsAndInitialize
  input <- getInput args
  let vertices = createCurveVertices input
  _window <- createWindow "Spline Curves"
  displayCallback $= (display vertices)
  mainLoop

display :: [Vertex2 Float] -> DisplayCallback
display vertices = do 
  clear [ColorBuffer]
  renderPrimitive Points $ mapM_ vertex vertices
  flush