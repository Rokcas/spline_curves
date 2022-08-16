import Data.List
import Graphics.UI.GLUT
import System.Random  


-- myPoints :: [(GLfloat,GLfloat,GLfloat)]
-- myPointsX = [ -0.8, -0.2, -0.4, 0.3, 0.8 ]
-- myPointsY = [ 0.8, -0.4, 0.9, 0.7, 0.5 ]

myPointsX = map ((subtract 1) . (2*)) [0.057620374653786, 0.15205970107439015, 0.5340820989492587, 0.5770928331542042, 0.25906285457742695, 0.08817991973941897, 0.41895571880020166, 0.16954369735990038]
myPointsY = map ((subtract 1) . (2*)) [0.6088092386417162, 0.9386682279120688, 0.727328484136289, 0.11353366158502609, 0.4129862685725345, 0.1605336905018283, 0.1448934803792601, 0.27571229269161224]


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


createCurveVertices :: [(Float, Float)] -> [Vertex3 Float]
createCurveVertices points = zipWith createVertex x' y'
  where 
    (x, y) = unzip points
    x' = createCurvePoints x
    y' = createCurvePoints y
    createVertex x y = Vertex3 x y 0


generateRandomInput :: IO [(Float, Float)]
generateRandomInput = do
  let pointCount = 8
      generateSequence = take (pointCount * 2) . randomRs (-1, 1)
      splitZip points = zip x y
        where (x, y) = splitAt pointCount points
      generatePoints = splitZip . generateSequence
  gen <- getStdGen
  return $ generatePoints gen


-- readFileInput :: IO [(Float, Float)]
-- readFileInput = do



getInput :: [String] -> IO [(Float, Float)]
getInput [] = generateRandomInput
-- getInput [filename] = 


main :: IO ()
main = do
  (_, args) <- getArgsAndInitialize
  input <- getInput args
  let vertices = createCurveVertices input
  _window <- createWindow "Spline Curves"
  displayCallback $= (display vertices)
  mainLoop

display :: [Vertex3 Float] -> DisplayCallback
display vertices = do 
  clear [ColorBuffer]
  renderPrimitive Points $ mapM_ vertex vertices
  flush