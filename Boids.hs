module Boids
where

{-
https://processing.org/examples/flocking.html
http://www.red3d.com/cwr/boids/
-}

import Data.List
import Data.Maybe

type Scalar = Double

baseScalar :: Scalar
baseScalar = 0

data Vector = Vector1d Scalar
            | Vector2d Scalar Scalar
            | Vector3d Scalar Scalar Scalar
    deriving (Show, Eq)

type Vectors = [Vector]

x :: Vector -> Scalar
x (Vector1d x) = x
x (Vector2d x _) = x
x (Vector3d x _ _) = x

y :: Vector -> Scalar
y (Vector2d _ y) = y
y (Vector3d _ y _) = y

z :: Vector -> Scalar
z (Vector3d _ _ z) = z

vectorToList :: Vector -> [Scalar]
vectorToList (Vector1d x) = [x]
vectorToList (Vector2d x y) = [x,y]
vectorToList (Vector3d x y z) = [x,y,z]

listToVector :: [Scalar] -> Vector
listToVector [x] = (Vector1d x)
listToVector [x,y] = (Vector2d x y)
listToVector [x,y,z] = (Vector3d x y z)

makeSameDimensionPair :: (Vector,Vector) -> (Vector,Vector)
makeSameDimensionPair (v1,v2) = (v1',v2')
  where
    z = zipWithPadding baseScalar baseScalar (vectorToList v1) (vectorToList v2)
    v1' = listToVector $ map fst z
    v2' = listToVector $ map snd z

makeSameDimension :: [Vector] -> [Vector]
makeSameDimension [] = []
makeSameDimension [v] = [v]
makeSameDimension [v1,v2] = [v1',v2']
  where
    (v1',v2') = makeSameDimensionPair (v1,v2)
makeSameDimension (v1:v2:vs) = v1':(makeSameDimension (v2:vs))
  where
    [v1',v2'] = makeSameDimension [v1,v2]

-- http://stackoverflow.com/questions/22403029/how-to-zip-lists-with-different-length
zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ [] ys = zip (repeat a) ys
zipWithPadding _ b xs [] = zip xs (repeat b)

applyBinaryOperator :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
applyBinaryOperator f b b' xs ys = map (uncurry f) $ zipWithPadding b b' xs ys

applyUnaryVectorOperator :: (Scalar -> Scalar) -> Vector -> Vector
applyUnaryVectorOperator f v = listToVector . (map f) $ vectorToList v

applyBinaryVectorOperator :: (Scalar -> Scalar -> Scalar) -> Scalar -> Scalar -> Vector -> Vector -> Vector
applyBinaryVectorOperator f b b' v v' = listToVector $ applyBinaryOperator f b' b' (vectorToList v) (vectorToList v')

instance Num Vector where
    v + v' = applyBinaryVectorOperator (+) 0 0 v v'
    v * v' = applyBinaryVectorOperator (*) 1 1 v v'
    abs v = applyUnaryVectorOperator abs v
    signum v = applyUnaryVectorOperator signum v
    fromInteger i = Vector1d $ fromIntegral i
    negate v = applyUnaryVectorOperator negate v

instance Fractional Vector where
    fromRational r = Vector1d $ fromRational r
    recip v = applyUnaryVectorOperator recip v

pythagoras :: (Floating a) => [a] -> a
pythagoras = sqrt . sum . (map square)
  where
    square x = x * x

type Location = Vector
type Locations = [Location]
type Velocity = Vector
type Velocities = [Velocity]
type Acceleration = Vector
type Accelerations = [Acceleration]

lengthV :: Vector -> Scalar
lengthV = pythagoras . vectorToList

distanceV :: Vector -> Vector -> Scalar
distanceV v v' = lengthV $ v - v'

data Object = Object
  { location :: Location
  , velocity :: Velocity
  } deriving (Show, Eq)

type Objects = [Object]

stepO o = Object {location = l', velocity = v'}
  where
    l = location o
    v = velocity o
    l' = l + v
    v' = v

applyAcceleration :: Object -> Acceleration -> Object
applyAcceleration o a = Object {location = l', velocity = v'}
    where
      v = velocity o
      v' = v + a
      l = location o
      l' = l + v'

applyAccelerations :: [(Object,Acceleration)] -> Objects
applyAccelerations = map (uncurry applyAcceleration)

nthItemRemoved :: [a] -> Integer -> [a]
nthItemRemoved xs i = map snd $ filter ((/=i) . fst) $ enumerate xs 

nthItemWithRest :: [a] -> Integer -> Maybe (a,[a])
nthItemWithRest [] _ = Nothing
nthItemWithRest xs i = Just $ (genericIndex xs i,nthItemRemoved xs i)

enumerate :: [a] -> [(Integer,a)]
enumerate xs = enumerateChooseStart xs 0

enumerateChooseStart :: [a] -> Integer -> [(Integer,a)]
enumerateChooseStart xs i = (zip [i..]) xs

indexes :: [a] -> [Integer]
indexes = (map fst) . enumerate

eachItemWithRest :: [a] -> [(a,[a])]
eachItemWithRest xs = catMaybes . (map (nthItemWithRest xs)) $ indexes xs

isNeighbour :: Scalar -> Location -> Location -> Bool
isNeighbour neighbourhood x y = (<=neighbourhood) $ distanceV x y

isNeighbourO :: Scalar -> Object -> Object -> Bool
isNeighbourO neighbourhood x y = isNeighbour neighbourhood (location x) (location y)

objectWithNeighbours :: Scalar -> (Object,[Object]) -> (Object,[Object])
objectWithNeighbours neighbourhood (x,xs) = (x,neighbours)
    where
      neighbours = filter (isNeighbourO neighbourhood x) xs

eachObjectWithNeighbours :: Scalar -> [Object] -> [(Object,[Object])]
eachObjectWithNeighbours neighbourhood = (map (objectWithNeighbours neighbourhood)) . eachItemWithRest

meanV :: [Vector] -> Vector
meanV [] = Vector1d baseScalar
meanV xs = (sum xs) / (genericLength xs)

meanLocation :: [Object] -> Location
meanLocation = meanV . (map location)

meanVelocity :: [Object] -> Velocity
meanVelocity = meanV . (map velocity)

calculateAcceleration :: Scalar -> (Object,[Object]) -> Acceleration
calculateAcceleration neighbourhood (x,xs) = separationFactor + locationFactor + velocityFactor
  where
    neighbours :: [Object]
    neighbours = snd $ objectWithNeighbours neighbourhood (x,xs)
    othersWhichAreTooClose :: [Object]
    othersWhichAreTooClose = snd $ objectWithNeighbours (neighbourhood / 4) (x,xs)
    separationFactor = sum $ (map (subtract (location x))) . (map location) $ othersWhichAreTooClose
    locationFactor = ((location x) - (meanLocation neighbours)) / 100
    velocityFactor = ((velocity x) - (meanVelocity neighbours)) / 100

calculateAccelerations :: Scalar -> Objects -> Accelerations
calculateAccelerations neighbourhood = (map (calculateAcceleration neighbourhood)) . eachItemWithRest

step :: Scalar -> Objects -> Objects
step neighbourhood xs = applyAccelerations $ zip xs $ calculateAccelerations neighbourhood xs

defaultNumberOfDimensions = 2

type Boid = Object
type Boids = [Boid]
{-
applyRule :: Boids -> Boids
type Rules = [Rule]
-}
-- https://wiki.haskell.org/Compose
compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v
{-
applyRules :: [(Boids -> Boids)] -> Boids -> Boids
applyRules = compose
-}
main :: IO ()
main = do
    putStrLn "Boids Flocking Simulator"
    putStrLn "...is under construction, please check back later."
