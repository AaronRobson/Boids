module Boids
where

{-
https://processing.org/examples/flocking.html
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

pythagoras :: (Floating a) => [a] -> a
pythagoras = sqrt . sum . (map square)
  where
    square x = x * x

type Location = Vector
type Velocity = Vector
type Acceleration = Vector

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

isNeighbour :: Location -> Location -> Bool
isNeighbour x y = (<=5) $ distanceV x y

isNeighbourO :: Object -> Object -> Bool
isNeighbourO x y = isNeighbour (location x) (location y)

eachLocationWithNeighbours :: [Location] -> [(Location,[Location])]
eachLocationWithNeighbours = (map f) . eachItemWithRest
  where
    f :: (Location,[Location]) -> (Location,[Location])
    f (x,xs) = (x,neighbours)
      where
        neighbours = filter (isNeighbour x) xs

eachObjectWithNeighbours :: [Object] -> [(Object,[Object])]
eachObjectWithNeighbours = (map f) . eachItemWithRest
  where
    f :: (Object,[Object]) -> (Object,[Object])
    f (x,xs) = (x,neighbours)
      where
        neighbours = filter (isNeighbourO x) xs

meanV :: [Vector] -> Vector
meanV [] = Vector1d baseScalar
meanV xs = undefined

meanLocation :: [Object] -> Location
meanLocation = meanV . (map location)

meanVelocity :: [Object] -> Velocity
meanVelocity = meanV . (map velocity)

step :: Objects -> Objects
step xs = undefined
  where
    xNeighbours = eachObjectWithNeighbours xs

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
main = undefined
