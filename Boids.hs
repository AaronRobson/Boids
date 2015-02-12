module Boids
where

{-
https://processing.org/examples/flocking.html
-}

import Data.List
import Data.Maybe

type Scalar = Double

data Vector1d = Vector1d
  { x1d :: Scalar
  } deriving (Show, Eq)

data Vector2d = Vector2d
  { x2d :: Scalar
  , y2d :: Scalar
  } deriving (Show, Eq)

data Vector3d = Vector3d
  { x3d :: Scalar
  , y3d :: Scalar
  , z3d :: Scalar
  } deriving (Show, Eq)

instance Num Vector1d where
    (Vector1d x) + (Vector1d x') = Vector1d (x+x')
    (Vector1d x) * (Vector1d x') = Vector1d (x*x')
    abs (Vector1d x) = Vector1d (abs x)
    signum (Vector1d x) = Vector1d (signum x)
    fromInteger i = Vector1d (fromInteger i)
    negate (Vector1d x) = Vector1d (negate x)

instance Num Vector2d where
    (Vector2d x y) + (Vector2d x' y') = Vector2d (x+x') (y+y')
    (Vector2d x y) * (Vector2d x' y') = Vector2d (x*x') (y*y')
    abs (Vector2d x y) = Vector2d (abs x) (abs y)
    signum (Vector2d x y) = Vector2d (signum x) (signum y)
    fromInteger i = Vector2d (fromInteger i) (fromInteger i)
    negate (Vector2d x y) = Vector2d (negate x) (negate y)

instance Num Vector3d where
    (Vector3d x y z) + (Vector3d x' y' z') = Vector3d (x+x') (y+y') (z+z')
    (Vector3d x y z) * (Vector3d x' y' z') = Vector3d (x*x') (y*y') (z*z')
    abs (Vector3d x y z) = Vector3d (abs x) (abs y) (abs z)
    signum (Vector3d x y z) = Vector3d (signum x) (signum y) (signum z)
    fromInteger i = Vector3d (fromInteger i) (fromInteger i) (fromInteger i)
    negate (Vector3d x y z) = Vector3d (negate x) (negate y) (negate z)

class X a where
    x :: a -> Scalar

instance X Vector1d where
    x = x1d

instance X Vector2d where
    x = x2d

instance X Vector3d where
    x = x3d

class Y a where
    y :: a -> Scalar

instance Y Vector2d where
    y = y2d

instance Y Vector3d where
    y = y3d

class Z a where
    z :: a -> Scalar

instance Z Vector3d where
    z = z3d

baseScalar :: Scalar
baseScalar = 0

baseVector1d :: Vector1d
baseVector1d = Vector1d baseScalar

baseVector2d :: Vector2d
baseVector2d = Vector2d baseScalar baseScalar

baseVector3d :: Vector3d
baseVector3d = Vector3d baseScalar baseScalar baseScalar

pythagoras :: (Floating a) => [a] -> a
pythagoras = sqrt . sum . (map square)
  where
    square x = x * x

class Vector v where
    vectorElements :: v -> [Scalar]
    lengthV :: v -> Scalar
    distanceV :: v -> v -> Scalar

instance Vector Vector1d where
    vectorElements v = [x v]
    lengthV = pythagoras . vectorElements
    distanceV v1 v2 = lengthV $ v1 - v2

instance Vector Vector2d where
    vectorElements v = [x v, y v]
    lengthV = pythagoras . vectorElements
    distanceV v1 v2 = lengthV $ v1 - v2

instance Vector Vector3d where
    vectorElements v = [x v, y v, z v]
    lengthV = pythagoras . vectorElements
    distanceV v1 v2 = lengthV $ v1 - v2

type Location1d = Vector1d
type Location2d = Vector2d
type Location3d = Vector3d

type Velocity1d = Vector1d
type Velocity2d = Vector2d
type Velocity3d = Vector3d

type Acceleration1d = Vector1d
type Acceleration2d = Vector2d
type Acceleration3d = Vector3d

data Object1d = Object1d
  { location1d :: Location1d
  , velocity1d :: Vector1d
  } deriving (Show, Eq)

data Object2d = Object2d
  { location2d :: Location2d
  , velocity2d :: Vector2d
  } deriving (Show, Eq)

data Object3d = Object3d
  { location3d :: Location3d
  , velocity3d :: Vector3d
  } deriving (Show, Eq)

class Object o where
    stepO :: o -> o

instance Object Object1d where
    stepO o = Object1d {location1d = l', velocity1d = v'}
      where
        l = location1d o
        v = velocity1d o
        l' = l + v
        v' = v

instance Object Object2d where
    stepO o = Object2d {location2d = l', velocity2d = v'}
      where
        l = location2d o
        v = velocity2d o
        l' = l + v
        v' = v

instance Object Object3d where
    stepO o = Object3d {location3d = l', velocity3d = v'}
      where
        l = location3d o
        v = velocity3d o
        l' = l + v
        v' = v

applyAcceleration1d :: Object1d -> Acceleration1d -> Object1d
applyAcceleration1d o a = Object1d {location1d = l', velocity1d = v'}
    where
      v = velocity1d o
      v' = v + a
      l = location1d o
      l' = l + v'

applyAcceleration2d :: Object2d -> Acceleration2d -> Object2d
applyAcceleration2d o a = Object2d {location2d = l', velocity2d = v'}
    where
      v = velocity2d o
      v' = v + a
      l = location2d o
      l' = l + v'

applyAcceleration3d :: Object3d -> Acceleration3d -> Object3d
applyAcceleration3d o a = Object3d {location3d = l', velocity3d = v'}
    where
      v = velocity3d o
      v' = v + a
      l = location3d o
      l' = l + v'

nthItemRemoved :: [a] -> Integer -> [a]
nthItemRemoved xs i = map snd $ filter ((/=i) . fst) $ enumerate xs 

nthItemWithRest :: [a] -> Integer -> Maybe (a,[a])
nthItemWithRest [] _ = Nothing
nthItemWithRest xs i = Just $ (genericIndex xs i,nthItemRemoved xs i)

enumerate :: [a] -> [(Integer,a)]
enumerate xs = enumerateChooseStart xs 0

enumerateChooseStart :: [a] -> Integer -> [(Integer,a)]
enumerateChooseStart xs i = zip [i..] xs

indexes :: [a] -> [Integer]
indexes = (map fst) . enumerate

eachItemWithRest :: [a] -> [(a,[a])]
eachItemWithRest xs = catMaybes . (map (nthItemWithRest xs)) $ indexes xs

{-
--Default Number of dimensions.
--type Vector = Vector2d

data Location = Vector

data Velocity = Vector

data Boid = Boid
  { currentLocation :: Location
  , currentVelocity :: Velocity
  }

type Boids = [Boid]

applyRule :: Boids -> Boids
type Rules = [Rule]
-}
-- https://wiki.haskell.org/Compose
compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v
{-
applyRules :: [(Boids -> Boids)] -> Boids -> Boids
applyRules = compose

applyVelocity :: Location -> Velocity -> Location
applyVelocity = undefined

applyVelocityMultiple :: Boids -> Boids
applyVelocityMultiple = undefined
-}
main :: IO ()
main = undefined
