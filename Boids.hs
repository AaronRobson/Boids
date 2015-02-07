module Boids
where

{-
https://processing.org/examples/flocking.html
-}

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

pythagoras :: (Floating a) => [a] -> a
pythagoras = sqrt . sum . (map square)
  where
    square x = x * x

class Vector v where
    vectorElements :: v -> [Scalar]
    lengthV :: v -> Scalar
    addV :: v -> v -> v

instance Vector Vector1d where
    vectorElements v = [x v]
    lengthV = pythagoras . vectorElements
    addV v1 v2 = Vector1d $ (x v1) + (x v2)

instance Vector Vector2d where
    vectorElements v = [x v, y v]
    lengthV = pythagoras . vectorElements
    addV v1 v2 = Vector2d ((x v1) + (x v2)) ((y v1) + (y v2))

instance Vector Vector3d where
    vectorElements v = [x v, y v, z v]
    lengthV = pythagoras . vectorElements
    addV v1 v2 = Vector3d ((x v1) + (x v2)) ((y v1) + (y v2)) ((z v1) + (z v2))

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

applyAcceleration1d :: Object1d -> Acceleration1d -> Object1d
applyAcceleration1d o a = Object1d {location1d = l', velocity1d = v'}
    where
      v = velocity1d o
      v' = v `addV` a
      l = location1d o
      l' = l `addV` v'

applyAcceleration2d :: Object2d -> Acceleration2d -> Object2d
applyAcceleration2d o a = Object2d {location2d = l', velocity2d = v'}
    where
      v = velocity2d o
      v' = v `addV` a
      l = location2d o
      l' = l `addV` v'

applyAcceleration3d :: Object3d -> Acceleration3d -> Object3d
applyAcceleration3d o a = Object3d {location3d = l', velocity3d = v'}
    where
      v = velocity3d o
      v' = v `addV` a
      l = location3d o
      l' = l `addV` v'

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
