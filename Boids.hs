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
  } deriving (Show)

data Vector3d = Vector3d
  { x3d :: Scalar
  , y3d :: Scalar
  , z3d :: Scalar
  } deriving (Show)

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

class VectorElements a where
    vectorElements :: a -> [Scalar]

instance VectorElements Vector1d where
    vectorElements v = [x v]

instance VectorElements Vector2d where
    vectorElements v = [x v, y v]

instance VectorElements Vector3d where
    vectorElements v = [x v, y v, z v]

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
