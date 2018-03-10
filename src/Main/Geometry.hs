{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds #-}

module Main.Geometry where

import Data.ByteString (ByteString)

import Data.Vect hiding (reflect')
import Data.Vect.Float.Instances ()

import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV

import LambdaCube.GL
import LambdaCube.GL.Mesh
 
import Common.Utils
import Common.GraphicsUtils

data Geometry a  = Geometry    { name :: ByteString, attribs :: a, objects :: [WorldObject] }
data WorldObject = WorldObject { worldPos :: Vec3, meshInfo :: MeshInfo }
data MeshInfo    = MeshInfo    { meshName :: ByteString, diffuseTexName :: [Char] }

sceneMeshes =
  [WorldObject (Vec3 0 0 0) $ MeshInfo "Object/Terrain.mesh" "Object/Terrain.png"]
  ++ [ WorldObject (Vec3 ((fromIntegral (signum i * (i `mod` 5))) * 5.0) 8 ((fromIntegral i/5) * 5.0)) $ MeshInfo "Object/Suzanne.mesh" "Object/Terrain.png" | i <- [-12..12]]

objectsGeom = Geometry
  { name    = "objects"
  , attribs = (IV3F "position", IV2F "texture_uv", IV3F "normal")
  , objects = sceneMeshes
  }

screenSquareGeom = Geometry
  { name    = "screenSquare"
  , attribs = (IV3F "position", IV2F "texture_uv", IV3F "normal")
  , objects = [WorldObject (Vec3 0 0 0) $ MeshInfo "Object/ScreenSquare.mesh" "Object/Cube.png"]
  }
