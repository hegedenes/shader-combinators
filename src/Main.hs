{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds #-}

module Main where

import "GLFW-b" Graphics.UI.GLFW as GLFW

import Data.Vect hiding (reflect')

import LambdaCube.GL
import LambdaCube.GL.Mesh

import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.Fix

import FRP.Elerea.Param

import Common.Utils
import Common.GraphicsUtils

import Main.Geometry
import Shader.Base
import Shader.Examples

import qualified Data.Trie as T
import qualified Data.ByteString.Char8 as SB

import Codec.Image.STB hiding (Image)

draw :: Renderer -> Window -> IO a -> IO ()
draw renderer win command = do
  render renderer
  command
  swapBuffers win
  pollEvents

readInput :: Fractional a => (Key -> IO Bool) -> IO (Maybe a)
readInput keyIsPressed = do
  Just t <- getTime
  setTime 0

  k <- keyIsPressed Key'Escape
  return $ if k then Nothing else Just (realToFrac t)

cameraSignal keyIsPressed =
  effectful $ (,,,,)
    <$> keyIsPressed Key'Left
    <*> keyIsPressed Key'Up
    <*> keyIsPressed Key'Down
    <*> keyIsPressed Key'Right
    <*> keyIsPressed Key'RightShift

setupRendering renderer objects time (windowWidth, windowHeight) (cameraPosition, cameraDirection, cameraUp, _, _) = do
  let sceneSlots = uniformSetter renderer
      setSunPos = uniformV3F "sunPos" sceneSlots . fromVec3
      setCameraPos = uniformV3F "cameraPos" sceneSlots . fromVec3

      setSize = setScreenSize renderer
      aspect = fromIntegral windowWidth / fromIntegral windowHeight
      cameraView = fromProjective (lookat cameraPosition (cameraPosition &+ cameraDirection) cameraUp)
      cameraProjection = perspective 0.1 1000 (pi/4) aspect

  setSunPos $ Vec3 (100 * cos (time / 5)) (100 * sin (time / 5)) 0
  setCameraPos cameraPosition

  forM_ objects $ \obj -> do
    let (object, loadedMesh, uniforms) = obj
        objectSlot = objectUniformSetter loadedMesh
    uniforms objectSlot cameraView cameraProjection

  setSize (fromIntegral windowWidth) (fromIntegral windowHeight)
  return $ return ()

mouseSignal :: Window -> IO Vec2
mouseSignal win = do
  (x, y) <- getCursorPos win
  return $ Vec2 (realToFrac x) (realToFrac y)

scene win keyIsPressed renderer objects windowSize = do
  pause <- toggle =<< risingEdge =<< effectful (keyIsPressed (Key'P))
  time <- transfer 0 (\dt paused time -> time + if paused then 0 else dt) pause 

  mousePosition <- effectful $ mouseSignal win
  mousePosition' <- delay zero mousePosition
  directionControl <- cameraSignal keyIsPressed
  camera <- denesCamera (Vec3 0 5 0) (mousePosition - mousePosition') directionControl

  effectful3 (setupRendering renderer objects) time windowSize camera

sceneThread sceneRes drawRes = do
  thread <- sceneRes
  return $ drawRes <$> thread

objUniforms obj img objectSlot cameraView cameraProjection = do
    let setDiffuseTex = uniformFTexture2D "diffuseTex" objectSlot
        setObjectMVP = uniformM44F "MVP" objectSlot . fromMat4
        setObjectM = uniformM44F "M" objectSlot . fromMat4
        pos = fromProjective $ translation (worldPos obj)

    setObjectMVP (pos .*. cameraView .*. cameraProjection)
    setObjectM pos

    setDiffuseTex img
    
lightColors = [ Vec3 r g b | r <- [0.5, 1], g <- [0, 0.5, 1], b <- [0, 0.5, 1]]

lightPosUniforms i objectSlot = do
    let setLightPos = uniformV3F "lightPos" objectSlot . fromVec3
    let setLightColor = uniformV3F "lightColor" objectSlot . fromVec3
    setLightPos $ Vec3 (5 * fromIntegral (i `div` 5)) 2 (5 * fromIntegral (i `mod` 5))
    setLightColor $ lightColors !! fromIntegral i

loadGeometry renderer geometry uniforms =
  liftM concat $ forM (objects geometry) $ \obj -> do
    let mi = meshInfo obj
    gpuMesh <- loadMesh (meshName mi)
    Right texFile <- loadImage (diffuseTexName mi)
    img <- compileTexture2DRGBAF True False texFile

    meshes <- forM [(0 :: Integer) .. 10] $ \lightId -> do
      loadedMesh <- addMesh renderer (SB.append (name geometry) (SB.pack $ show lightId)) gpuMesh ["MVP","lightPos","lightColor","M","diffuseTex"]
      return (obj,loadedMesh,\slot cameraView cameraProjection -> lightPosUniforms lightId slot >> uniforms obj img slot cameraView cameraProjection)

    loadedMesh1 <- addMesh renderer (name geometry) gpuMesh ["MVP","M","diffuseTex","lightPos"]
    return $ (obj,loadedMesh1,uniforms obj img):meshes

main :: IO ()
main = run shaderProgramToUse

run :: Shader -> IO ()
run shaderProgram = do
  (win,windowSize) <- initWindow "Test" 1280 720

  renderer <- compileRenderer $ ScreenOut shaderProgram

  putStrLn "Renderer stream slots:"
  forM_ (T.toList (slotStream renderer)) $ \(name, (primitive, attributes)) -> do
        putStrLn $ "  " ++ SB.unpack name ++ " - " ++ show primitive
        forM_ (T.toList attributes) $ \(attributeName, attributeType) -> do
            putStrLn $ "    " ++ SB.unpack attributeName ++ " :: " ++ show attributeType

  objects <- loadGeometry renderer objectsGeom objUniforms
  screenObj <- loadGeometry renderer screenSquareGeom (\obj img objectSlot cameraView cameraProjection -> return ())
  
  putStrLn "Loaded geometry"
  
  let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k

  sceneSignal <- start $ do
    thread <- scene win keyIsPressed renderer (objects ++ screenObj) windowSize
    return $ draw renderer win <$> thread
  driveNetwork sceneSignal $ readInput keyIsPressed

  putStrLn "Terminating"

  dispose renderer
  destroyWindow win
  terminate
