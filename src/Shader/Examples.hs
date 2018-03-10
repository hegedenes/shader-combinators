{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, AllowAmbiguousTypes, FlexibleInstances, DataKinds, ViewPatterns, PatternSynonyms, FlexibleContexts, GADTs #-}

module Shader.Examples where

import LambdaCube.GL

import Common.GraphicsUtils

import Shader.Base
import Shader.Combinator

import Main.Geometry

import qualified Data.ByteString.Char8 as SB

-- Base

type VertexShaderInput     = Exp V (V3F, V2F, V3F)

type VertexShaderOutput    = V3F :+: V2F :+: V3F :+: ZZ
type VertexShaderOutputExp = Exp F (FTRepr VertexShaderOutput)

type VertexShaderFun'      = VertexShaderFun VertexShaderInput VertexShaderOutput
type FragmentShaderFun'    = FragmentShaderFun VertexShaderOutputExp FragmentShaderOutExp'

model, modelViewProj :: Exp V M44F
model         = Uni (IM44F "M")
modelViewProj = Uni (IM44F "MVP")

skyColor, blackColor :: V4F
skyColor   = V4 0.5 0.7 1 1
blackColor = V4 0 0  0  1

vertexPosId :: PosTransformFun VertexShaderInput
vertexPosId (untup3 -> (pos,_,_)) = v3v4 pos

vertexPosToScreen :: PosTransformFun VertexShaderInput
vertexPosToScreen (untup3 -> (pos,_,_)) = modelViewProj @*. v3v4 pos

--vsPos :: VertexShaderFun'
vsPos (untup3 -> (pos,_,_)) = Smooth pos :. ZT

blendEquation1 = Blend (FuncAdd, FuncAdd) ((One, One), (One, One)) one'
blendEquation2 = Blend (FuncAdd, FuncAdd) ((SrcAlpha, OneMinusSrcAlpha), (SrcAlpha, OneMinusSrcAlpha)) one'
blendEquation4 = Blend (FuncAdd, FuncAdd) ((SrcAlpha, DstAlpha), (One, One)) one' --NoBlending

baseContext :: Context (V4F :+: ZZ)
baseContext = Context
  { frameBuffer = FrameBuffer $ DepthImage n1 1000 :. ColorImage n1 blackColor :. ZT
  , ctxConfig = DepthOp Less False :. ColorOp blendEquation4 (one' :: V4B) :. ZT
  , fsInit = Const (V4 0 0 0 0) :. ZT
  }

baseBlendContext :: Context (V4F :+: ZZ)
baseBlendContext = Context
  { frameBuffer = FrameBuffer $ DepthImage n1 1000 :. ColorImage n1 blackColor :. ZT
  , ctxConfig = DepthOp Less True :. ColorOp blendEquation2 (one' :: V4B) :. ZT
  , fsInit = Const (V4 0 0 0 0) :. ZT
  }

baseNoBlendContext :: Context (V4F :+: ZZ)
baseNoBlendContext = Context
  { frameBuffer = FrameBuffer $ DepthImage n1 1000 :. ColorImage n1 blackColor :. ZT
  , ctxConfig = DepthOp Less True :. ColorOp NoBlending (one' :: V4B) :. ZT
  , fsInit = Const (V4 0 0 0 0) :. ZT
  }

deferredContext :: Context (V4F :+: V4F :+: V4F :+: V4F :+: ZZ)
deferredContext = Context
  { frameBuffer = FrameBuffer $ DepthImage n1 1000 :. ColorImage n1 blackColor :. ColorImage n1 blackColor :. ColorImage n1 blackColor :. ColorImage n1 blackColor :. ZT
  , ctxConfig = DepthOp Less True :. ColorOp NoBlending (one' :: V4B) :. ColorOp NoBlending (one' :: V4B) :. ColorOp NoBlending (one' :: V4B) :. ColorOp NoBlending (one' :: V4B) :. ZT
  , fsInit = Const (V4 0 0 0 0) :. Const (V4 0 0 0 0) :. Const (V4 0 0 0 0) :. Const (V4 0 0 0 0) :. ZT
  }

postProcessProg fs baseProg = ShaderProgram vertexPosId vsPos (fs (createSampler 1280 720 . baseProg))

--defer :: FragmentShaderFun' -> ShaderSampler (((t0, V4F), V4F), V4F) -> FragmentShaderFun'
defer fs sampler screenPos = (fsDeferDiffuse --> fs) (tup3 (pos,texture,normal))
  where
    pos    = v4v3 $ sample sampler tix0 screenPos
    texture = v4v2 $ sample sampler tix1 screenPos
    normal  = v4v3 $ sample sampler tix2 screenPos
    fsDeferDiffuse _ _ = sample sampler tix3 screenPos :. ZT

deferredShader layer = projectTo layer $ accumulate objectsGeom deferredContext prog
  where
    prog = ShaderProgram vertexPosToScreen vsDefault fsDefer

deferredProg fs = postProcessProg (defer fs) deferredShader

--deferredFB context fs = accumulate screenSquareGeom context $ deferredProg fs

fsDefer (untup3 -> (pos,uv,normal)) c0 = texture' diffuseTex uv :. v3v4 normal :. v2v4 uv :. v3v4 pos :. ZT

with :: VertexShaderFun' -> FragmentShaderFun' -> Shader
vs  `with`   fs       = toScreen $ accumulate objectsGeom baseNoBlendContext (ShaderProgram vertexPosToScreen vs fs)

samples
  :: (ShaderSampler t -> FragmentShaderFun (Exp F V3F) FragmentShaderOutExp')
  -> Shader -> Shader
fs `samples` baseProg = toScreen $ accumulate screenSquareGeom baseNoBlendContext $ postProcessProg fs (\layer -> baseProg)

simple   = with vsDefault
deferred fs = toScreen $ accumulate screenSquareGeom baseNoBlendContext $ deferredProg fs

-- Vertex Shaders

vsDefault :: VertexShaderFun'
vsDefault (untup3 -> (pos,uv,normal)) = Smooth (v4v3 $ model @*. v3v4 pos) :. Smooth uv :. Smooth (normalize' normal) :. ZT

-- Fragment Shaders

sunPos, lightPos, lightColor, cameraPos :: Exp F V3F
sunPos     = Uni (IV3F "sunPos")
lightPos   = Uni (IV3F "lightPos")
lightColor = Uni (IV3F "lightColor")
cameraPos  = Uni (IV3F "cameraPos")

diffuseTex = Sampler LinearFilter ClampToEdge $ TextureSlot "diffuseTex" (Texture2D (Float RGBA) n1)

fsRed, fsGreen, fsBlue :: FragmentShaderFun'
fsRed   _ _ = (pack' $ V4 (floatF 1) (floatF 0) (floatF 0) (floatF 0.25)) :. ZT
fsGreen _ _ = (pack' $ V4 (floatF 0) (floatF 1) (floatF 0) (floatF 0.25)) :. ZT
fsBlue  _ _ = (pack' $ V4 (floatF 0) (floatF 0) (floatF 1) (floatF 0.05)) :. ZT

fsPos    (untup3 -> (pos,_,_))    _ = v3v4 pos    :. ZT
fsUV     (untup3 -> (_,uv,_))     _ = v2v4 uv     :. ZT
fsNormal (untup3 -> (_,_,normal)) _ = v3v4 normal :. ZT

fsTexture (untup3 -> (pos,uv,normal)) c0 = texture' diffuseTex uv :. ZT

fsDiffuse _ c0 = c0

fsAmbient :: FragmentShaderFun'
fsAmbient _ _ = (pack' $ V4 (floatF 0.2) (floatF 0.2) (floatF 0.2) (floatF 1)) :. ZT

fsSpecular' :: FragmentShaderFun'
fsSpecular' (untup3 -> (pos,_,normal)) _ = result :. ZT
  where
    sunDir = normalize' $ sunPos @- pos
    viewDir = normalize' $ cameraPos @- pos
    halfDir = normalize' $ sunDir @+ viewDir
    angle = max' (dot' halfDir normal) (floatF 0.0)
    specular = pow' angle (floatF 32.0)
    result = pack' $ V4 specular specular specular (floatF 1)

fsSpecular :: FragmentShaderFun'
fsSpecular (untup3 -> (pos,_,normal)) _ = result :. ZT
  where
    sunDir = normalize' $ sunPos @- pos
    viewDir = normalize' $ cameraPos @- pos
    refl = reflect' (neg' sunDir) normal
    angle = max' (dot' refl viewDir) (floatF 0.0)
    specular = pow' angle (floatF 128.0)
    result = pack' $ V4 specular specular specular (floatF 1)

fsGrayscale :: FragmentShaderFun'
fsGrayscale s (color :. ZT) = pack' (V4 luminance luminance luminance a) :. ZT
  where
    V4 r g b a = unpack' color
    luminance = floatF 0.3 @* r @+ floatF 0.59 @* g @+ floatF 0.11 @* b

fsFog :: FragmentShaderFun'
fsFog (untup3 -> (pos,_,_)) (c0 :. ZT) = result :. ZT
  where
    rate = getDist pos cameraPos @/ floatF 40
    result = mix' c0 (Const skyColor) (saturate rate)

fsSun :: FragmentShaderFun'
fsSun (untup3 -> (pos,_,normal)) c0 = result :. ZT
  where
    sunDir = normalize' $ sunPos @- pos
    rate = saturate $ dot' normal sunDir
    result = pack' $ V4 rate rate rate (floatF 1)

fsLight :: FragmentShaderFun'
fsLight (untup3 -> (pos,_,normal)) _ = result :. ZT
  where
    lightDir = normalize' $ lightPos @- pos
    V3 r g b = unpack' lightColor
    norm = normalize' normal
    rate = saturate $ dot' norm lightDir
    result' = pack' $ V4 (rate @* r) (rate @* g) (rate @* b) (floatF 1)
    distRate = getDist pos lightPos @/ floatF 10
    result = mix' result' (Const (V4 0 0 0 0)) (saturate distRate)

fsThresholding :: FragmentShaderFun'
fsThresholding _ (c0 :. ZT) = result :. ZT
  where
    V4 r0 g0 b0 a = unpack' c0
    levels = 5
    threshold c = round' (c @* floatF levels) @/ floatF levels
    result = pack' $ V4 (threshold r0) (threshold g0) (threshold b0) a

fsEdge :: FragmentShaderFun'
fsEdge (untup3 -> (pos,_,normal)) (c0 :. ZT) = result :. ZT
  where
    eyeDir = normalize' $ cameraPos @- pos
    lightDir = normalize' $ sunPos @- pos
    maxRes = max' (floatF 0.0) (normal @. lightDir)
    thickness = mix (floatF 0.4) (floatF 0.1) maxRes
    result = Cond (eyeDir @. normal @< thickness) (Const (V4 0 0 0 1)) c0

-- combined

fsPhong        = fsAmbient <++> (fsTexture <**> fsSun) <++> fsSpecular
fsPhong2       = fsAmbient <++> (fsDiffuse <**> fsSun) <++> fsSpecular
fsPhongTex     = fsTexture --> fsPhong2

fsToon         = fsThresholding --> fsEdge
fsPhongToon    = fsPhong --> fsToon

fsPhongToonFogGrayscale = fsPhongToon --> fsFog

fsPhongToonFog = fsPhongToon --> fsFog
fsGrayscaleFog = fsPhong --> (fsGrayscale <++> fsFog)
fsFogGrayscale = fsPhong --> (fsFog --> fsGrayscale)

fsLightDiff    = fsDiffuse <**> fsLight

shaderProgram1     = simple fsPhongToonFog
shaderProgramPhong = simple fsPhong

-- Fragment Shaders - Post Processing

ppScreenCoord (unpack' -> V3 x y z) = pack' $ V2 (toTex x) (toTex y)
  where
    toTex a = (a @+ floatF 1) @/ floatF 2

sample sampler layer = texture' (sampler layer) . ppScreenCoord

fsBlur :: ShaderSampler (t, V4F) -> FragmentShaderFun (Exp F V3F) (FragmentShaderOutExp (V4F :+: ZZ))
fsBlur sampler pos _ = color :. ZT
  where
    screenPos = ppScreenCoord pos
    kernel = 5
    steps = [V2 x y | x <- [-kernel..kernel], y <- [-kernel..kernel]]
    samp (V2 x y) = texture' (sampler tix0) $ screenPos @- pack' (V2 (floatF $ x*1/1280) (floatF $ y*1/720))
    colors = map samp steps
    color = foldl1 (@+) colors @/ floatF (fromIntegral $ length steps)

-- combined

blur         = fsBlur `samples` (simple fsPhong)
deferredBlur = fsBlur `samples` (deferred fsPhongToonFog)

-- Multipass rendering

deferredLightAt i = Accumulation (screenSquareGeom { name = (SB.append (name screenSquareGeom) (SB.pack $ show i)) }) baseContext (deferredProg fsLight)
deferredLightsProg = toScreen $ runAcc $ folda [deferredLightAt i | i <- [0..10]]

lightAt i = Accumulation (objectsGeom { name = (SB.append (name objectsGeom) (SB.pack $ show i)) }) baseContext (ShaderProgram vertexPosToScreen vsDefault fsLight)
lightsProg = toScreen $ runAcc $ folda [lightAt i | i <- [0..10]]

shaderProgramToUse = simple fsPhong --simple fsLightDiff -- simple fsNormal -- deferredProgram1