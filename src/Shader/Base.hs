{-# LANGUAGE OverloadedStrings, TypeOperators, TypeFamilies, DataKinds, FlexibleContexts #-}

module Shader.Base {-(PosTransformFun, VertexShaderFun, FragmentShaderFun, Shader, shader, createSampler)-} where

import GHC.TypeLits

import Data.ByteString

import LambdaCube.GL
import Common.GraphicsUtils

import Main.Geometry

-- Base

type FragmentShaderOutExp a = FlatExp F a
type FragmentShaderResult a = Depth Float :+: ColorRepr a

type PosTransformFun vertexShaderIn = vertexShaderIn -> Exp V V4F
type VertexShaderFun input output   = input -> InterpolatedFlatExp V output
type FragmentShaderFun input output = input -> output -> output

type FrameBufferType t = Exp Obj (FrameBuffer 1 t)
type FrameBufferCtx a  = FrameBufferType (FTRepr' (FragmentShaderResult a))
type Shader            = Exp Obj (Image 1 V4F)

type V4Texture = Texture Tex2D SingleTex (Regular Float) RGBA
type V4Sampler = Sampler Tex2D SingleTex (Regular Float) RGBA

type SamplerIdx i = TupleIdx (TupleRepr i) V4F
type ShaderSampler t = TupleIdx t V4F -> Exp F V4Sampler

data Context a = Context
  { frameBuffer :: FrameBufferCtx a
  , ctxConfig   :: FlatTuple NoConstraint FragmentOperation (FragmentShaderResult a)
  , fsInit      :: FragmentShaderOutExp a
  }

data ShaderProgram vIn vOut fOut = ShaderProgram
  { posTransform   :: PosTransformFun (Exp V (InputTupleRepr vIn))
  , vertexShader   :: VertexShaderFun (Exp V (InputTupleRepr vIn)) vOut
  , fragmentShader :: FragmentShaderFun (Exp F (FTRepr vOut)) (FragmentShaderOutExp fOut)
  }

fragmentStreamBase program = Rasterize rasterCtx . Transform (vertexShaderBase program)
  where
    rasterCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex
    vertexShaderBase program input = VertexOut (posTransform program input) (Const 1) ZT (vertexShader program input)

accumulate
  :: (SGPU (InputTupleRepr vIn), InputTuple vIn, GPU (FTRepr vOut),
      GPU (FTRepr' (FragmentShaderResult fOut)), IsColorOutput (ColorRepr fOut), NoStencilRepr (ColorRepr fOut) ~ ColorRepr fOut) =>
  Geometry vIn
  -> Context fOut
  -> ShaderProgram vIn vOut fOut
  -> FrameBufferCtx fOut
accumulate geom context program = Accumulate ctx PassAll fsResult fragmentStream (frameBuffer context)
  where
    objects = Fetch (name geom) Triangles (attribs geom)
    fragmentStream = fragmentStreamBase program objects
    ctx = AccumulationContext Nothing (ctxConfig context)
    fsResult input = FragmentOutRastDepth $ fragmentShader program input (fsInit context)

projectTo :: TupleIdx (EltRepr b) V4F -> FrameBufferType b -> Shader
projectTo = PrjFrameBuffer ""

toScreen :: EltRepr b ~ (t, V4F) => FrameBufferType b -> Shader
toScreen = projectTo tix0

postProcessTex :: Word32 -> Word32 -> Shader -> Exp Obj V4Texture
postProcessTex width height baseProgram = Texture (Texture2D (Float RGBA) n1) (V2 width height) NoMip [baseProgram]

createSampler :: Word32 -> Word32 -> Shader -> Exp stage V4Sampler
createSampler width height = Sampler PointFilter ClampToEdge . postProcessTex width height