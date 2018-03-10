{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DataKinds, ViewPatterns, PatternSynonyms, FlexibleContexts, GADTs #-}

module Shader.Combinator where

import Control.Arrow

import LambdaCube.GL
import Shader.Base

import Main.Geometry

type FragmentShaderOutExp' = FragmentShaderOutExp (V4F :+: ZZ)

(-->) :: FragmentShaderFun input output -> FragmentShaderFun input output -> FragmentShaderFun input output
(-->) = combine $ uncurry (>>>)

(<++>),(<-->),(<**>),(<//>) :: FragmentShaderFun input FragmentShaderOutExp' -> FragmentShaderFun input FragmentShaderOutExp' -> FragmentShaderFun input FragmentShaderOutExp'
(<++>) = merge (@+)
(<-->) = merge (@-)
(<**>) = merge (@*)
(<//>) = merge (@/)

merge f = combine $ uncurry (&&&) >>> ((uncurry (doOp f)) .)
  where
    doOp :: (Exp F V4F -> Exp F V4F -> Exp F V4F) -> FragmentShaderOutExp' -> FragmentShaderOutExp' -> FragmentShaderOutExp'
    doOp op (a :. ZT) (b :. ZT) = op a b :. ZT

combine :: Arrow cat => cat (outf, outg) output -> cat input outf -> cat input outg -> cat input output
combine op f g = (f &&& g) >>> op

--infixl 3 -->, <++>

-- Multipass rendering

data Accumulation vIn vOut fOut = Accumulation { geom :: Geometry vIn, ctx :: Context fOut, shader :: ShaderProgram vIn vOut fOut }

runAcc (Accumulation geom ctx prog) = accumulate geom ctx prog
modAcc acc fb = acc { ctx = (ctx acc) { frameBuffer = fb } }

(||>)
  :: (IsColorOutput (ColorRepr fOut1), InputTuple vIn1,
      SGPU (InputTupleRepr vIn1), GPU (FTRepr vOut1),
      GPU (FTRepr' (Depth Float :+: ColorRepr fOut)),
      FTRepr' (Depth Float :+: ColorRepr fOut1)
      ~ FTRepr' (Depth Float :+: ColorRepr fOut),
      NoStencilRepr (ColorRepr fOut1) ~ ColorRepr fOut1) =>
     Accumulation vIn1 vOut1 fOut1
     -> Accumulation vIn vOut fOut -> Accumulation vIn vOut fOut
a ||> b = modAcc b (runAcc a)

folda
  :: (SGPU (InputTupleRepr vIn), InputTuple vIn,
      IsColorOutput (ColorRepr fOut), GPU (FTRepr vOut),
      GPU (FTRepr' (Depth Float :+: ColorRepr fOut)),
      NoStencilRepr (ColorRepr fOut) ~ ColorRepr fOut) =>
     [Accumulation vIn vOut fOut] -> Accumulation vIn vOut fOut
folda = foldl1 (||>)
