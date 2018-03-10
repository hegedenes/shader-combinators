import sys
import os

name = sys.argv[1]

def from_f(f):
  x = f.split('/')
  #return '('+name.lower()+'Vertices !! ('+x[0]+'-1), '+name.lower()+'TexCoords !! ('+x[1]+'-1), '+name.lower()+'Normals !! ('+x[2]+'-1))'
  return [int(x[0]), int(x[1]), int(x[2])]

def to_str(line):
  if line[0] in ['v','vn']:
    return 'V3 ('+line[1]+') ('+line[2]+') ('+line[3]+')'
  if line[0] in ['vt']:
    return 'V2 ('+line[1]+') ('+line[2]+')'
  if line[0] == 'f':
    # return ', '.join([from_f(line[1]),from_f(line[2]),from_f(line[3])])
    return [from_f(line[1]), from_f(line[2]), from_f(line[3])]
  return str(line)

file_in = open(name + '.obj', 'r')
file_out = open(name + '.hs', 'w')

file_out.write(
"""{-# LANGUAGE OverloadedStrings #-}

module Object."""+name+""" ("""+name.lower()+"""Mesh) where

import LambdaCube.GL
import LambdaCube.GL.Mesh

import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV

"""
)

data = {}

for line in file_in:
  x = line.split()
  key = x[0]
  if key == '#':
    continue
  if not key in data.keys():
    data[key] = []
  data[key].append(to_str(x))


file_out.write(
name.lower() + """Vertices :: [V3F]
""" + name.lower() + """Vertices =
  [ """
)



vertices = []
for line in data['f']:
  for [pos,tex,norm] in line:
    vertices.append(data['v'][pos-1])

file_out.write('\n  , '.join(vertices))

file_out.write(
"""
  ]

"""
)

file_out.write(
name.lower() + """Normals :: [V3F]
""" + name.lower() + """Normals =
  [ """
)

normals = []
for line in data['f']:
  for [pos,tex,norm] in line:
    normals.append(data['vn'][norm-1])
  
file_out.write('\n  , '.join(normals))

file_out.write(
"""
  ]

"""
)

texcoords = []
for line in data['f']:
  for [pos,tex,norm] in line:
    texcoords.append(data['vt'][tex-1])
  
file_out.write(
name.lower() + """TexCoords :: [V2F]
""" + name.lower() + """TexCoords =
  [ """
)

file_out.write('\n  , '.join(texcoords))

file_out.write(
"""
  ]

"""
)

file_out.write(
name.lower() + """Mesh :: Mesh
""" + name.lower() + """Mesh = Mesh
  { mAttributes   = T.fromList
    [ ("position",      A_V3F $ SV.fromList """ + name.lower() + """Vertices)
    , ("texture_uv",    A_V2F $ SV.fromList """ + name.lower() + """TexCoords)
    , ("normal",        A_V3F $ SV.fromList """ + name.lower() + """Normals)
    ]
  , mPrimitive    = P_Triangles
  , mGPUData      = Nothing
  }

main = do
  saveMesh \""""+name+""".mesh\" """ + name.lower() + """Mesh
"""
)

file_in.close()
file_out.close()

os.system('runhaskell ' + name + '.hs')
os.remove(name + '.hs')