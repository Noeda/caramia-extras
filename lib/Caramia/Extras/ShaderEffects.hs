{-# LANGUAGE RecordWildCards, NegativeLiterals, DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- | A toy module to implement fullscreen effects.
--
-- This works by rendering to a texture with a shader, and then rendering again
-- to another, same sized texture with another shader, using the first texture
-- as source data.
--
-- You can implement interesting visual effects such as Conway's Game of Life
-- in GPU and see the results with this module.
--

module Caramia.Extras.ShaderEffects
    ( newShaderEffects
    , stepShaderEffects
    , ShaderEffects() )
    where

import Caramia.Prelude
import Caramia
import Caramia.Extras.MonotonicTime
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.Storable
import Foreign
import System.Random

-- | Creates new shader effects.
--
-- You need to give a fragment shader source code to this function.
--
-- Several uniform values are provided if they are defined in the shader:
--
-- @
--     resolution           ivec2     Gives the current size of the texture.
--     pixel_size           vec2      Size of one pixel.
--     monotonic_time       float     Gives the current time in seconds
--     previous             sampler2D The texture of the previous iteration
-- @
--
-- Also, some attributes that can be used:
--
-- @
--     xy           vec2              The xy coordinates of the current
--                                    fragment.
--     uv           vec2              The uv coordinates of the current
--                                    fragment.
-- @
--
-- The texture used is a 32-bit floating point texture with all RGBA channels
-- available.
--
-- Initially the textures are filled with uniformily random data.
--
-- Write the results to attribute location 0.
newShaderEffects :: T.Text -> Int -> Int -> IO ShaderEffects
newShaderEffects shader_code w h = do
    fs <- newShader shader_code Fragment
    fs_unload <- newShader unloadFragmentCode Fragment
    vs <- newShader quadVertexCode Vertex
    pl <- newPipeline [fs, vs]
    pl_unload <- newPipeline [fs_unload, vs]

    tex_loc <- getUniformLocation "tex" pl_unload
    setUniform (0::Int) tex_loc pl_unload

    res_loc <- getUniformLocation "resolution" pl
    res_unload_loc <- getUniformLocation "resolution" pl_unload
    mon_loc <- getUniformLocation "monotonic_time" pl
    previous_loc <- getUniformLocation "previous" pl
    psize_loc <- getUniformLocation "pixel_size" pl

    tex  <- makeTex
    tex2 <- makeTex

    noise <- allocaArray (w*h*4) $ \noises_ptr -> do
        for_ [0..(w*h*4) - 1] $ \idx -> do
            rnd <- randomRIO (0, 1)
            pokeElemOff noises_ptr idx (rnd :: Float)

        newBuffer defaultBufferCreation
                        { accessHints = (Static, Draw)
                        , size = bytes_used
                        , initialData = Just (castPtr noises_ptr)
                        , accessFlags = NoAccess }

    uploadToTexture (uploading2D noise w h FFloat URGBA) tex
    uploadToTexture (uploading2D noise w h FFloat URGBA) tex2

    fbuf1 <- newFramebuffer [(ColorAttachment 0, frontTextureTarget tex)]
    fbuf2 <- newFramebuffer [(ColorAttachment 0, frontTextureTarget tex2)]
    ref <- newIORef ((tex2, fbuf1), (tex, fbuf2))

    setUniform (0::Int) previous_loc pl
    setUniform (w, h) res_loc pl
    setUniform ( 1.0/fromIntegral w :: Float
               , 1.0/fromIntegral h :: Float) psize_loc pl
    setUniform (w, h) res_unload_loc pl_unload

    vao <- newQuadVAO

    return ShaderEffects {
        resolutionLoc = res_loc
      , unloadResolutionLoc = res_unload_loc
      , monotonicLoc = mon_loc
      , pixelSizeLoc = psize_loc
      , texes = ref
      , vao = vao
      , sePipeline = pl
      , unloadPipeline = pl_unload }
  where
    bytes_used = w*h*4*4

    makeTex = newTexture textureSpecification {
                  mipmapLevels = 1
                , imageFormat = RGBA32F
                , topology = Tex2D { width2D = w
                                   , height2D = h }
                }

newQuadVAO :: IO VAO
newQuadVAO = do
    buf <- withArray
                [ -128, -128
                ,  127, -128
                , -128,  127 :: Int8
                ,  127,  127 ] $ \ptr ->
        newBuffer defaultBufferCreation {
                accessHints = (Static, Draw)
                , size = sizeOf (undefined :: Int8) * 8
                , initialData = Just (castPtr ptr)
                , accessFlags = NoAccess }

    vao <- newVAO
    sourceVertexData buf defaultSourcing
        { components = 2
        , attributeIndex = 0
        , normalize = True
        , integerMapping = False
        , sourceType = SInt8 }
        vao
    return vao

stepShaderEffects :: ShaderEffects -> IO ()
stepShaderEffects (ShaderEffects {..}) = do
    tm <- getMonotonicTime
    ((tex_src, fbuf), (tex_dst, _)) <- atomicModifyIORef' texes $ \(t1, t2) ->
        ( (t2, t1), (t1, t2) )

    setUniform (tm :: Float) monotonicLoc sePipeline
    draw drawCommand
         { primitiveType = TriangleStrip
         , primitivesVAO = vao
         , numIndices = 4
         , sourceData = Primitives { firstIndex = 0 } }
         defaultDrawParams
         { pipeline = sePipeline
         , blending = nopBlend
         , targetFramebuffer = fbuf
         , bindTextures = IM.fromList [(0, tex_src)] }
    draw drawCommand
         { primitiveType = TriangleStrip
         , primitivesVAO = vao
         , numIndices = 4
         , sourceData = Primitives { firstIndex = 0 } }
         defaultDrawParams
         { pipeline = unloadPipeline
         , blending = nopBlend
         , bindTextures = IM.fromList [(0, tex_dst)] }

data ShaderEffects = ShaderEffects
    { resolutionLoc :: !UniformLocation
    , unloadResolutionLoc :: !UniformLocation
    , monotonicLoc  :: !UniformLocation
    , pixelSizeLoc  :: !UniformLocation
    , texes         :: !(IORef ((Texture, Framebuffer)
                        , (Texture, Framebuffer)))
    , vao           :: !VAO
    , sePipeline      :: !Pipeline
    , unloadPipeline :: !Pipeline }
    deriving ( Eq, Typeable )

unloadFragmentCode :: T.Text
unloadFragmentCode =
    "#version 330\n" <>
    "uniform sampler2D tex;\n" <>
    "in vec2 uv;\n" <>
    "layout (location = 0) out vec4 color;\n" <>
    "void main() {\n" <>
    "    color = texture(tex, uv);\n" <>
    "}\n"

quadVertexCode :: T.Text
quadVertexCode =
    "#version 330\n" <>
    "layout (location = 0) in vec2 pos;\n" <>
    "uniform ivec2 resolution;\n" <>
    "out vec2 xy;\n" <>
    "out vec2 uv;\n" <>
    "void main() {\n" <>
    "    uv = ((pos + vec2(1.0))*0.5);\n" <>
    "    xy = uv * vec2(resolution);\n" <>
    "    gl_Position = vec4(pos, 0.0, 1.0);\n" <>
    "}\n"

