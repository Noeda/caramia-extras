-- | Loading images from various files.
--
-- The backend used in this module is JuicyPixels.
--

{-# LANGUAGE TupleSections #-}

module Caramia.Extras.LoadImage
    (
    -- * Loading images
    -- ** From the file system
      loadImage
    , loadImage2D
    -- ** From memory
    , loadImageFromMemory
    , loadImage2DFromMemory
    -- * Data structures
    , HowToLoad(..)
    -- ** Exceptions
    , ImageLoadingError(..) )
    where

import Caramia.Prelude
import Caramia.Texture
import Caramia.Buffer
import Caramia.ImageFormats
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as B
import Codec.Picture
import qualified Data.Text as T
import Control.Exception
import Foreign.Ptr

-- | Describes where exactly you want to load a texture.
data HowToLoad = ToNewTexture2D
               -- ^ Return a `Tex2D` of the loaded textures
               --
               -- The texture will have mipmaps.
               | ToTexture2D !Texture
               -- ^ Loads the texture to an existing `Tex2D` texture.
               --
               -- If the target texture has a different size than the loaded
               -- image, then the image is either clipped (if the image is
               -- larger) or placed at the top-left corner of the layer in the
               -- texture (the rest of the area is not touched).
               --
               -- `loadImage` returns the same texture as given here if you use
               -- this.
               --
               -- Mipmaps are not automatically generated.
               | ToTexture2DArray !Texture !Int
               -- ^ Load the image to a texture array.
               --
               -- The integer in this constructor is the layer index. Otherwise
               -- this is the same as `ToTexture2D`.
               --
               -- Mipmaps are not automatically generated.
               deriving ( Eq, Typeable )

-- | Thrown if something goes wrong in image decoding part.
data ImageLoadingError = DecodePhaseError !T.Text
                       | Unsupported !T.Text
                       deriving ( Eq, Ord, Show, Read, Typeable )

instance Exception ImageLoadingError

type ImageLoadingAction a = Int
                         -> Int
                         -> Int
                         -> SpecificationType
                         -> UploadFormat
                         -> Ptr ()
                         -> IO a

-- | Loads an image a bytestring to a texture.
--
-- There is a limited number of color formats that are supported. At the
-- moment, we support:
--
--   * 8-bit RGB and RGBA
--   * 16-bit RGB and RGBA
--   * floating point (32-bit) RGB
--   * 8-bit, 16-bit and floating point grayscale images (loaded as red-channel
--   only image)
--
-- 8-bit RGB and RGBA images are assumed to be in the sRGB color space.
--
-- May throw `ImageLoadingError` if something goes wrong.
loadImageFromMemory :: B.ByteString -> HowToLoad -> IO Texture
loadImageFromMemory bs howto =
    withImageLoading bs $ \width height pitch spec uf ptr -> do
        let maybe_format = uploadFToTexF spec uf
        ((tex, uploader), bytes_per_pixel) <- case maybe_format of
            Nothing -> throwIO $ Unsupported $
                "loadImage: Combination of " <> showT spec <> " and " <>
                showT uf <> " is not supported."
            Just (format, bytes_per_pixel) ->
                (,bytes_per_pixel) <$>
                chooseTexture format width height pitch spec uf ptr

        buf <- newBuffer defaultBufferCreation {
                     accessHints = (Static, Draw)
                   , size = width*height*bytes_per_pixel
                   , initialData = Just ptr
                   , accessFlags = NoAccess }
        uploader buf
        return tex
  where
    chooseTexture format width height pitch spec uf ptr = case howto of
        ToNewTexture2D -> do
            tex <- newTexture textureSpecification {
                          topology = Tex2D { width2D = width
                                           , height2D = height }
                        , imageFormat = format
                        , mipmapLevels = maxMipmapLevels (max width height)
                        }
            return (tex, \buf -> uploadToTexture
                             (uploading2D buf width height spec uf) {
                              numColumns = pitch
                              }
                               tex >> generateMipmaps tex)
        ToTexture2D tex ->
            return (tex, \buf -> uploadToTexture
                                (uploading2D buf width height spec uf) {
                                numColumns = pitch
                                , uWidth = min width (viewWidth tex)
                                , uHeight = min height (viewHeight tex)
                                }
                                 tex)
        ToTexture2DArray tex layer ->
            return (tex, \buf -> uploadToTexture
                             (uploading2D buf width height spec uf) {
                              numColumns = pitch
                            , zOffset = layer
                            , uWidth = min width (viewWidth tex)
                            , uHeight = min height (viewHeight tex)
                              }
                              tex)

-- | Same as `loadImageFromMemory` but always returns a new `Tex2D` texture.
loadImage2DFromMemory :: B.ByteString -> IO Texture
loadImage2DFromMemory = flip loadImageFromMemory ToNewTexture2D

-- | Same as `loadImageFromMemory` but reads the image from the filesystem.
--
-- May throw an `IOError` for failed file operations.
loadImage :: FilePath -> HowToLoad -> IO Texture
loadImage fpath howto = do
    bs <- B.readFile fpath
    loadImageFromMemory bs howto

-- | Same as `loadImage` but always returns a new `Tex2D` texture.
loadImage2D :: FilePath -> IO Texture
loadImage2D = flip loadImage ToNewTexture2D

uploadFToTexF :: SpecificationType -> UploadFormat -> Maybe (ImageFormat, Int)
uploadFToTexF FWord8 UR = Just (R8, 1)
uploadFToTexF FWord16 UR = Just (R16, 2)
uploadFToTexF FFloat UR = Just (R32F, 4)
uploadFToTexF FWord8 URGB = Just (SRGB8, 3)
uploadFToTexF FWord16 URGB = Just (RGB16, 6)
uploadFToTexF FFloat URGB = Just (RGB32F, 12)
uploadFToTexF FWord8 URGBA = Just (SRGB8_ALPHA8, 4)
uploadFToTexF FWord16 URGBA = Just (RGBA16, 8)
uploadFToTexF FFloat URGBA = Just (RGBA32F, 16)
uploadFToTexF _ _ = Nothing

withImageLoading :: B.ByteString
                 -> ImageLoadingAction a
                 -> IO a
withImageLoading (decodeImage -> maybe_img) fun =
    case maybe_img of
        Left err -> throwIO $ DecodePhaseError $ T.pack err
        Right img -> withLoadedImage img fun

withLoadedImage :: DynamicImage
                -> ImageLoadingAction a
                -> IO a
withLoadedImage (ImageY8 img) = unsafing img FWord8 UR
withLoadedImage (ImageY16 img) = unsafing img FWord16 UR
withLoadedImage (ImageYF img) = unsafing img FFloat UR
withLoadedImage (ImageRGB8 img) = unsafing img FWord8 URGB
withLoadedImage (ImageRGB16 img) = unsafing img FWord16 URGB
withLoadedImage (ImageRGBF img) = unsafing img FFloat URGB
withLoadedImage (ImageRGBA8 img) = unsafing img FWord8 URGBA
withLoadedImage (ImageRGBA16 img) = unsafing img FWord16 URGBA
withLoadedImage dynimg = const $ throwIO $ Unsupported $
    "This image format (" <> showT (namify dynimg) <> ") is not " <>
    "supported."

namify :: DynamicImage -> T.Text
namify (ImageY8 {}) = "grayscale 8-bit"
namify (ImageY16 {}) = "grayscale 16-bit"
namify (ImageYF {}) = "grayscale float"
namify (ImageRGB8 {}) = "rgb 8-bit channels"
namify (ImageRGB16 {}) = "rgb 16-bit channels"
namify (ImageRGBF {}) = "rgb float channels"
namify (ImageRGBA8 {}) = "rgba 8-bit channels"
namify (ImageRGBA16 {}) = "rgba 16-bit channels"
namify (ImageCMYK8 {}) = "cmyk 8-bit channels"
namify (ImageCMYK16 {}) = "cmyk 16-bit channels"
namify (ImageYA8 {}) = "grayscale-alpha 8-bit"
namify (ImageYA16 {}) = "grayscale-alpha 16-bit"
namify (ImageYCbCr8 {}) = "YCbCr 8-bit"
namify _ = "unknown format"

unsafing :: V.Storable (PixelBaseComponent a)
         => Image a
         -> SpecificationType
         -> UploadFormat
         -> ImageLoadingAction b
         -> IO b
unsafing img stype uf fun =
    V.unsafeWith (imageData img) $ \ptr ->
        fun (imageWidth img)
            (imageHeight img)
            (imageWidth img)
            stype
            uf
            (castPtr ptr)

