module Main where

import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT ( Capability(..), ClearBuffer(..), Color4(..), ColorMaterialParameter(..)
                        , ComparisonFunction(..), Cursor(..), DisplayMode(..), Face(..)
                        , Key(..), KeyState(..), Light(..), Modifiers(..), Position(..)
                        , ShadingModel(..), Size(..)
                        , DisplayCallback, ReshapeCallback
                        , ($=)
                        )
import Text.Printf ( printf )
import Codec.BMP
import qualified Data.ByteString.Unsafe as BS
import Data.Word ( Word8 )
import Foreign.Marshal.Alloc ( free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( sizeOf )
import Control.Concurrent

import Vis
import Vis.VisObject
import Vis.Camera

import Linear

import qualified Data.ByteString as B

makePictures :: Real b => Camera -> VisObject b -> IO ()
makePictures cam visobj = do
  GLUT.clear [ GLUT.ColorBuffer, GLUT.DepthBuffer ]

  -- draw the scene
  GLUT.preservingMatrix $ do
    setCamera cam
    drawObjects $ (fmap realToFrac) visobj

  GLUT.flush
  --GLUT.swapBuffers
  --_ <- swapMVar visReadyMVar True
  --GLUT.postRedisplay Nothing

screenShot :: Real b => Camera -> VisObject b -> IO BMP
screenShot camera visobj = do
  -- todo: are width/height reversed?
  --size@(Size width height) <- GLUT.get GLUT.windowSize
  let pos = Position 0 0
      width = 50
      height = 200
      size = Size width height
  ubytePtr <- mallocArray (fromIntegral (4*width*height)) :: IO (Ptr GLUT.GLubyte)
  let pixelData = GLUT.PixelData GLUT.RGBA GLUT.UnsignedByte ubytePtr
  makePictures camera visobj
  -- "glFinish" will do the job, but it may be overkill.
  -- "swapBuffers" is probably good enough.
  -- http://stackoverflow.com/questions/2143240/opengl-glflush-vs-glfinish
  -- We just need to make sure that readPixels will do the right thing
  GLUT.finish
  
  GLUT.readPixels pos size pixelData
  let wordPtr :: Ptr Word8
      wordPtr
        | sizeOf (0 :: GLUT.GLubyte) == sizeOf (0 :: Word8) = castPtr ubytePtr
        | otherwise = error "GLubyte size /= Word8 size"

  bs <- BS.unsafePackCStringFinalizer
        wordPtr (fromIntegral (4*width*height)) (free ubytePtr)
  
  let bmp :: BMP
      bmp = packRGBA32ToBMP32 (fromIntegral width) (fromIntegral height) bs
  
  return bmp

--cam = makeCamera (Camera0 60 20 7)
cam = makeCamera (Camera0 0 0 0)
cylinder = Trans (V3 0 0 1) $ Cylinder (0.1, 0.2) green

scene :: VisObject Double
scene = VisObjects $ [axes,box,ellipsoid,sphere] ++ (map text [-5..5]) ++ [boxText, plane] 
  where
    x = -1
    quat = normalize $ Quaternion 1 (V3 2 3 4)
    
    axes = Axes (0.5, 15)
    sphere = Trans (V3 0 x (-1)) $ Sphere 0.15 Wireframe (makeColor 0.2 0.3 0.8 1)
    ellipsoid = Trans (V3 x 0 (-1)) $ RotQuat quat $ Ellipsoid (0.2, 0.3, 0.4) Solid (makeColor 1 0.3 0.5 1)
    box = Trans (V3 0 0 x) $ RotQuat quat $ Box (0.2, 0.2, 0.2) Wireframe (makeColor 0 1 1 1)
    plane = Plane (V3 0 0 1) (makeColor 1 1 1 1) (makeColor 0.4 0.6 0.65 0.4)
    text k = Text2d "OLOLOLOLOLO" (100,500 - k*100*x) TimesRoman24 (makeColor 0 (0.5 + x'/2) (0.5 - x'/2) 1)
      where
        x' = realToFrac $ (x + 1)/0.4*k/5
    boxText = Text3d "trololololo" (V3 0 0 (x-0.2)) TimesRoman24 (makeColor 1 0 0 1)

main = do
  GLUT.getArgsAndInitialize
  setCamera cam
  bmp <- screenShot cam cylinder
  writeBMP "vis.bmp" bmp

