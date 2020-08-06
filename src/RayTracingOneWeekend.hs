module RayTracingOneWeekend (
    pack,
    example2_fromWp,
    example2_fromWp',
    example2_1
) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as B
import Data.Tuple (swap)
import Data.Word
import qualified Data.Vector as V

newtype Colour = Colour (Word8, Word8, Word8)
    deriving (Show, Eq)

data Ppm = Ppm
    { _width :: !Int
    , _height :: !Int
    , _pixels :: V.Vector Colour
    }
    deriving (Show, Eq)

newtype Point = Point (Int, Int)
    deriving (Show, Eq)

pack :: Ppm -> LBS.ByteString
pack ppm =
    let spc = B.char7 ' '
        n   = B.char7 '\n'
        maxColour = 255
    in B.toLazyByteString $
        B.string7 "P3\n"
        <> B.intDec (_width ppm) <> spc <> B.intDec (_height ppm) <> n
        <> B.intDec maxColour <> n
        <> V.foldr' (\(Colour (r,g,b)) ->
            mappend (B.word8Dec r <> spc <> B.word8Dec g <> spc <> B.word8Dec b <> n)
        ) mempty (_pixels ppm)

mkPpm :: Int -> Int -> (Point -> Colour) -> Ppm
mkPpm width height getValue =
    let
        totalPixels = width * height
        indexToPoint i = Point . swap $ i `quotRem` width
        pixels = V.generate totalPixels (getValue . indexToPoint)
    in Ppm width height pixels

example2_fromWp :: Ppm
example2_fromWp =
    Ppm 3 2 $ V.fromList
        (fmap Colour [(255, 0, 0), (0, 255, 0), (0, 0, 255)
        , (255, 255, 0), (255, 255, 255), (0, 0, 0)
        ])

example2_fromWp' :: Ppm
example2_fromWp' =
    mkPpm 3 2 $ \(Point p) ->
        case p of
        (0,0) -> Colour (255, 0, 0)
        (1,0) -> Colour (0, 255, 0)
        (2,0) -> Colour (0, 0, 255)
        (0,1) -> Colour (255, 255, 0)
        (1,1) -> Colour (255, 255, 255)
        (2,1) -> Colour (0, 0, 0)
        _ -> Colour (0,0,0)

example2_1 :: Ppm
example2_1 =
    let imageWidth = 256
        imageHeight = 256
        blueValue = 255 `div` 4
    in mkPpm imageWidth imageHeight (\(Point (x,y)) ->
        let r = fromIntegral x
            g = fromIntegral (imageHeight - 1 - y)
            b = blueValue
        in Colour (r,g,b)
    )
