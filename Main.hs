module Main where

import Codec.Picture
import Control.Monad (when)
import System.Exit (exitSuccess)

maxIterations :: Int
maxIterations = 100

hsvToRgb :: Int -> Int -> Int -> PixelRGB8
hsvToRgb h_ s_ v_ = PixelRGB8 (round(r*255)) (round(g*255)) (round(b*255))
    where
        (r, g, b) = hsvToRgb_
        h = fromIntegral h_ / 360 :: Double
        s = fromIntegral s_ / 100 :: Double
        v = fromIntegral v_ / 100 :: Double

        i = floor(h * 6) :: Int
        f = h * 6 - fromIntegral i
        p = v * (1 - s)
        q = v * (1 - f * s)
        t = v * (1 - (1 - f) * s)

        hsvToRgb_ :: (Double, Double, Double)
        hsvToRgb_ = case i `mod` 6 of
            0 -> (v, t, p)
            1 -> (q, v, p)
            2 -> (p, v, t)
            3 -> (p, q, v)
            4 -> (t, p, v)
            5 -> (v, p, q)
            _ -> error "impossible"

lerp :: Double -> Double -> Double -> Double
lerp s e t = s * (1 - t) + e * t

iterMandelbrot :: Double -> Double -> Int -> Double -> Double -> (Int, Double, Double)
iterMandelbrot za zb iter ca cb | (za * za + zb * zb) > 256 || iter == maxIterations = (iter, za, zb)
                                | otherwise = iterMandelbrot newZa newZb (iter + 1) ca cb
    where
        newZa = za * za - zb * zb + ca
        newZb = 2 * za * zb + cb

colorsUF :: [PixelRGB8]
colorsUF = [
      PixelRGB8 66  30  15 -- brown 3
    , PixelRGB8 25   7  26 -- dark violet
    , PixelRGB8  9   1  47 -- darkest blue
    , PixelRGB8  4   4  73 -- blue 5
    , PixelRGB8  0   7 100 -- blue 4
    , PixelRGB8 12  44 138 -- blue 3
    , PixelRGB8 24  82 177 -- blue 2
    , PixelRGB8 57 125 209 -- blue 1
    , PixelRGB8 134 181 229 -- blue 0
    , PixelRGB8 211 236 248 -- lightest blue
    , PixelRGB8 241 233 191 -- lightest yellow
    , PixelRGB8 248 201  95 -- light yellow
    , PixelRGB8 255 170   0 -- dirty yellow
    , PixelRGB8 204 128   0 -- brown 0
    , PixelRGB8 153  87   0 -- brown 1
    , PixelRGB8 106  52   3 -- brown 2
    ]

hexToPixel :: String -> PixelRGB8
hexToPixel str = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
    where
        r = read ("0x" ++ take 2 str) :: Int
        g = read ("0x" ++ take 2 ( drop 2 str)) :: Int
        b = read ("0x" ++ take 2 ( drop 4 str)) :: Int

rainbow :: [String]
rainbow = ["f94144","f3722c","f8961e","f9c74f","90be6d","43aa8b","577590"]

colorsRainbow :: [PixelRGB8]
colorsRainbow = map hexToPixel rainbow

brown :: [String]
brown = ["b76935","a56336","935e38","815839","6f523b","5c4d3c","4a473e","38413f","263c41","143642"]
colorsBrown :: [PixelRGB8]
colorsBrown = map hexToPixel brown

colorsPurple :: [PixelRGB8]
colorsPurple = [
      PixelRGB8 247 37 133
    , PixelRGB8 114 9 183
    , PixelRGB8 58 12 163
    , PixelRGB8 67 97 238
    , PixelRGB8 76 201 240
    ]

lerpPx :: Pixel8 -> Pixel8 -> Double -> Pixel8
lerpPx s e t = round $ fromIntegral s * (1 - t) + fromIntegral e * t

lerpColor :: PixelRGB8 -> PixelRGB8 -> Double -> PixelRGB8
lerpColor (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) t = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
    where
        r = lerpPx r1 r2 t
        g = lerpPx g1 g2 t
        b = lerpPx b1 b2 t

getColorContinous :: Double -> Double ->  Int -> [PixelRGB8] -> PixelRGB8
getColorContinous za zb nIter colors = colorContinous
    where
        log_zn = log(za * za + zb * zb) / 2
        nu = logBase 2 (log_zn / log 2)
        fracNIter = fromIntegral nIter - nu
        color1 = colors !! (floor fracNIter `mod` length colors)
        color2 = colors !! ((floor fracNIter + 1) `mod` length colors)

        (_, t) = properFraction fracNIter :: (Integer, Double)
        colorContinous = lerpColor color1 color2 t

getColorBasic :: Int -> PixelRGB8
getColorBasic nIter = hsvToRgb (round(fromIntegral nIter / 100.0 * 360 :: Double)) 100 100

generateMandelbrot :: Int -> Int -> Int -> Int -> Double -> Double -> Double -> String -> PixelRGB8
generateMandelbrot x y w h cx cy scale color | nIter == maxIterations = PixelRGB8 0 0 0
                                             | color == "continous" = colorContinous colorsUF
                                             | color == "rainbow" = colorContinous colorsRainbow
                                             | color == "purple" = colorContinous colorsPurple
                                             | color == "brown" = colorContinous colorsBrown
                                             | color == "basic" = colorBasic
                                             | otherwise = error "Unreachable code"
    where
        mapping = fromIntegral w / fromIntegral h * scale
        ca = cx + lerp (-mapping) mapping (fromIntegral x / fromIntegral w)
        cb = cy + lerp (-mapping) mapping (fromIntegral y / fromIntegral h)
        (nIter, za, zb) = iterMandelbrot ca cb 0 ca cb

        colorBasic = getColorBasic nIter
        colorContinous = getColorContinous za zb nIter


generateImageMandelbrot :: Int -> Double -> Double -> Double -> String -> String -> (String, DynamicImage)
generateImageMandelbrot w cx cy scale out color = 
    let img = ImageRGB8 (generateImage (\x y -> generateMandelbrot x y w w cx cy scale color) w w)
    in (out, img)

main :: IO ()
main = do
    -- fiksni argumenti umesto CLI
    let w = 800               -- širina slike
        cx = -0.7             -- x-koordinata centra
        cy = 0.0              -- y-koordinata centra
        scale = 1.25           -- skala
        out = "mandelbrot.png" -- izlazni fajl
        color = "purple"     -- paleta boja

    -- generisanje slike
    let (outFile, img) = generateImageMandelbrot w cx cy scale out color

    -- čuvanje slike
    savePngImage outFile img
