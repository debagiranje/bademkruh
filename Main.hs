module Main where

import Codec.Picture

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
lerpColor (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) t =
    PixelRGB8 (lerpPx r1 r2 t) (lerpPx g1 g2 t) (lerpPx b1 b2 t)

getColorContinous :: Double -> Double -> Int -> [PixelRGB8] -> PixelRGB8
getColorContinous za zb nIter colors = colorContinous
    where
        log_zn = log(za * za + zb * zb) / 2
        nu = logBase 2 (log_zn / log 2)
        fracNIter = fromIntegral nIter - nu
        color1 = colors !! (floor fracNIter `mod` length colors)
        color2 = colors !! ((floor fracNIter + 1) `mod` length colors)
        (_, t) = properFraction fracNIter
        colorContinous = lerpColor color1 color2 t

generateMandelbrot :: Int -> Int -> Int -> Int -> Double -> Double -> Double -> String -> PixelRGB8
generateMandelbrot x y w h cx cy scale color | nIter == maxIterations = PixelRGB8 0 0 0
                                             | color == "continuous" = colorContinous colorsPurple
                                             | color == "purple" = colorContinous colorsPurple
                                             | otherwise = error "Invalid color"
    where
        mapping = fromIntegral w / fromIntegral h * scale
        ca = cx + lerp (-mapping) mapping (fromIntegral x / fromIntegral w)
        cb = cy + lerp (-mapping) mapping (fromIntegral y / fromIntegral h)
        (nIter, za, zb) = iterMandelbrot ca cb 0 ca cb

        colorContinous = getColorContinous za zb nIter

generateImageMandelbrot :: Int -> Double -> Double -> Double -> String -> String -> (String, DynamicImage)
generateImageMandelbrot w cx cy scale out color = 
    let img = ImageRGB8 (generateImage (\x y -> generateMandelbrot x y w w cx cy scale color) w w)
    in (out, img)

interestingPoints :: [(Double, Double, Double, String)]
interestingPoints =
    [ (-0.75, 0.0,     1.25, "center")
    , (-0.75, 0.1,     0.01, "spiral")
    , (-1.25, 0.0,     0.03, "antenna")
    , (-0.745, 0.105,  0.002, "seahorse_valley")
    , (0.001643721971153, 0.822467633298876, 0.0000005, "deep_zoom")
    ]

main :: IO ()
main = do
    let w = 800
        color = "continuous" 

    mapM_ (\(cx, cy, scale, name) -> do
        let out = "mandelbrot_" ++ name ++ ".png"
            (outFile, img) = generateImageMandelbrot w cx cy scale out color
        savePngImage outFile img
        ) interestingPoints
