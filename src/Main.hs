{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- Combinatorics helpers
nextCRow :: Integer -> [Integer] -> [Integer]
nextCRow i []     = [i]
nextCRow i (j:js) = i + j : nextCRow j js

combs :: Int -> [Integer]
combs 0 = [1]
combs n = nextCRow 0 $ combs $ n - 1

coeffs :: Num a => Int -> a -> [a]
coeffs n alpha = zipWith (*) (map fromInteger $ combs n) $ zipWith (*) tpow $ reverse ltpow
 where tpow  = scanl (*) 1 $ take n $ repeat alpha
       ltpow = scanl (*) 1 $ take n $ repeat $ 1 - alpha

-- Bezier curves
data Bezier f a = Bezier [Point f a] -- points
                         a           -- begin
                         a           -- end
type instance N (Bezier f a) = a
type instance V (Bezier f a) = f
type instance Codomain (Bezier f a) = Point f

mkBezier :: (Num a) => [Point f a] -> Bezier f a
mkBezier pts = Bezier pts 0 1

instance (Additive f, Num a) => Parametric (Bezier f a) where
    atParam (Bezier pts u v) s = foldr (^+^) zero $ zipWith (*.) cfs pts
     where cfs   = coeffs n alpha
           n     = length pts - 1
           alpha = s * u + (1 - s) * v

instance (Num a) => DomainBounds (Bezier f a)
instance (Additive f, Num a) => EndValues (Bezier f a) where
    atStart (Bezier pts _ _) = head pts
    atEnd   (Bezier pts _ _) = head $ reverse pts
instance (Additive f, Fractional a) => Sectionable (Bezier f a) where
    splitAtParam (Bezier pts u v) s = (Bezier pts u mid, Bezier pts mid v)
     where mid = s * u + (1 - s) * v
    reverseDomain (Bezier pts u v) = Bezier pts v u

-- Drawing parametrics

approxParam :: ( Codomain p ~ Point (V a), N a ~ N p, TrailLike a
               , Semigroup a, Integral n, Fractional (N p), Parametric p)
            => p -> n -> a
approxParam b n = foldr1 (<>) $ zipWith (~~) pts $ tail pts
 where pts = map (atParam b) $ map (/ (fromIntegral n)) $ map fromIntegral [0..n]

estimateBezierParam :: (Integral n, RealFrac a, Floating a, Metric f) => Bezier f a -> n
estimateBezierParam (Bezier pts _ _) = ceiling $ threshold $ sum
                                     $ map norm $ zipWith (.-.) pts $ tail pts
 where threshold x = 50 * (x + 1 / (x + 1))

approxBezier :: (Semigroup a, TrailLike a, RealFrac (N a))
             => Bezier (V a) (N a) -> a
approxBezier b = approxParam b n
 where n :: Integer
       n = estimateBezierParam b

-- Main
main :: IO ()
main = mainWith diag 
 where diag :: QDiagram Cairo V2 Double Any
       diag = approxBezier bez
       bez  :: Bezier V2 Double
       bez  = mkBezier [0 ^& 0, 1 ^& 1, 2 ^& 0.5, 1 ^& 0]

