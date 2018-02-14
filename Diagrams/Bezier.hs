{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}

-- |A library for drawing arbitrary degree bezier curves with Diagrams
module Diagrams.Bezier
     ( Bezier(Bezier,bez_controls,bez_begin,bez_end)
     , mkBezier
     , approxParam
     , estimateBezierParam
     , approxBezier
     , bezier_spec
     ) where

import Diagrams.Prelude hiding ((===))
import Test.QuickCheck
import Test.Hspec

-- Combinatorics helpers

-- |Given a list of the numbers of subsets of cardinal i in n,
-- computes the list of numbers of subsets of cardinal i in n+1.
nextCRow :: [Integer] -- ^List of the number of subsets of a set of cardinal n
         -> [Integer] -- ^List of the number of subsets of a set of cardinal n+1
nextCRow l = zipWith (+) (0:l) $ l ++ [0]

-- |Gives the list of number of subsets of cardinal i in n
combs :: Int       -- ^n
      -> [Integer] -- ^List of the number of subsets of a set of cardinal n
combs 0 = [1]
combs n = nextCRow $ combs $ n - 1

-- |Property to check the validity of combs
prop_combs :: Positive Int -> NonNegative Int -> Property
prop_combs ci cn = withMaxSuccess 1000 $ i <= n ==> c (i-1) n + c i n === c i (n+1)
 where i = getPositive ci
       n = getNonNegative cn
       c k m = (combs m) !! k

-- |Computes the coefficients of a n-degree bezier curve, where
-- 'alpha' is the index between 0 and 1
coeffs :: Num a => Int -> a -> [a]
coeffs n alpha = zipWith (*) (map fromInteger $ combs n) $ zipWith (*) tpow $ reverse ltpow
 where tpow  = scanl (*) 1 $ take n $ repeat alpha
       ltpow = scanl (*) 1 $ take n $ repeat $ 1 - alpha

-- |A generator for numbers between 0 and 1
newtype Unit a = Unit a deriving (Show)
getUnit :: Unit a -> a
getUnit (Unit x) = x
instance (Arbitrary a, Fractional a) => Arbitrary (Unit a) where
    arbitrary = do
        x <- (fmap abs) arbitrary
        return $ Unit $ 1 / (x + 1)

-- |The sum of the coefficients is always 1
prop_coeffs_sum :: Positive Int -> Unit Double -> Bool
prop_coeffs_sum cn calpha = abs ((sum (coeffs n alpha)) - 1) <= epsilon
 where n       = getPositive cn
       alpha   = getUnit calpha
       epsilon = 1e-6

-- |The coefficients are all positives
prop_coeffs_positive :: Positive Int -> Unit Double -> Bool
prop_coeffs_positive cn calpha = all (\x -> x >= 0) $ coeffs n alpha
 where n       = getPositive cn
       alpha   = getUnit calpha

-- |A new line of coefficients is obtained by combining the previous line with itself
prop_coeffs :: Positive Int -> Unit Double -> Bool
prop_coeffs cn calpha = all (\x -> abs x <= epsilon) $ zipWith (-) ncfs1 ncfs2
 where n       = getPositive cn
       alpha   = getUnit calpha
       epsilon = 1e-6
       cfs     = coeffs n alpha
       cfs1    = map (* alpha)     $ 0 : cfs
       cfs2    = map (* (1-alpha)) $ cfs ++ [0]
       ncfs1   = zipWith (+) cfs1 cfs2
       ncfs2   = coeffs (n+1) alpha

-- Bezier curves
-- |Stores a portion of a n-degree bezier curve
-- 'f' is the supporting vector space and 'a' the scalar type
data Bezier f a = Bezier
                { bez_controls :: [Point f a] -- ^Control points
                , bez_begin    :: a           -- ^Beginning of the portion, between 0 and 1
                , bez_end      :: a           -- ^End of the protion, between 0 and 1
                }
type instance N (Bezier f a) = a
type instance V (Bezier f a) = f
type instance Codomain (Bezier f a) = Point f

-- |Creates a complete bezier curves from a list fo control points
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

-- |Approximates a paramteric curve by segments, splitting the
-- control space in 'n' chunks
-- TODO: use BomainBounds instance for p
approxParam :: ( Codomain p ~ Point (V a), N a ~ N p, TrailLike a
               , Semigroup a, Integral n, Fractional (N p), Parametric p)
            => p -> n -> a
approxParam b n = foldr1 (<>) $ zipWith (~~) pts $ tail pts
 where pts = map (atParam b) $ map (/ (fromIntegral n)) $ map fromIntegral [0..n]

-- |Estimate in how many chunks should the control space be split for a bezier curve.
-- This is done by majorating the length of the curve by the distance between the controls
-- points
estimateBezierParam :: (Integral n, RealFrac a, Floating a, Metric f) => Bezier f a -> n
estimateBezierParam (Bezier pts _ _) = ceiling $ threshold $ sum
                                     $ map norm $ zipWith (.-.) pts $ tail pts
 where threshold x = 50 * (x + 1 / (x + 1))

-- |Approximate a bezier curves by splitting it in segments according to 'estimateBezierParam'
approxBezier :: (Semigroup a, TrailLike a, RealFrac (N a))
             => Bezier (V a) (N a) -> a
approxBezier b = approxParam b n
 where n :: Integer
       n = estimateBezierParam b

bezier_spec :: Spec
bezier_spec = do
    describe "Binomial coefficients" $ do
        it "prop_combs" $ property prop_combs
    describe "Bezier coefficients" $ do
        it "prop_coeffs_sum"      $ property prop_coeffs_sum
        it "prop_coeffs_positive" $ property prop_coeffs_positive
        it "prop_coeffs"          $ property prop_coeffs

