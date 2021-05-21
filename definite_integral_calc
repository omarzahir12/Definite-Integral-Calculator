{- Assignment 5
 - Name: Mohammad Omar Zahir
 -}
module Assign_5 where

macid :: String
macid = "zahirm1"

{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - Description: 
   Finds the definite integral for a function by implementing
   the trapezoid evaluation method provided to us, in which the big
   sum notation is replaced with a recursive call that imlements the
   sum of the trapezoid evaluation until the intervals are virtually
   equal. 
   definiteIntegral uses an Auxillary function so it can store the 
   original values of the limit so that the intervals stay constant 
   in length. definiteIntegral also accounts for when the lower limit 
   a is greater than the upper limit b. 
 -}

definiteIntegral :: Double -> Double -> (Double -> Double) -> Int -> Double
definiteIntegral a b g n = definiteIntegralAux a b g n a b

definiteIntegralAux :: Double -> Double -> (Double -> Double) -> Int -> Double -> Double -> Double
definiteIntegralAux a b g n x y
    | ((b-a) <= 0.0000000000001) && ((b-a) >= -0.0000000000001) = 0
    | (b==x) = 0
    | (b<=x) = - ((a-y)/fromIntegral(n*2)) * (g (b) + g (b + (a-y)/(fromIntegral(n)))) + definiteIntegralAux  a (b + (a-y)/(fromIntegral(n))) g n x y
    | otherwise = ((b-x)/fromIntegral(n*2)) * (g (a) + g (a + (b-x)/(fromIntegral(n)))) + definiteIntegralAux (a + (b-x)/(fromIntegral(n))) b g n x y

{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - Description:
   Finds the area between the two curves, within the interval [0,1] 
   x^(1/n) and x^n, with n specified by the user input, for which the 
   area is essentially the difference of the integrals of the curve 
   x^(1/n) and x^n, in that order (f(x^(1/n)) > f(x^n)), between the 
   interval of [0,1] for each.
 -}

funH :: Integer -> Double
funH n
  | (n<=0) = error "input a value greater than 0"
  |otherwise = a - b
    where
        a = (definiteIntegral 0 1 (\x->x**(1/fromIntegral(n))) 10000)
        b = (definiteIntegral 0 1 (\x->x**fromIntegral(n)) 10000)

{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - Description:
   Finds the area between the function a^x, between the interval [-1,1] 
   where a is specified by user input, and the x-axis, which is 
   equivalent to simply computing the definite integral of the function
   with the limit [-1,1]
 -}

funK :: Double -> Double
funK a 
  |(a<=0) = error "input a value greater than 0"
  |otherwise = (definiteIntegral (-1) 1 (\x->a**(x)) 100)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 *Note - The expected outputs are what we would expect and since the 
         functions use floating point arithmetic, the answers may be
         different by a negligible amount.
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 1
 - - Input: definiteIntegral 0 5 (\x -> x) 100
 - - Expected Output: 12.5
 - - Acutal Output: 12.49999999999999
 - -----------------------------------------------------------------
 - Function: definiteIntegral
 - - Test Case Number: 2
 - - Input: definiteIntegral -5 0 (\x -> 7) 100
 - - Expected Output: 35
 - - Acutal Output: 35.000000000000064
 - -----------------------------------------------------------------
 - Function: definiteIntegral
 - - Test Case Number: 3
 - - Input: definiteIntegral 5 0 (\x -> x) 100
 - - Expected Output: -35
 - - Acutal Output: -35.000000000000064
 - -----------------------------------------------------------------
 - Function: funH
 - - Test Case Number: 1
 - - Input: 100000
 - - Expected Output: 1
 - - Acutal Output: 0.9998990005580983
 - -----------------------------------------------------------------
 - Function: funH
 - - Test Case Number: 2
 - - Input: 2
 - - Expected Output: 1/3
 - - Acutal Output: 0.33333312419714234
 - -----------------------------------------------------------------
 - Function: funH
 - - Test Case Number: 3
 - - Input: 5
 - - Expected Output: 2/3
 - - Acutal Output: 0.6666611208296516
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 1
 - - Input: 1000
 - - Expected Output: 145
 - - Acutal Output: 144.99486760021423
 - -----------------------------------------------------------------
 -   Function: funK
 - - Test Case Number: 2
 - - Input: 1
 - - Expected Output: 2
 - - Acutal Output: 2.0000000000000013
 - - ---------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 3
 - - Input: 100000
 - - Expected Output: 8724.232
 - - Acutal Output: 8724.232186850917
 -}

