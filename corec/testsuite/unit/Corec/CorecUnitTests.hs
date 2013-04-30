import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Corec.COR

main = defaultMain corecUnitTests

corecUnitTests = 
  [
    testGroup "showing a theFibs fail" [
      testCase "should fail" on_purpose
    ],
    testGroup "showing theFibs pass" [
      testCase "should return [0,1,1]" one_is_entered,
      testCase "should return Fizz" three_is_entered
    ] 
  ]

on_purpose = take 10 theFibs @?= [0,1,1,2,3,5,8,13,21,34]
one_is_entered = [0,1,1] @=? take 3 theFibs
three_is_entered = assertEqual "checking that Fizz works" [0,1,1] $ take 3 theFibs