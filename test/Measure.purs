module Test.Measure where

import Prelude

import Control.Comonad (extract)
import Rhythm.Measure (makeMeasure, nextMeasure)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

bpm :: Number
bpm = 120.0

testMeasure :: Spec Unit
testMeasure = do
  describe "Measure" do
    it "correctly advances the time when changing factors" do
      let ms0 = makeMeasure bpm { measureFactor: 4.0 / 4.0, measureSections: 4.0 }
          ms1 = nextMeasure ms0 { measureFactor: 3.0 / 4.0, measureSections: 4.0 }
          ms2 = nextMeasure ms1 { measureFactor: 2.0 / 4.0, measureSections: 4.0 }
          ms3 = nextMeasure ms2 { measureFactor: 4.0 / 4.0, measureSections: 4.0 }
      (extract ms0).currentTime `shouldEqual` 0.0
      (extract ms1).currentTime `shouldEqual` 2.0
      (extract ms2).currentTime `shouldEqual` 3.5
      (extract ms3).currentTime `shouldEqual` 4.5
