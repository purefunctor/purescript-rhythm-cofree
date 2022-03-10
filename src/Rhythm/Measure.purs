-- | A cofree comonad for representing streams of musical measures.
-- |
-- | This encodes a possibly infinite stream of measures given the
-- | overall tempo (BPM) of the track, as well as the individual time
-- | signatures and number of subdivisions to be made for each measure.
module Rhythm.Measure where

import Prelude

import Control.Comonad.Cofree (Cofree, tail, (:<))

-- | The parameters needed to build a `Measure`.
-- |
-- | Fields:
-- | * measureFactor - The time signature of a measure represented as a
-- |   floating point number. This is obtained by taking the fractional
-- |   representation of the time signature and dividing them.
-- |
-- |   4/4 -> 1.00
-- |   3/4 -> 0.75
-- |   8/4 -> 2.00
-- |
-- | * measureSections - The number of subsections that a measure can be
-- |   split into. Each of these subsections serve as anchor points that
-- |   notes can snap onto. For example, given a 4/4 time signature, the
-- |   value 4.0 would make it so quarter notes can be played. Likewise,
-- |   the value 8.0 would make it so that 8th notes can be played.
-- |
type MeasureParameters =
  { measureFactor :: Number
  , measureSections :: Number
  }

-- | The metadata produced by extracting a `Measure`.
-- |
-- | Fields:
-- | * currentMeasure - The current measure starting at zero.
-- |
-- | * currentTime - The current clock time in seconds.
-- |
-- | * beatLength - The time between each beat in seconds.
-- |
-- | * noteLength - The time between each note in seconds.
-- |
-- | * measureLength - The time between each measure in seconds.
-- |
-- | * beatPulse - The number of beats in a measure.
-- |
-- | * notePulse - The number of notes in a measure.
type MeasureMetaData =
  { currentMeasure :: Number
  , currentTime :: Number
  , beatLength :: Number
  , noteLength :: Number
  , measureLength :: Number
  , beatPulse :: Number
  , notePulse :: Number
  }

-- | A possibly-infinite stream of musical measures.
type Measure = Cofree (Function MeasureParameters) MeasureMetaData

-- | Create a `Measure` instance provided the tempo and the
-- | `MeasureParameters`.
makeMeasure :: Number -> MeasureParameters -> Measure
makeMeasure bpm = go { elapsedMeasure: 0.0, elapsedTime: 0.0 }
  where
  beatLength = 60.0 / bpm

  go pastMeasure { measureFactor, measureSections } =
    let
      measureLength = beatLength * 4.0 * measureFactor

      currentMeasure = pastMeasure.elapsedMeasure
      currentTime = pastMeasure.elapsedTime

      elapsedMeasure = 1.0 + currentMeasure
      elapsedTime = measureLength + currentTime

      noteLength = measureLength / measureSections

      beatPulse = 4.0 * measureFactor
      notePulse = measureSections

      presentMeasure = { elapsedMeasure, elapsedTime }
    in
      { currentMeasure
      , currentTime
      , beatLength
      , noteLength
      , measureLength
      , beatPulse
      , notePulse
      } :< go presentMeasure

-- | Advance to the next measure given the `MeasureParameters`.
nextMeasure :: Measure -> MeasureParameters -> Measure
nextMeasure = tail
