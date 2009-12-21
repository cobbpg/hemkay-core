{-# OPTIONS_GHC -fno-warn-name-shadowing -funbox-strict-fields #-}

{-|

This module contains the routines to turn a song into a stream of
floating point samples.  Some intermediate structures are also made
available to allow more fine-grain control over playback.

-}

module Sound.Hemkay.Mixer
       ( -- * Output format
         sampleFrequency
       , Sample(..)

         -- * Player and mixer state
       , PlayState(..)
       , startState
       , ChannelState(..)
       , ChunkMixState
       , SongMixState

         -- * Sample-level mixing routines
       , prepareMix
       , mixToBuffer
       , nextSample

         -- * Mixing whole songs
       , mixSong
       , mixChunk
       , performSong
       , flattenSong
       , performTicks
       ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Data.List
--import Prelude hiding (replicate,head,concat,map,(++),take,span,null,drop,last,
--                       concatMap,zipWith,(!!),tail,scanl,zip,splitAt,length)
--import Data.List.Stream
import Data.Maybe
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Sound.Hemkay.Music
import Text.Printf

type ChunkMixState = (Int, [(WaveData, Float, Int, Float, Float, Float)])

type SongMixState = [ChunkMixState]

data Sample = Smp { leftChannel :: !Float, rightChannel :: !Float }

baseFrequency :: Float
baseFrequency = 3546894.6

-- | The frequency at which mixer output should be played back.  For
-- the time being, this is a fixed value.
sampleFrequency :: Float
sampleFrequency = 44100

-- | The state of the player upon entering a tick.
data PlayState = PS
                 { psTempo :: Int               -- ^ The current tempo
                 , psBPM :: Int                 -- ^ The current BPM
                 , psRow :: Maybe [Note]        -- ^ The current row during its first tick, 'Nothing' in subsequent ticks
                 , psChannels :: [ChannelState] -- ^ The state of the channels
                 }

instance Show PlayState where
  show (PS _ _ Nothing _) = ""
  show (PS t b (Just r) _) = printf " | %s%2d/%3d\n" (concatMap show r) t b

-- | The state of a channel upon entering a tick.
data ChannelState = CS
                    { csWaveData :: WaveData
                    , csPeriod :: Int
                    , csFineTune :: Float
                    , csSubSample :: Float
                    , csSampleStep :: Float
                    , csVolume :: Float
                    , csInstrument :: Instrument
                    , csEffect :: [Effect]
                    , csPanning :: Float
                    , csPortaDown :: Int
                    , csPortaUp :: Int
                    , csFinePorta :: Int
                    , csTonePortaEnd :: Int
                    , csTonePortaSpeed :: Int
                    , csVolumeSlide :: Float
                    , csFineVolumeSlide :: Float
                    , csVibratoSpeed :: Int
                    , csVibratoAmp :: Float
                    , csVibratoWave :: [Float]
                    , csTremoloSpeed :: Int
                    , csTremoloAmp :: Float
                    , csTremoloWave :: [Float]
                    , csTremoloDiff :: Float
                    , csDelayedPeriod :: Int
                    , csDelayedInstrument :: Instrument
                    }

instance Show ChannelState where
  show cs = printf "<%s %02d %02d %s>" (periodName (csPeriod cs)) (ident (csInstrument cs))
            (round (csVolume cs*99) :: Int) (show (csEffect cs))

-- | The initial state of the player given the number of channels.
startState :: Int -> PlayState
startState numChn = PS { psTempo = 6
                       , psBPM = 125
                       , psRow = Nothing
                       , psChannels = map chn [0..numChn]
                       }
  where chn n = CS { csWaveData = []
                   , csPeriod = 0
                   , csFineTune = 1
                   , csSubSample = 0
                   , csSampleStep = 0
                   , csVolume = 1
                   , csInstrument = emptyInstrument
                   , csEffect = []
                   , csPanning = if (n+3) `mod` 4 < 2 then 0.8 else 0.2
                   , csPortaDown = 0
                   , csPortaUp = 0
                   , csFinePorta = 0
                   , csTonePortaEnd = 0
                   , csTonePortaSpeed = 0
                   , csVolumeSlide = 0
                   , csFineVolumeSlide = 0
                   , csVibratoSpeed = 0
                   , csVibratoAmp = 0
                   , csVibratoWave = snd (head waveForms)
                   , csTremoloSpeed = 0
                   , csTremoloAmp = 0
                   , csTremoloWave = snd (head waveForms)
                   , csTremoloDiff = 0
                   , csDelayedPeriod = 0
                   , csDelayedInstrument = emptyInstrument
                   }

-- | Create a mixer state from a player state.  This basically strips
-- away a lot of unnecessary information and throws away the channels
-- that don't contribute to the output in the given chunk.
prepareMix :: PlayState -> ChunkMixState
prepareMix state = (tickLength (psBPM state), channels)
  where channels = [(wd, csSubSample cs, stepi, stepf, vol, csPanning cs) |
                    cs <- psChannels state,
                    let wd = csWaveData cs
                        vol = clampVolume (csVolume cs + csTremoloDiff cs) /
                              fromIntegral (length (psChannels state))
                        (stepi,stepf) = properFraction (csSampleStep cs),
                    not (null wd),
                    vol > 0.001]

-- | Given a pointer to a float buffer and a number of samples desired
-- (n), mix the appropriate amount of the song and return the mix
-- state for the remainder or 'Nothing' if finished.  This is the most
-- efficient way to render a song.  Note that each sample consists of
-- two floats, so the buffer has to be able to hold 2*n floats.  The
-- initial song mix state can be simply created by @'map' 'prepareMix'
-- . 'performSong'@.
mixToBuffer :: Ptr Float -> Int -> SongMixState -> IO (Maybe SongMixState)
mixToBuffer _ _ [] = return Nothing
mixToBuffer ptr len ((cnt,dat):rest) = do
  let mixLen = min len cnt
  pokeArray ptr $ replicate (mixLen*2) 0
  dat' <- forM dat $ \d@(wd,wcnt,stepi,stepf,vol,pan) ->
    if null wd then return d
    else do
      flip fix (mixLen,ptr,wd,wcnt) $ \fill (len,ptr,wd,wcnt) ->
        if len == 0 || null wd then return (wd,wcnt,stepi,stepf,vol,pan)
        else do let wsmp = head wd*vol
                    wcnt' = wcnt+stepf
                    (wd'',wcnt'') = if wcnt' < 1 then (drop stepi wd,wcnt')
                                    else (drop (stepi+1) wd,wcnt'-1)
                ml <- peek ptr
                mr <- peekElemOff ptr 1
                poke ptr (ml+wsmp*(1-pan))
                pokeElemOff ptr 1 (mr+wsmp*pan)
                fill (len-1,advancePtr ptr 2,wd'',wcnt'')
  if mixLen == len then return $ Just ((cnt-mixLen,dat'):rest)
    else mixToBuffer (advancePtr ptr (mixLen*2)) (len-mixLen) rest

-- | Mix a single sample given a chunk mix state.  Returns 'Nothing'
-- at the end of the chunk.
nextSample :: ChunkMixState -> Maybe (Sample, ChunkMixState)
nextSample (0, _) = Nothing
nextSample (cnt, dat) = cnt' `seq` dat' `seq` smp `seq` Just (smp, (cnt', dat'))
  where cnt' = cnt-1
        (smp, dat') = accum dat [] (Smp 0 0)
        accum [] cs acc = (acc,cs)
        accum (d@(wd,wcnt,stepi,stepf,vol,pan):dat) cs acc@(Smp ml mr) =
          if null wd then accum dat (d:cs) acc
          else acc' `seq` c `seq` accum dat (c:cs) acc'
            where c = wd'' `seq` wcnt'' `seq` (wd'',wcnt'',stepi,stepf,vol,pan)
                  wsmp = head wd*vol
                  acc' = Smp (ml+wsmp*(1-pan)) (mr+wsmp*pan)
                  wcnt' = wcnt+stepf
                  (wd'',wcnt'') = if wcnt' < 1 then (drop stepi wd,wcnt')
                                  else (drop (stepi+1) wd,wcnt'-1)

-- | Mix a whole song in chunks, pairing up the play states with the
-- respective chunks.
mixSong :: Song -> [(PlayState, [Sample])]
mixSong = map (id &&& mixChunk) . performSong

-- | Mix a single chunk given a play state.  It's equivalent to
-- @'unfoldr' 'nextSample' . 'prepareMix'@.
mixChunk :: PlayState -> [Sample]
mixChunk = unfoldr nextSample . prepareMix

{-
mixChunk :: PlayState -> [Sample]
mixChunk state = mixedChannels
  where mixedChannels = foldl' addChannel emptyChannel (psChannels state)
        emptyChannel = replicate (tickLength (psBPM state)) (Smp 0 0)
        addChannel mix cs = if vol < 0.0001 then mix else mixChannel mix (csWaveData cs) (csSubSample cs)
          where (stepi,stepf) = properFraction (csSampleStep cs)
                vol = clampVolume (csVolume cs + csTremoloDiff cs) * volFact
                volFact = 1 / fromIntegral (length (psChannels state))
                pan = csPanning cs
                mixChannel []             _  _    = []
                mixChannel ms             [] _    = ms
                mixChannel (Smp ml mr:ms) wd wcnt = smp `seq` smp : mixChannel ms wd'' wcnt''
                  where wsmp = head wd*vol
                        smp = Smp (ml+wsmp*(1-pan)) (mr+wsmp*pan)
                        wcnt' = wcnt+stepf
                        (wd'',wcnt'') = if wcnt' < 1 then (drop stepi wd,wcnt')
                                        else (drop (stepi+1) wd,wcnt'-1)
-}

-- | Turn a song into a series of play states, one for each tick.
-- It's a shorthand for @'performTicks' . 'flattenSong'@.
performSong :: Song -> [PlayState]
performSong = performTicks . flattenSong

-- | Turn a song into a series of pattern rows.  This includes
-- handling control structures like pattern breaks, delays and loops.
-- Order jumps are ignored.
flattenSong :: Song -> [[Note]]
flattenSong = concat . map handleLoops . map handleDelays . handleBreaks 0 . patterns
  where handleBreaks _   []         = []
        handleBreaks row (pat:pats) = (pat' ++ take 1 rest) : handleBreaks row' pats
          where (pat',rest) = span (null . getBreaks) (drop row pat)
                row' = maybe 0 (last . getBreaks) (listToMaybe (take 1 rest))
                getBreaks row = [b | [PatternBreak b] <- map effect row]

        handleDelays = concatMap (\l -> replicate (delayCount l) l)
          where delayCount row = last (1:[d | [PatternDelay d] <- map effect row])

        handleLoops pat = pat' ++ if null rest' then loop else rest''
          where (pat',rest) = span noLoopStart pat
                (loop,rest') = span (isNothing . getLoopEnd) rest
                loopLast:loopRest = rest'
                rest'' = case getLoopEnd loopLast of
                  Just cnt -> concat (replicate (cnt+1) (loop ++ [loopLast])) ++ handleLoops loopRest
                  Nothing -> loop
                noLoopStart row = null [() | [PatternLoop Nothing] <- map effect row]
                getLoopEnd row = listToMaybe [cnt | [PatternLoop (Just cnt)] <- map effect row]

-- | Turn a list of rows into a list of play states.  Each row gives
-- birth to a number of play states equal to the tempo on that row.
performTicks :: [[Note]] -> [PlayState]
performTicks flatSong = unfoldr performRow (0, startState . length . head $ flatSong, flatSong)
  where performRow (0,_,[]) = Nothing
        performRow (0,PS tempo bpm _ channels,(row:rows)) = Just (state,(tick',state',rows))
          where tempo' = last (tempo:[x | [SetTempo x] <- map effect row])
                bpm' = last (bpm:[x | [SetBPM x] <- map effect row])
                tick' = if tempo > 1 then 1 else 0
                state = PS tempo' bpm' (Just row) $ zipWith processNote row channels
                state' = advanceSamples state
        performRow (tick,PS tempo bpm _ channels,rows) = Just (state,(tick',state',rows))
          where tick' = if tick < tempo-1 then tick+1 else 0
                state = PS tempo bpm Nothing $ map (processChannel tick) channels
                state' = advanceSamples state

        advanceSamples state = state { psChannels = map advanceSample (psChannels state) }
          where tickLen = tickLength (psBPM state)
                advanceSample cs = cs { csWaveData = drop wdstep (csWaveData cs)
                                      , csSubSample = smp'
                                      }
                  where (wdstep,smp') = properFraction (csSubSample cs+csSampleStep cs*fromIntegral tickLen)

tickLength :: Int -> Int
tickLength bpm = round (sampleFrequency*2.5) `div` bpm

processNote :: Note -> ChannelState -> ChannelState
processNote (Note per ins eff) cs = cs'''
  where ins' = fromMaybe (csInstrument cs) ins
        insStays = isNothing ins || ins == Just (csInstrument cs)

        -- Handling the new note
        cs' = if per == 0
              then cs { csInstrument = ins'
                      , csVolume = if isJust ins then volume ins' else csVolume cs
                      , csFineTune = fineTune ins'
                      , csWaveData = if insStays then csWaveData cs else wave ins'
                      }
              else cs { csInstrument = ins'
                      , csVolume = volume ins'
                      , csFineTune = fineTune ins'
                      , csWaveData = case eff of
                           [SampleOffset o] -> drop o (wave ins')
                           _ -> wave ins'
                      , csPeriod = per
                      }

        -- Setting up the new effect
        cs'' = case eff of
          (TonePortamento _:_) ->
            cs' { csWaveData = if insStays then csWaveData cs else wave ins'
                , csPeriod = csPeriod cs
                , csTonePortaEnd = if per == 0 then csTonePortaEnd cs else per
                }
          (Vibrato spd amp:_) ->
            cs' { csVibratoSpeed = fromMaybe (csVibratoSpeed cs') spd
                , csVibratoAmp = maybe (csVibratoAmp cs') ((*2).fromIntegral) amp
                }
          (Tremolo spd amp:_) ->
            cs' { csTremoloSpeed = fromMaybe (csTremoloSpeed cs') spd
                , csTremoloAmp = maybe (csTremoloAmp cs') ((/64).fromIntegral) amp
                }
          [FinePanning p] -> cs' { csPanning = p }
          [SetVolume v] -> cs' { csVolume = v }
          [FinePortamento (Porta p)] -> (addPeriod cs' p) { csFinePorta = abs p }
          [FinePortamento LastUp] -> addPeriod cs' (-csFinePorta cs')
          [FinePortamento LastDown] -> addPeriod cs' (csFinePorta cs')
          [SetVibratoWaveform wf] -> cs' { csVibratoWave = (snd.fromJust) (find ((==wf).fst) waveForms) }
          [SetTremoloWaveform wf] -> cs' { csTremoloWave = (snd.fromJust) (find ((==wf).fst) waveForms) }
          [FineVolumeSlide x] -> let slide = fromMaybe (csFineVolumeSlide cs') x in
            cs' { csVolume = max 0 $ min 1 $ csVolume cs' + slide
                , csFineVolumeSlide = slide
                }
          [FineTuneControl ft] -> addPeriod cs' { csFineTune = ft } 0
          [NoteDelay _] -> if per == 0 then cs' else
            cs' { csInstrument = csInstrument cs
                , csVolume = csVolume cs
                , csFineTune = csFineTune cs
                , csWaveData = csWaveData cs
                , csDelayedPeriod = per
                , csDelayedInstrument = ins'
                }
          _ -> cs'

        -- Finalising state and handling vibrato effects
        cs''' = handleVibs (addPeriod cs'' 0) { csEffect = eff, csTremoloDiff = 0 }
        handleVibs cs = case eff of
          (Vibrato _ _:_) -> let period = clampPeriod (csPeriod cs + round (head (csVibratoWave cs) * csVibratoAmp cs)) in
            cs { csSampleStep = sampleStep period (csFineTune cs)
               , csVibratoWave = drop (csVibratoSpeed cs) (csVibratoWave cs)
               }
          (Tremolo _ _:_) ->
            cs { csTremoloDiff = head (csTremoloWave cs) * csTremoloAmp cs
               , csTremoloWave = drop (csTremoloSpeed cs) (csTremoloWave cs)
               }
          _ -> cs

processChannel :: Int -> ChannelState -> ChannelState
processChannel tick cs = foldl' addEffect cs (csEffect cs)
  where addEffect cs eff = case eff of
          Arpeggio n1 n2 ->
            cs { csSampleStep = sampleStep (csPeriod cs) (csFineTune cs) * ([1,n1,n2] !! (tick `mod` 3)) }
          Portamento (Porta p) -> (addPeriod cs p) { csPortaDown = if p > 0 then p else csPortaDown cs
                                                   , csPortaUp = if p < 0 then p else csPortaUp cs
                                                   }
          Portamento LastUp -> addPeriod cs (csPortaUp cs)
          Portamento LastDown -> addPeriod cs (csPortaDown cs)
          TonePortamento (Just p) -> targetPeriod cs { csTonePortaSpeed = p }
          TonePortamento Nothing -> targetPeriod cs
          Vibrato _ _ -> let period = clampPeriod (csPeriod cs + round (head (csVibratoWave cs) * csVibratoAmp cs)) in
                         cs { csSampleStep = sampleStep period (csFineTune cs)
                            , csVibratoWave = drop (csVibratoSpeed cs) (csVibratoWave cs)
                            }
          Tremolo _ _ -> cs { csTremoloDiff = head (csTremoloWave cs) * csTremoloAmp cs
                            , csTremoloWave = drop (csTremoloSpeed cs) (csTremoloWave cs)
                            }
          VolumeSlide x -> let slide = fromMaybe (csVolumeSlide cs) x in
            cs { csVolume = clampVolume (csVolume cs + slide)
               , csVolumeSlide = slide
               }
          RetrigNote r -> if tick `mod` r == 0 then cs { csWaveData = wave (csInstrument cs) } else cs
          NoteCut c -> if tick == c then cs { csVolume = 0 } else cs
          NoteDelay d -> if tick /= d then cs
                         else let ins = csDelayedInstrument cs in
                         flip addPeriod 0 cs { csInstrument = ins
                                             , csVolume = volume ins
                                             , csFineTune = fineTune ins
                                             , csWaveData = wave ins
                                             , csPeriod = csDelayedPeriod cs
                                             }
          _ -> cs

addPeriod :: ChannelState -> Int -> ChannelState
addPeriod cs p = cs { csPeriod = period
                    , csSampleStep = sampleStep period (csFineTune cs)
                    }
  where period = clampPeriod (csPeriod cs + p)

targetPeriod :: ChannelState -> ChannelState
targetPeriod cs = cs { csPeriod = period
                     , csSampleStep = sampleStep period (csFineTune cs)
                     }
  where period = if csPeriod cs > csTonePortaEnd cs
                then max (csTonePortaEnd cs) (csPeriod cs-csTonePortaSpeed cs)
                else min (csTonePortaEnd cs) (csPeriod cs+csTonePortaSpeed cs)

sampleStep :: Int -> Float -> Float
sampleStep p ft = baseFrequency / (fromIntegral p * sampleFrequency) * ft

clampPeriod :: Int -> Int
clampPeriod = min 1712 . max 57

clampVolume :: Float -> Float
clampVolume = max 0 . min 1
