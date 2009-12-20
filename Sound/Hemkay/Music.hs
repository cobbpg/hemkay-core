{-|

This module contains the song structure definitions.

-}

module Sound.Hemkay.Music
       ( -- * Overall song structure
         Song(..)
       , numChannels

         -- * Pattern structure
       , Pattern
       , Note(..)
       , periodName
       , Effect(..)
       , PortaParam(..)
       , Waveform(..)
       , waveForms

         -- * Instruments
       , Instrument(..)
       , emptyInstrument
       , WaveData

       ) where

import Data.Maybe
import Text.Printf

data Song = Song
            { title :: String             -- ^ Song title.
            , instruments :: [Instrument] -- ^ Instruments.
            , patterns :: [Pattern]       -- ^ Patterns in the order of playback.
            }

-- | The number of channels in a song.
numChannels :: Song -> Int
numChannels = length . head . head . patterns

instance Show Song where
  show (Song t smps pats) = unlines [line |
                                     block <- [["Title: " ++ t,""],
                                               map show smps,
                                               "":map showPattern pats
                                              ],
                                     line <- block]

data Instrument = Instrument
                  { ident :: Int      -- ^ Instrument number, needed for equality check.
                  , name :: String    -- ^ Instrument name.
                  , wave :: WaveData  -- ^ List of samples; infinite for looped instruments.
                  , volume :: Float   -- ^ Default volume (0..1).
                  , fineTune :: Float -- ^ Fine tune (-log_12 2..log_12 2).
                  }

type WaveData = [Float]

instance Eq Instrument where
  i1 == i2 = ident i1 == ident i2

instance Show Instrument where
  show (Instrument _ n dat vol ft) = "Instrument: " ++ show n ++
                                     ", samples: " ++ show (take 5 dat) ++
                                     ", volume: " ++ show vol ++
                                     ", finetune: " ++ show ft

-- | A silent instrument that's not equal to any other in a loaded
-- song.
emptyInstrument :: Instrument
emptyInstrument = Instrument
                  { ident = 0
                  , name = ""
                  , wave = []
                  , volume = 0
                  , fineTune = 1
                  }

type Pattern = [[Note]]

showPattern :: Pattern -> String
showPattern pat = unlines ["| " ++ concatMap show line | line <- pat]

data Note = Note
            { period :: Int                  -- ^ Period of the note (0 for none); the corresponding frequency is 3546894.6/period.
            , instrument :: Maybe Instrument -- ^ The instrument of the note, if any.
            , effect :: [Effect]             -- ^ Special effects, at most two per note.
            }

instance Show Note where
  show (Note p i e) = printf "%s %s %s | " (periodName p) (maybe ".." (printf "%02d" . ident) i) (show e)

periodName :: Int -> [Char]
periodName 0 = "..."
periodName p = head $ [str ++ show oct |
                       (oct,pers) <- zip [0 :: Int ..] periodTable,
                       (per,str) <- zip pers noteNames,
                       per == p] ++ [printf "%3d" (p `mod` 1000)]

-- | The possible waveforms of the vibrato and tremolo effects.
data Waveform = SineWave | SawtoothWave | SquareWave deriving Eq

instance Show Waveform where
  show SineWave = "sin"
  show SawtoothWave = "swt"
  show SquareWave = "sqr"

data PortaParam = LastUp | LastDown | Porta Int deriving Show

data Effect = Arpeggio Float Float            -- ok
            | Portamento PortaParam           -- ok
            | TonePortamento (Maybe Int)      -- ok
            | Vibrato (Maybe Int) (Maybe Int) -- ok
            | Tremolo (Maybe Int) (Maybe Int) -- ok
            | FinePanning Float               -- ok
            | SampleOffset Int                -- ok
            | VolumeSlide (Maybe Float)       -- ok
            | OrderJump Int                   -- no plans to support
            | SetVolume Float                 -- ok
            | PatternBreak Int                -- ok
--            | SetFilter Int                 -- no plans to support
            | FinePortamento PortaParam       -- ok
--            | GlissandoControl Int          -- no plans to support
            | SetVibratoWaveform Waveform     -- ok
            | FineTuneControl Float           -- ok
            | PatternLoop (Maybe Int)         -- ok
            | SetTremoloWaveform Waveform     -- ok
            | RetrigNote Int                  -- ok
            | FineVolumeSlide (Maybe Float)   -- ok
            | NoteCut Int                     -- ok
            | NoteDelay Int                   -- ok
            | PatternDelay Int                -- ok
--            | FunkRepeat                    -- no plans to support
            | SetTempo Int                    -- ok
            | SetBPM Int                      -- ok

instance Show Effect where
  show (Arpeggio a1 a2) = printf "arp %x %x" (unhalf a1) (unhalf a2)
  show (Portamento LastUp) = "por ^^^"
  show (Portamento LastDown) = "por vvv"
  show (Portamento (Porta p)) = printf "por %3d" p
  show (TonePortamento Nothing) = "ton ..."
  show (TonePortamento (Just p)) = printf "ton %3d" p
  show (Vibrato amp spd) = printf "vib %x %x" (fromMaybe 0 amp) (fromMaybe 0 spd)
  show (Tremolo amp spd) = printf "trm %x %x" (fromMaybe 0 amp) (fromMaybe 0 spd)
  show (FinePanning p) = printf "<=> %3d" (round (p*99) :: Int)
  show (SampleOffset o) = printf "ofs $%02x" (o `div` 256)
  show (VolumeSlide Nothing) = "vsl ..."
  show (VolumeSlide (Just s)) = printf "vsl %3d" (round (s*99) :: Int)
  show (OrderJump o) = printf "ord %3d" o
  show (SetVolume v) = printf "vol %3d" (round (v*99) :: Int)
  show (PatternBreak b) = printf "brk %3d" b
  show (FinePortamento LastUp) = "por!^^^"
  show (FinePortamento LastDown) = "por!vvv"
  show (FinePortamento (Porta p)) = printf "por!%3d" p
  show (SetVibratoWaveform w) = "vib " ++ show w
  show (FineTuneControl ft) = printf "fin %x  " (unhalf ft)
  show (PatternLoop Nothing) = "lop beg"
  show (PatternLoop (Just c)) = printf "lop %3d" c
  show (SetTremoloWaveform w) = "trm " ++ show w
  show (RetrigNote r) = printf "ret %3d" r
  show (FineVolumeSlide Nothing) = "vsl!..."
  show (FineVolumeSlide (Just s)) = printf "vsl!%3d" (round (s*99) :: Int)
  show (NoteCut c) = printf "cut %3d" c
  show (NoteDelay d) = printf "ndl %3d" d
  show (PatternDelay d) = printf "pdl %3d" d
  show (SetTempo t) = printf "tmp %3d" t
  show (SetBPM b) = printf "bpm %3d" b

  showList [] = showString "       "
  showList [eff] = shows eff
  showList [TonePortamento _, VolumeSlide Nothing] = showString "tvs ---"
  showList [TonePortamento _, VolumeSlide (Just s)] = showString (printf "tvs %3d" (round (s*99) :: Int))
  showList [Vibrato _ _, VolumeSlide Nothing] = showString "vvs ---"
  showList [Vibrato _ _, VolumeSlide (Just s)] = showString (printf "vvs %3d" (round (s*99) :: Int))
  showList _ = showString "???????"

unhalf :: Float -> Int
unhalf x = round (log x / log 2 * 12)

periodTable :: [[Int]]
periodTable = [[1712, 1616, 1525, 1440, 1375, 1281, 1209, 1141, 1077, 1017,  961,  907],
               [ 856,  808,  762,  720,  678,  640,  604,  570,  538,  508,  480,  453],
               [ 428,  404,  381,  360,  339,  320,  302,  285,  269,  254,  240,  226],
               [ 214,  202,  190,  180,  170,  160,  151,  143,  135,  127,  120,  113],
               [ 107,  101,   95,   90,   85,   80,   76,   71,   67,   64,   60,   57]]

noteNames :: [String]
noteNames = ["C-", "C#", "D-", "D#", "E-", "F-", "F#", "G-", "G#", "A-", "A#", "B-"]

-- | Waveforms needed for vibrato and tremolo effects.  The lists are
-- infinite.
waveForms :: [(Waveform, [Float])]
waveForms = [(SineWave, cycle [sin (v*pi/32) | v <- [0..64]])
            ,(SawtoothWave, cycle [(v+0.5)/31.5 | v <- [-32..31]])
            ,(SquareWave, cycle (replicate 32 (-1) ++ replicate 32 1))
            ]
