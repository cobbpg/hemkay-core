{-|

Hemkay (An M.K. Player Whose Name Starts with an H) is a simple music
module player that performs all the mixing in Haskell.  It supports
the popular ProTracker format and some of its variations with
different numbers of channels.  This package contains all the device
independent mixing routines of the player.

Import this module to gain access to all the functionality, i.e. the
ability to load MOD files and serialise them into sound samples in
various ways.

-}

module Sound.Hemkay
       ( module Sound.Hemkay.Music
       , module Sound.Hemkay.Loader
       , module Sound.Hemkay.Mixer
       ) where

import Sound.Hemkay.Music
import Sound.Hemkay.Loader
import Sound.Hemkay.Mixer
