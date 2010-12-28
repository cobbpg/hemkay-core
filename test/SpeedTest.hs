import Control.Monad.Fix
import Foreign.Marshal.Alloc
import Sound.Hemkay
import System.Environment

bufLength :: Int
bufLength = 0x10000

main :: IO ()
main = do
  args <- getArgs
  song <- loadModule (head args)
  allocaBytes (bufLength*8) $ \ptr -> do
    flip fix (map prepareMix . performSong $ song) $ \loop state -> do
      mstate' <- mixToBuffer ptr bufLength state
      case mstate' of
        Nothing -> return ()
        Just state' -> loop state'
