{-  Provides a function for playing an audio file using the VLC media player.

    Functions:
    * playAudio - Plays an audio file using VLC and calls a callback function when the audio has finished playing
    
    Example:
    >>> playAudio "path/to/file.mp3" (\_ -> putStrLn "Finished playing!")
-}
module Audio (playAudio) where

import System.Process (ProcessHandle, createProcess, proc, waitForProcess)
import Control.Concurrent (forkIO)

{-  Plays an audio file using VLC and calls a callback function when the audio has finished playing.

    Input:
      audioFile: the path to the audio file to be played.
      callback: a function to be called when the audio has finished playing.

    Output:
      A handle to the VLC process.

    Example usage:
    >>> playAudio "path/to/file.mp3" (\_ -> putStrLn "Finished playing!")
-}
playAudio :: FilePath -> IO () -> IO ProcessHandle
playAudio audioFile callback = do
    -- Use createProcess to spawn a new VLC process and makes a handle for it
    -- this process starts playing the audioFile when spawned
    (_, _, _, handle) <- createProcess (proc vlcPath ["--intf", "dummy", "--quiet", "--no-video", audioFile])
    -- Use forkIO to to create a new thread that waits for the previous VLC process to end
    -- after it has ended, call the function in the callback
    _ <- forkIO $ do
        _ <- waitForProcess handle
        callback
    -- Returns the handle the VLC process is stored in, to interact with later
    return handle

-- The path to the VLC media player
vlcPath :: FilePath
vlcPath = "C:\\Program Files\\VideoLAN\\VLC\\vlc.exe"
