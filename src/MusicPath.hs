{-  Module for playing mp3 files and checking if the provided file is indeed a mp3 file

    Functions:
    * playMp3File - Prompts the user for the path to a mp3 file and plays it using playSongs from MusicOperations
    * isMp3 - Checks if a given file has the extension .mp3.
-}
module MusicPath (
    playMp3File,
    isMp3
) where

import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import Control.Monad (when)
import Control.Monad.State (evalStateT)
import MusicOperations (AppState(..), playSongs)

{-  Prompts the user for the path to a MP3 file and plays it using playSongs

    Example:
    >>> playMp3File
    Enter the path to the mp3 file:
    path/to/file.mp3
-}
playMp3File :: IO ()
playMp3File = do
    putStrLn "Enter the path to the mp3 file:"
    inputPath <- getLine
    -- Checks if the provided file exists in the given path and that it is a mp3 file
    isFile <- doesFileExist inputPath
    mp3Files <- if isFile && isMp3 inputPath
                then return [inputPath]
                else return []

    -- If the file is not located in the path or it is not a mp3 file, print an error to the terminal
    when (null mp3Files) $ putStrLn "Invalid file path or no mp3 files found."

    -- If the file exists and it is an mp3, use playSongs to start the playback
    evalStateT (playSongs inputPath mp3Files []) AppState{currentSongFile = Nothing, audioPlayerProcess = Nothing, quit = False, songFinished = False}

{-  Checks if a given file has the extension mp3

    Input:
      file: the path to the file to be checked

    Output:
      True if the file has the extension .mp3, False otherwise

    Example:
    >>> isMp3 "path/to/file.mp3"
    True
    >>> isMp3 "path/to/file.txt"
    False
-}
isMp3 :: FilePath -> Bool
isMp3 file = takeExtension file == ".mp3"
