{- Module provides functions to play songs from a folder, handle user inputs, and maintain the state of the application.

   Data types:
   * AppState - Represents the application state, including the current song, audio player process, and control flags.

   Functions:
   * playSongs - Plays songs from a folder, handles user inputs to control song playback, and maintains application state.
-}
{-# LANGUAGE OverloadedStrings #-}
module MusicOperations (AppState(..), playSongs) where

import Control.Monad.State
import Data.Text.IO as TIO
import System.FilePath
import System.Process
import System.Timeout (timeout)
import System.Exit (ExitCode(ExitSuccess))

import SongDetails
import Audio (playAudio)
import Common (universalQuit)

data AppState = AppState
  { currentSongFile :: Maybe FilePath
  , audioPlayerProcess :: Maybe ProcessHandle
  , quit :: Bool
  , songFinished :: Bool
  }

type App = StateT AppState IO

instance Show AppState where
  show (AppState { currentSongFile = Just file, quit = q }) =
    "Now playing: " ++ file ++ "\n" ++
    "Available inputs:\n" ++
    "s: Skip to the next song\n" ++
    "b: Go back to the previous song\n" ++
    "q: Quit the program\n" ++
    "Quit: " ++ show q
  show (AppState { currentSongFile = Nothing, quit = q }) =
    "No song currently playing.\n" ++
    "Available inputs:\n" ++
    "s: Skip to the next song\n" ++
    "q: Quit the program\n" ++
    "Quit: " ++ show q


{-  Takes a folder path, a list of song files, and a list of played song files as arguments
    It plays the songs from the folder and handles user inputs to control the playback

    Input:
      folderPath: the path to the folder containing the songs
      (file:files): a list of song's filepaths to be played
      played: a list of songs that has been played

    Output:
       A modified application state based on user inputs and song playback status

    Example:
    >>> playSongs "path/to/songs" ["song1.mp3", "song2.mp3"] []
-}
playSongs :: FilePath -> [FilePath] -> [FilePath] -> App ()
playSongs _ [] _ = liftIO $ Prelude.putStrLn "No more songs to play."
playSongs folderPath (file:files) played = do
  -- Get the song information for the current song file
  songInfo <- liftIO $ getSongInfo (folderPath </> file)
  -- Modify the current AppState with the current song file
  modify $ \s -> s {currentSongFile = Just file, audioPlayerProcess = Nothing, songFinished = False}
  liftIO $ TIO.putStrLn $ "Now playing: " <> title songInfo <> " by " <> artist songInfo

  -- Start playing the mp3 file using playAudio
  procHandle <- liftIO $ playAudio (folderPath </> file) (return ())
  -- Modify the application state to store the process handle of the audio player
  modify $ \s -> s {audioPlayerProcess = Just procHandle}
  printInputs

  -- Function to process user input and control playback
  let processInput userInput = do
        -- Wait for the audio player process to finish with a timeout of 1 second, used to simulate automation of switching songs
        maybeExitCode <- liftIO $ timeout 1000000 (waitForProcess procHandle)
        case maybeExitCode of
          -- Check if the exit code succeeds
          Just ExitSuccess -> do
            -- If the exit code is success, modify application state and proceed to next song (should work in theory)
            modify $ \s -> s { songFinished = True }
            nextSong folderPath files (file : played)
          _ -> case userInput of
            "s" -> nextSong folderPath files (file : played) -- Go to next song
            "b" -> handlePreviousSong played folderPath files file -- Go to previous played song
            "q" -> terminationProcess -- Quit the program
            _ -> processInput =<< liftIO Prelude.getLine -- Prompts user if the input is invalid

  -- Call processInput with the user's input
  processInput =<< liftIO Prelude.getLine
  -- Get the updated AppState
  appState <- get
  {- Continue to the next song based on the application state

     unless: control structure that executes if the condition is False
     (quit appState): checks if quit in appState is set to True or False, if True -> program stops playing music and exits, if False -> continue playing songs
     nextSong folderPath files (if songFinished appState then file : played else played):
     - Calls nextSong to start the next song
     - folderPath: path to the folder containing the remaining songs
     - files: the list of songs that has been played
     - (if songFinished appState then file : played else played): checks if songFinished in appState is True or False, 
        if True -> adds the current song to the played list, if False -> keeps the played list unchanged
  -}
  unless (quit appState) $ nextSong folderPath files (if songFinished appState then file : played else played)

-- Prints the valid inputs the user can use
printInputs :: App ()
printInputs = do
  liftIO $ Prelude.putStrLn "Available inputs:"
  liftIO $ Prelude.putStrLn "s: Skip to the next song (you need to press s to play the next song)"
  liftIO $ Prelude.putStrLn "b: Go back to the previous song"
  liftIO $ Prelude.putStrLn "q: Quit the program"

{- Stops the audio player process, if there is a process running and updates the application state

    Input:
      markAsPlayed: indicator of wheter the song should be considered as played or not (True/False)
      appState: current AppState
    Output:
       A modified App state with songs marked as played

    Example:
    >>> stopAudioPlayer (songFinished) appState
-}
stopAudioPlayer :: Bool -> AppState -> App ()
stopAudioPlayer markAsPlayed appState = do
  -- Checks if there is an active audio player process
  case audioPlayerProcess appState of
    Just procHandle -> do
      -- If there is a process, stop it
      liftIO $ terminateProcess procHandle
      -- Update the AppState with values Nothing (indicating that there is no process running) and markAsPlayed (indicating wheter the song is considered played or not)
      modify $ \s -> s { audioPlayerProcess = Nothing, songFinished = markAsPlayed }
    -- If there is no process, return ()
    Nothing -> return ()

-- Terminates the audio player process, if running, and updates the application state to quit
-- It also calls universalQuit to handle any additional quitting logic
terminationProcess :: App ()
terminationProcess = do
  -- Gets the running process
  maybeProcHandle <- gets audioPlayerProcess
  case maybeProcHandle of
    -- If there is a process running, stop it
    Just procHandle -> liftIO $ terminateProcess procHandle
    -- If there are none, return ()
    Nothing -> return ()
  -- Mark the AppState value as True (indicating that the program is exited)
  modify $ \s -> s { quit = True }
  -- Call for the qutting logic used in universalQuit
  liftIO $ universalQuit

{- nextSong transitions to the next song in the playlist
   It stops the audio player, updates the application state, and checks if there are any more songs left in the playlist
   If the playlist is empty, it asks the user whether to restart from the beginning or quit the program

    Input:
      folderPath: path to the playlist folder
      files: list of songs to be played
      played: list of songs that has been played
    Output:
       A modified App state where the next song is playing

    Example:
    >>> nextSong "path/to/songs" ["song1.mp3", "song2.mp3"] []
-}
nextSong :: FilePath -> [FilePath] -> [FilePath] -> App ()
nextSong folderPath files played = do
  appState <- get
  -- Stops the current audio player process using stopAudioPlayer
  stopAudioPlayer (songFinished appState) appState
  if null files
    -- Checks if there are no more songs left in the playlist
    then do
      -- If there are none, prompt the user with the choice of restarting the playlist or not
      liftIO $ Prelude.putStrLn "End of playlist reached. Restart from the beginning (y/n)?"
      choice <- liftIO Prelude.getLine
      case choice of
        "y" -> do
          -- If they chose to start over, print to terminal and call for playSongs with the same:
          -- path, reversed played (so that it plays in the same order as original playlist) and an empty list
          liftIO $ Prelude.putStrLn "Restarting playlist"
          playSongs folderPath (reverse played) []
        -- Else start the quitting process
        _ -> terminationProcess
      -- If there are songs left in the list, play the next
    else playSongs folderPath files played

{- handlePreviousSong transitions to the previous song in the playlist
   If there are no previous songs played, it restarts the current song

    Input:
      played: list of songs that has been played
      folderPath: path to the playlist folder
      files: list of songs to be played
      file: current song
    Output:
       A modified App state where the previous song is playing

    Example:
    >>> handlePreviousSong ["song1.mp3"] "path/to/songs" ["song2.mp3", "song3.mp3"] "song1.mp3"
-}
handlePreviousSong :: [FilePath] -> FilePath -> [FilePath] -> FilePath -> App ()
handlePreviousSong played folderPath files file = do
  -- Gets current application state
  appState <- get
  -- Retrieves songFinished flag from application state
  markAsPlayed <- gets songFinished
  -- Stops the current audio player process
  stopAudioPlayer markAsPlayed appState
  case played of
    [] -> do
      -- If there are no songs in played list, prompt a message for user and start the current song over
      liftIO $ Prelude.putStrLn "No previous songs played. Restarting the song."
      playSongs folderPath (file:files) played
    (prev:rest) -> do
      -- If there is a song in played list, play the previous song and put it back into the files list
      playSongs folderPath (prev : file : files) rest

