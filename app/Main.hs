-- Module used for running the program
module Main (main) where

import MusicPath (playMp3File)
import PlaylistOperations (displayPlaylists)

import Common (universalQuit)

data UserChoice = PlaySong | NavigateToPlaylists | Quit

{-  Prompts the user to choose an action, and the main function processes the user's choice and calls the corresponding functions

    Input:
      None

    Output:
      An IO action representing the user's choice and the corresponding function to be executed

    Example:
    >>> promptUser
    What would you like to do?
    1. Play a song
    2. Navigate to playlists
    3. Quit
    1
    PlaySong
-}
promptUser :: IO UserChoice
promptUser = do
  putStrLn "What would you like to do?"
  putStrLn "1. Play a song"
  putStrLn "2. Navigate to playlists"
  putStrLn "3. Quit"

  choice <- getLine

  case choice of
    "1" -> return PlaySong
    "2" -> return NavigateToPlaylists
    "3" -> return Quit
    _ -> do
      putStrLn "Invalid choice. Please choose a valid option."
      promptUser

{-  The main function is the entry point of the program 
    It prompts the user to make a choice by calling the promptUser function, and then processes the user's choice by calling the corresponding functions

    Input:
      None

    Output:
      An IO action that calls the relevant function based on the user's choice

    Example:
    >>> main
    What would you like to do?
    1. Play a song
    2. Navigate to playlists
    3. Quit
    1
    PlaySongs (playMp3File)
-}
main :: IO ()
main = do
  userChoice <- promptUser

  case userChoice of
    PlaySong -> playMp3File
    NavigateToPlaylists -> displayPlaylists
    Quit -> universalQuit
