{- Module provides functions to create, edit, and play playlists using a directory-based structure for organizing the playlists

   Functions:
   * displayPlaylists - Obtains the current working directory and constructs the path for the Music directory where the playlists are stored
                        Then sets the working directory to this location and calls playlistOperations to perform further operations on the playlists
   * playlistOperations - Lists all the directories within the current directory and displays them as available playlists
   * getMP3Files - Used to recursively obtain a list of MP3 files to add to a playlist
   * checkIfPlayPlaylist - Check if the user wants to play the playlsit after performing operations on it
   * getValidChoice - Recursively checks if the user's input matches one of the valid options and returns the valid input when it is found
   * yesNoOptions -  Constant list containing the valid yes or no options as strings ("y" and "n")
-}
module PlaylistOperations (displayPlaylists) where

import System.Directory (getCurrentDirectory, setCurrentDirectory, listDirectory, doesDirectoryExist, createDirectory, removeFile, doesFileExist, copyFile, removeDirectoryRecursive)
import Control.Monad (filterM)
import Control.Monad.State (evalStateT)
import System.FilePath

import MusicOperations (AppState(..), playSongs)
import MusicPath (isMp3)
import Common (universalQuit)

-- Entry point for displaying and managing playlists
displayPlaylists :: IO ()
displayPlaylists = do
  -- Obtain the current working directory
  currentDir <- getCurrentDirectory
  -- Append the Music directory to the current working directory to store playlists
  let parentDir = currentDir </> "Music"
  -- Set the working directory to the parent directory containing the playlists
  setCurrentDirectory parentDir
  -- Call the playlistOperations function to perform operations on playlists
  playlistOperations parentDir

{- playlistOperations function handles user interaction and manages playlists

   Input:
     parentDir: takes in the directory path containing the playlists

   Output:
      An IO action that performs user-specified operations on playlists, such as
      accessing, creating, editing, or removing playlists, and playing songs within
      the playlists

  Example:
  >>> playlistOperations "path/to/playlist"
  Available playlists:
  Playlist1
  Playlist2
-}
playlistOperations :: FilePath -> IO ()
playlistOperations parentDir = do
  -- List all files and directories in the current directory
  filesAndDirs <- listDirectory "."
  -- Filter out the directories (playlists)
  dirs <- filterM doesDirectoryExist filesAndDirs
  putStrLn "Available playlists:"
  mapM_ putStrLn dirs
  putStrLn "Enter the name of the playlist you want to access, or type 'new' to create a new playlist. Remember that you can input 'q' when prompted to quit!"
  choice <- getLine
  case choice of
    "new" -> do
      putStrLn "Enter the name of the new playlist:"
      playlist <- getLine
      -- Create the new folder for the new playlist
      let newFolder = parentDir </> playlist
      createDirectory newFolder
      -- Retrieve MP3 files from the user
      mp3Files <- getMP3Files []
      -- Display the new playlist creation and added files
      putStrLn $ "Creating new playlist '" ++ playlist ++ "' and adding files:"
      mapM_ putStrLn mp3Files
      -- Copy the MP3 files into the new playlist folder
      mapM_ (\f -> copyFile f (newFolder </> takeFileName f)) mp3Files
      putStrLn "Playlist created successfully!"
      -- Check if the user wants to play the new playlist
      checkIfPlayPlaylist newFolder
    _ -> do
      if choice `elem` dirs
        then do
          -- Get the path of the selected playlist
          let selectedFolder = parentDir </> choice
          -- Define the possible actions for the chosen playlist
          let options1 = ["play", "edit", "remove", "q"]
          -- Prompt user for an action and check if it is a valid action
          putStrLn "What would you like to do with this playlist? (play/edit/remove/q)"
          actionChoice <- getValidChoice options1
          case actionChoice of
            "play" -> do
              -- List the files in the selected playlist
              files <- listDirectory selectedFolder
              -- Play the songs in the playlist
              evalStateT (playSongs selectedFolder files []) AppState{currentSongFile = Nothing, audioPlayerProcess = Nothing, quit = False, songFinished = False}
            "edit" -> do
              -- Display the content of the selected playlist
              putStrLn $ "Playlist content:"
              playlistContent <- listDirectory selectedFolder
              mapM_ putStrLn playlistContent
              -- Define the possible actions for editing the playlist
              let options2 = ["add", "remove", "cancel", "q"]
              -- Prompt the user to choose an action and check if it is a valid action
              putStrLn "Do you want to add or remove a song, or cancel? (add/remove/cancel/q)"
              choice2 <- getValidChoice options2
              case choice2 of
                "add" -> do
                  -- Get new MP3 files from the user
                  newMp3Files <- getMP3Files []
                  -- Copy the new MP3 files into the selected playlist folder
                  mapM_ (\f -> copyFile f (selectedFolder </> takeFileName f)) newMp3Files
                  -- Display the added files
                  putStrLn $ "Added files to playlist '" ++ choice ++ "':"
                  mapM_ putStrLn newMp3Files
                "remove" -> do
                  putStrLn "Enter the name of the song you want to remove, or press Enter to cancel:"
                  choice3 <- getLine
                  if null choice3
                    -- If the user presses Enter, call for playlistOperations with the parentDir as argument to start over
                    then playlistOperations parentDir
                    else do
                      -- Get the path of the file to remove
                      let fileToRemove = selectedFolder </> choice3
                      -- Check if the file exists
                      exists <- doesFileExist fileToRemove
                      if exists
                        then do
                          -- Remove the file from the playlist
                          removeFile fileToRemove
                          putStrLn $ "Removed song '" ++ choice3 ++ "' from the playlist."
                        else do
                          -- Display a message if the song was not found and start over
                          putStrLn $ "Song '" ++ choice3 ++ "' not found in the playlist."
                          playlistOperations parentDir
                "cancel" -> playlistOperations parentDir
                "q" -> do
                  universalQuit
                _ -> playlistOperations parentDir
              -- Check if the user wants to play the edited playlist
              checkIfPlayPlaylist selectedFolder
            "remove" -> do
              -- Prompt the user for playlist removal confirmation
              putStrLn "Are you sure you want to remove this playlist? (y/n)"
              confirm <- getValidChoice yesNoOptions
              case confirm of
                "y" -> do
                  -- Remove the selected playlist folder recursively
                  removeDirectoryRecursive selectedFolder
                  putStrLn $ "Playlist '" ++ choice ++ "' removed successfully."
                "n" -> playlistOperations parentDir
                "q" -> universalQuit
                _ -> do
                  putStrLn "Invalid choice. Please try again:"
                  playlistOperations parentDir
            "q" -> universalQuit
            _ -> playlistOperations parentDir
        else if choice == "q" then
          -- Quit the application using universalQuit
          universalQuit
        else do
          putStrLn "Invalid playlist name. Please choose an existing playlist from the list."
          playlistOperations parentDir


{- Collects MP3 files from the user input and stores them in a list

   Input:
     files: A list of file paths (Strings) to store the collected MP3 files

   Output:
     An IO action that returns a list of MP3 file paths collected from the user

  Example:
  >>> getMP3Files []
      Enter the filepath of an MP3 file to add to the playlist, or press Enter to finish:
      /path/to/song1.mp3
      Do you want to add another file? (y/n)
      y
      Enter the filepath of an MP3 file to add to the playlist, or press Enter to finish:
      /path/to/song2.mp3
      Do you want to add another file? (y/n)
      n
      ["path/to/song1.mp3", "path/to/song2.mp3"]
-}
getMP3Files :: [FilePath] -> IO [FilePath]
getMP3Files files = do
  putStrLn "Enter the filepath of an MP3 file to add to the playlist, or press Enter to finish:"
  choice <- getLine
  case choice of
    "" -> do
      -- If the user input is empty (presses Enter), inform that there are no more files to add and return the current list of files
      putStrLn "No more files to add."
      return files
    _ -> do
      -- Construct the file path from the user input
      let filepath = takeDirectory choice </> takeFileName choice
      -- Check if the file exists and if it is an MP3 file
      exists <- doesFileExist filepath
      if exists && isMp3 filepath
        then do
          -- If the file is valid, add it to the list of files
          let newFiles = files ++ [filepath]
          putStrLn "Do you want to add another file? (y/n)"
          choice2 <- getValidChoice yesNoOptions
          case choice2 of
            "y" -> getMP3Files newFiles
            "n" -> return newFiles
            _ -> getMP3Files newFiles
        else do
          -- If the file is not valid, inform the user and prompt for another file
          putStrLn $ "The file '" ++ filepath ++ "' is not an MP3 file or does not exist."
          getMP3Files files

{- Prompts the user if they want to play the specified playlist

   Input:
     playlistPath: A path containing the directory of the playlist to play

   Output:
     An IO action that prompts the user and proceeds based on their choice

  Example:
  >>> checkIfPlayPlaylist "/path/to/playlist"
  Playlist located in /path/to/playlist
  Do you want to play the playlist? (y/n)
  ...
-}
checkIfPlayPlaylist :: FilePath -> IO ()
checkIfPlayPlaylist playlistPath = do
  putStrLn $ "Playlist located in " ++ playlistPath
  putStrLn "Do you want to play the playlist? (y/n)"
  choice <- getValidChoice yesNoOptions
  case choice of
    "y" -> do
      -- If yes, list the directory containing the songs in the playlist and play the songs
      files <- listDirectory playlistPath
      evalStateT (playSongs playlistPath files []) AppState{currentSongFile = Nothing, audioPlayerProcess = Nothing, quit = False, songFinished = False}
    "n" -> do
      -- If no, inform the user that the playlist will not be played and return to playlist operations
      putStrLn "Playlist not played."
      playlistOperations playlistPath
    _ -> do
      -- If an invalid choice is made, display an error message and retry the function
      putStrLn "Invalid choice. Please try again."
      checkIfPlayPlaylist playlistPath

{- Prompts the user for a valid input from a list of options

   Input:
     options: A list of valid String options

   Output:
     A String containing the user's valid input

  Example:
  >>> getValidChoice ["option1", "option2"]
  option1
-}
getValidChoice :: [String] -> IO String
getValidChoice options = do
  input <- getLine
  if input `elem` options
    then return input
  else do
    putStrLn "Invalid option. Please try again:"
    getValidChoice options


{- Description: The yesNoOptions list contains the standard options for yes/no user inputs

   Input: None

   Output:
     A list of Strings representing the standard yes/no options

  Example:
  >>> yesNoOptions
  ["y", "n"]
-}
yesNoOptions :: [String]
yesNoOptions = ["y", "n"]
