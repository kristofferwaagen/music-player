{-  Module for defining the SongInfo data type and extracting song information from the loaded mp3 file

    Types:
    * SongInfo - A record type with fields for song title and artist

    Functions:
    * getSongInfo - Extracts the song title and artist from a mp3 files and returns them as a SongInfo record
    * extractArtistTitle - Splits a file name into artist and title components and returns them as a tuple
-}
module SongDetails (SongInfo(..), getSongInfo, extractArtistTitle) where

import Data.Text (Text, pack)
import System.FilePath (takeFileName)

data SongInfo = SongInfo
  { title :: Text
  , artist :: Text
  } deriving (Show)

{-  Extracts the song title and artist from a file name and returns them as a SongInfo record

    Input:
      file: the path to the file to be checked

    Output:
      A SongInfo record with the song title and artist extracted from the file name

    Example:
    >>> getSongInfo "path/to/artist-title.mp3"
    SongInfo {title = "title", artist = "artist"}
-}
getSongInfo :: FilePath -> IO SongInfo
getSongInfo file = do
  -- Extract the file name from the given file path
  let fileName = takeFileName file
  -- Use the extractArtistTitle function to get the artist and title components of the file name
      (artistName, titleName) = extractArtistTitle fileName
  -- Return a SongInfo record with the title and artist fields filled using the Text.pack function
  return $ SongInfo (pack titleName) (pack artistName)

{-  Splits a file name into artist and title components and returns them as a tuple

    Input:
      fileName: the name of the file to be split

    Output:
      A tuple containing the artist and title components of the file name

    Example:
    >>> extractArtistTitle "artist-title.mp3"
    ("artist", "title")
-}
extractArtistTitle :: String -> (String, String)
extractArtistTitle fileName = (takeWhile (/= '-') fileName, drop 1 $ dropWhile (/= '-') fileName)
