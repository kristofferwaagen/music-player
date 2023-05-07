{-  Module for common functions used by multiple modules

    Functions:
    * universalQuit - A quit function that prints text to the terminal and stops the program from running
-}
module Common (universalQuit) where

import System.Exit (exitWith, ExitCode(ExitSuccess))
-- sjekk om det er noen flere metoder som kan plasseres inn her

{-  Prints out "Quitting..." to the terminal and stops the code from running at the point it is called

    Useful for making sure that the program exits in a clean manner or when a user wants to manually quit

    Example:
    >>> universalQuit
    Quitting...
-}
universalQuit :: IO ()
universalQuit = do
  putStrLn "Quitting..."
  exitWith ExitSuccess

