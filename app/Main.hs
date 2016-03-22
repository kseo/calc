module Main where

import Calculator
import Data.Bifunctor
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          case evalString input of
            Left msg -> outputStrLn $ show msg
            Right exp -> outputStrLn $ show exp
          loop
