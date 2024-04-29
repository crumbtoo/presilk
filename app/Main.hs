module Main where
--------------------------------------------------------------------------------
import System.Console.Haskeline
import Control.Monad.IO.Class
import Control.Monad
import Data.Text                      qualified as T
import Data.Text                      (Text)
import Data.Char
import Text.Megaparsec.Error

import Presilk.Parse
--------------------------------------------------------------------------------

repl :: InputT IO ()
repl = getInputLine "λ> " >>= \case
    Nothing                -> quit
    Just ":q"              -> quit
    -- ignore empty input
    Just s | all isSpace s -> repl
    Just s                 -> liftIO (eval $ T.pack s) *> repl
  where quit = liftIO $ putStrLn "G'bye."

eval :: Text -> IO ()
eval s = case parse1 s of
    Left es -> putStrLn . errorBundlePretty $ es
    Right a -> print a

main :: IO ()
main = do
    putStrLn "Welcome to Presilk :3"
    putStrLn "Currently this is just a \"read parse print loop\""
    putStrLn "Type :q or send an EOF to quit"
    runInputT defaultSettings repl

