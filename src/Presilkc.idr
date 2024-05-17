module Presilkc
--------------------------------------------------------------------------------
import System.REPL
import System.File.Error
import Text.Show.Pretty
import Language.Presilk.Parse
import Language.Presilk.Lex
--------------------------------------------------------------------------------
%default total
--------------------------------------------------------------------------------

rep : String -> String
rep s =
    case s' of
         Left e  => "Error: " ++ e ++ "\n"
         Right e => ppShow e
  where
    s' : Either String Sexp
    s' = lexSilk >=> parseSilk $ s

silkRepl : String -> Maybe String
silkRepl ":q" = Nothing
silkRepl ""   = Just ""
silkRepl s    = Just $ rep s

covering
main : IO ()
main = do
    putStrLn greeting
    replWith () "silk> " (\a,s => (,a) <$> silkRepl s)
    putStrLn "g'bye"
  where
    greeting =
      """
      Welcome to Presilk :3
      Currently this is just a "read parse print loop"
      Type :q or send an EOF to quit
      """

