-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import qualified Data.Text.IO as T
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Parser (parseFuthark)
import System.Exit
import System.IO

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      s <- T.readFile file
      case parseFuthark file s of
        Left e -> do
          hPrint stderr e
          exitWith $ ExitFailure 2
        Right prog -> T.putStrLn $ prettyText prog
    _ -> Nothing
