import System.IO
import Control.Monad
import qualified Data.Text.IO as TIO
import qualified Data.Text as DT

import Lexer
import TypeStack

{-main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  putStrLn $ show $ P "I32" `is_subtype` P "Num"
  show <$> (parse <$> make_types <$> DT.words <$> TIO.getLine) >>= putStrLn-}

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  c <- getLine
  putStrLn $ show $ run c
