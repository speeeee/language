import System.IO
import Control.Monad
import qualified Data.Text.IO as TIO
import qualified Data.Text as DT

import Lexer
import TypeStack

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  show <$> (parse (LS types_init funs_init st_init) <$> make_types <$> DT.words <$> TIO.getLine)
    >>= putStrLn

{-main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  c <- getLine
  putStrLn $ show $ run c-}
