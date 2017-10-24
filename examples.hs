import           Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as S
import           Text.Read         (readEither, readMaybe)

--------------------------------------------------------------------------------

double :: Int -> Int
double = (*2)

doubleLines :: IO ()
doubleLines = S.print
              . S.map double
              . S.mapMaybe readMaybe
              . S.takeWhile (not . null)
              $ S.repeatM getLine

--------------------------------------------------------------------------------

doubleLinesError :: IO ()
doubleLinesError = S.print
                   . S.map double
                   . S.stdoutLn
                   . S.map ("Not an Int: " ++)
                   . S.partitionEithers
                   . S.map readEither
                   . S.takeWhile (not . null)
                   $ S.repeatM getLine
