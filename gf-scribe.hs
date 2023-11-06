import PGF2
import Data.Char (toUpper)
import Database.Daison
import Scribe.AbsSyn
import Scribe.Parser
import Scribe.Lexer
import Scribe.Interpreter
import Scribe.Prelude
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import System.Environment(getArgs)
import System.Console.Haskeline
import Control.Monad.IO.Class
import Network.HTTP
import Text.JSON

main = do
  (qid:lang:args) <- getArgs
  db <- openDB "/usr/local/share/x86_64-linux-ghc-8.8.4/gf-4.0.0/www/robust/semantics.db"
  gr <- readNGF "/usr/local/share/x86_64-linux-ghc-8.8.4/gf-4.0.0/www/robust/Parse.ngf"
  let Just cnc = Map.lookup (toCnc lang) (languages gr)
  mb_prg <- case args of
              (fname:_) -> do bs <- BS.readFile fname
                              case runP pDocument bs of
                                Right prg      -> return (Just prg)
                                Left (Pn line col,msg) -> fail (show line ++ ":" ++ show col ++ ":" ++ msg)
              _         -> do return Nothing
  rsp <- simpleHTTP (getRequest ("https://www.wikidata.org/wiki/Special:EntityData/"++qid++".json"))
  case decode (rspBody rsp) >>= valFromObj "entities" >>= valFromObj qid >>= valFromObj "claims" of
    Ok obj    -> do let entity db gr [] = return (Entity [(qid,obj)])
                        env = Map.insert "entity" entity prelude
                    case mb_prg of
                      Just prg -> do val <- run env db gr cnc prg []
                                     putStrLn (showValue val "")
                      Nothing  -> do interactive env db gr cnc qid
    Error msg -> fail msg
  where
    toCnc (c:cs) = "Parse"++(toUpper c:cs)
    toCnc s      = s

interactive env db gr cnc qid = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine (qid++"> ")
      case minput of
        Nothing    -> return ()
        Just input -> case runP pDocument (BS.pack input) of
                        Right prg      -> do val <- liftIO (run env db gr cnc prg [])
                                             outputStrLn (showValue val "")
                                             loop
                        Left (pos,msg) -> do outputStrLn msg
                                             loop
