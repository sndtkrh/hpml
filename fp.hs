import System.IO
import Control.Monad as M
import PML.Parser
import PML.Formulaproperties
import PML.StandardTranslation as ST

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  interactive

interactive :: IO ()
interactive = do
  putStr ">> "
  cstr <- getLine
  let fml = parse cstr
  case fml of
    (Just f) -> do
      putStrLn "OK."
      putStrLn $ "The properties of " ++ show f
      putStrLn $ "Standard translation for model is " ++ show (ST.modelST f)
      M.when (isPositive f) $ putStrLn "Positive."
      M.when (isNegative f) $ putStrLn "Negative."
      M.when (isVerySimpleShalqvist f) $ putStrLn "Very simple Shalqvist formula."
      interactive
    Nothing -> do
      putStrLn "ERROR"
      interactive
