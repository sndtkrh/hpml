import System.IO
import PML.Command

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  interactive emptyEnv

interactive :: Proved -> IO ()
interactive env = do
  putStr ">> "
  cstr <- getLine
  let mc = cparse cstr
  case mc of
    Just Q -> putStrLn "Quit."
    Just (Comment _) -> interactive env
    Just c -> case run c env of
      Just env'@(Proved thms _) -> do
        putStrLn $ "#" ++ show (length thms - 1) ++ " |- " ++ show (head thms)
        interactive env'
      Nothing -> do
        putStrLn "Error"
        interactive env
    Nothing -> do
      putStrLn "Error"
      interactive env
