import Data.List             (intercalate)
import Data.List.Split       (splitOn)
import PandocDecksAnkiImport (deckImportFile)
import PandocDecksParser     (fileDecks)
import System.Environment    (getArgs)

name f
  = if length cs == 1
    then head cs
    else intercalate "." . take (length cs - 1) $ cs
  where cs = splitOn "." f

main = do
  args  <- getArgs
  let f = case args of
            [f] -> f
            _   -> error "Usage: ankiMd FILE"
  dsE <- fileDecks f
  let d = case dsE of
            Left err -> error err
            Right [d] -> d
            _ -> error "Multiple decks are not supported yet"
  deckImportFile (name f) d
