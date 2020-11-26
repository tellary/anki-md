{-# LANGUAGE OverloadedStrings #-}

module PandocDecksAnkiImport where

import           Data.Either       (fromRight)
import qualified Data.Map          as M
import           Data.Text         (Text, append)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import           PandocDecksParser (Card (cardBack, cardBidirectional,
                                          cardFront),
                                    Deck (deckCards))
import           Text.Pandoc       (Block (Plain), Meta (Meta), Pandoc (Pandoc),
                                    def, runPure, writeHtml5String)

cardImport card
  = (quoteCardText cardFrontHtml)
    `append` "\t" `append`
    (quoteCardText cardBackHtml)
  where cardFrontHtml
          = fromRight
            (error $ "Can't write card front: " ++ show (cardFront card))
          . runPure
          . writeHtml5String def
          . Pandoc (Meta M.empty)
          . (:[]) . Plain
          $ cardFront card
        cardBackHtml
          = fromRight
            (error $ "Can't write card back: " ++ show (cardBack card))
          . runPure
          . writeHtml5String def
          . Pandoc (Meta M.empty)
          $ cardBack card

quoteCardText t = "\"" `T.append` T.replace "\"" "\"\"" t `T.append` "\""

data Import
  = Import
  { bidirectional  :: Text
  , unidirectional :: Text
  } deriving Eq

deckImport deck
  = Import
    (T.intercalate "\n" . map cardImport $ bi)
    (T.intercalate "\n" . map cardImport $ uni)
  where
    bi  = filter  cardBidirectional        . deckCards $ deck
    uni = filter (not . cardBidirectional) . deckCards $ deck

deckImportFile name deck = do
  let i = deckImport deck
  TIO.writeFile (name ++ "-uni") (unidirectional i)
  putStrLn (name ++ "-uni written")
  TIO.writeFile (name ++ "-bi" ) (bidirectional  i)
  putStrLn (name ++ "-bi written" )
