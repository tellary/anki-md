{-# LANGUAGE OverloadedStrings #-}

module PandocDecksAnkiImport where

import           Control.Monad     (unless)
import           Data.Either       (fromRight)
import qualified Data.Map          as M
import           Data.Text         (Text, append)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import           PandocDecksParser (Card (cardBack, cardBidirectional,
                                          cardFront),
                                    Deck (deckCards))
import           Text.Pandoc       (Block (Plain), HTMLMathMethod (MathJax),
                                    Meta (Meta), Pandoc (Pandoc),
                                    WriterOptions (writerExtensions,
                                                   writerHTMLMathMethod),
                                    def, pandocExtensions, runPure,
                                    writeHtml5String)

writeHtml p
  = fromRight
    (error $ "Can't write card html: " ++ show p)
    . runPure
    . writeHtml5String
      def { writerExtensions = pandocExtensions
          , writerHTMLMathMethod = MathJax "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          }
    $ p

cardImport card
  = (quoteCardText cardFrontHtml)
    `append` "\t" `append`
    (quoteCardText cardBackHtml)
  where cardFrontHtml
          = writeHtml
          . Pandoc (Meta M.empty)
          . (:[]) . Plain
          $ cardFront card
        cardBackHtml
          = writeHtml
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
  unless (T.null $ unidirectional i) $ do
    TIO.writeFile (name ++ "-uni") (unidirectional i)
    putStrLn (name ++ "-uni written")
  unless (T.null $ bidirectional  i) $ do
    TIO.writeFile (name ++ "-bi" ) (bidirectional  i)
    putStrLn (name ++ "-bi written" )
