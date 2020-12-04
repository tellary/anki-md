{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module PandocDecksParser where

import           Data.Either   (fromRight)
import           Data.Function (on)
import           Data.List     (groupBy)
import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Text.Pandoc   (Block (BulletList, Header, Para, Plain),
                                Inline (Space, Str), Meta (Meta),
                                Pandoc (Pandoc),
                                ReaderOptions (readerExtensions), def,
                                pandocExtensions, readMarkdown, runPure,
                                writeMarkdown)
import           Text.Printf   (printf)

pandocOrError f
  = either (error $ "Can't parse Markdown from " ++ f) id
    . runPure
    . readMarkdown def { readerExtensions = pandocExtensions }
    <$> TIO.readFile f

pandocBlocksOrError f = do
  Pandoc _ blks <- pandocOrError f
  return blks

data Card
  = Card
  { cardBidirectional :: Bool
  , cardFront         :: [Inline]
  , cardBack          :: [Block]
  } deriving (Eq, Show)

data CardDelim = Dash | Arrow deriving (Show, Eq)

dashStr = "\8211"
arrowStr = "->"

cardDelim (Str str)
  | str == dashStr  = Just Dash
  | str == arrowStr = Just Arrow
  | otherwise       = Nothing
cardDelim _         = Nothing

isSpace Space = True
isSpace _     = False

trimPandocSpace
  = reverse . dropWhile isSpace . reverse . dropWhile isSpace

simpleCard :: [Inline] -> Either String Card
simpleCard inls
  = case groupBy ((==) `on` cardDelim) inls of
      front:(sep:seps):backCardGroups
        -> let dir = case cardDelim sep of
                       Just Dash  -> True
                       Just Arrow -> False
                       Nothing    -> error "Separator may only be dash or arrow"
           in Right
              $ Card dir
                (trimPandocSpace front)
                [Para . trimPandocSpace $ seps ++ concat backCardGroups]
      front:_ ->
        Left . printf "Neither dash or arrow separator found for '%s'"
        . T.unpack
        . fromRight (error $ "Can't write card front: " ++ show front)
        . runPure
        . writeMarkdown def
        . Pandoc (Meta M.empty)
        . (:[]) . Plain $ front
      _       -> error "groupBy cannot return empty list"

complexCard [Para _]
  = Left "Single paragraph can't be a complex card"
complexCard (Para inls:blocks)
  = Right $ Card False inls blocks
complexCard (_:_)
  = Left "Complex card must start from a paragraph"
complexCard _
  = Left "Complex card must be multiple blocks starting from a paragraph"

card [Para inls] = simpleCard inls
card blks        = complexCard blks

header2 h@(Header 2 _ _) = Just h
header2 _                = Nothing

data Deck
  = Deck
  { deckName  :: [Inline]
  , deckCards :: [Card]
  } deriving (Eq, Show)

decks = sequence . groupToDecks . groupBy ((==) `on` header2)
  where groupToDecks [] = []
        groupToDecks ([Header 2 _ name]:[BulletList blkss]:headerBlocks)
          = (fmap (Deck name) . sequence . map card $ blkss)
          : groupToDecks headerBlocks
        groupToDecks ([Header 2 _ _]:_)
          = [Left "Header 2 must be followed by BulletList"]
        groupToDecks ((blk:_):_)
          = [Left $ "Decks must start from Header 2, but found " ++ show blk]
        groupToDecks ([]:_)
          = error "Empty group is not possible"

fileDecks = (decks <$>) .  pandocBlocksOrError
