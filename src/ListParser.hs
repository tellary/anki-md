{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module ListParser where

import           Data.Function (on)
import           Data.List     (groupBy)
import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Text.Pandoc   (Block (BulletList, Para), Inline (Space, Str),
                                def, readMarkdown, runPure)

isBulletList (BulletList _) = True
isBulletList _              = False

bulletListBlocks (BulletList blkss)
  = Just blkss
bulletListBlocks _ = Nothing

bulletListBlocksOrError
  = maybe (error "BulletList expected to get list blocks") id
  . bulletListBlocks

data Card
  = Card
  { cardUnidirectional :: Bool
  , cardFront          :: [Inline]
  , cardBack           :: Block
  } deriving (Eq, Show)

data CardDelim = Dash | Arrow deriving (Show, Eq)

cardDelim (Str "--") = Just Dash
cardDelim (Str "->") = Just Arrow
cardDelim _          = Nothing

isSpace Space = True
isSpace _     = False

trimPandocSpace
  = reverse . dropWhile isSpace . reverse . dropWhile isSpace

simpleCard :: [Inline] -> Either String Card
simpleCard inls
  = case groupBy ((==) `on` cardDelim) inls of
      front:(sep:seps):backCardGroups
        -> let dir = case cardDelim sep of
                       Just Dash  -> False
                       Just Arrow -> True
                       Nothing    -> error "Separator may only be dash or arrow"
           in Right
              $ Card dir
                (trimPandocSpace front)
                (Para . trimPandocSpace $ seps ++ concat backCardGroups)
      _ -> Left "Neither dash or arrow separator found"


--
-- cardFromBlocks [Para inls]
