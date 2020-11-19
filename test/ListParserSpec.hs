{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO    as TIO
import           ListParser      (bulletListBlocksOrError, isBulletList)
import           ListParser      (Card (Card, cardBack, cardFront
                                       , cardUnidirectional),
                                  simpleCard)
import           System.FilePath (takeDirectory, (</>))
import           Test.Hspec      (describe, hspec, it, shouldBe)
import           Text.Pandoc     (Block (Para), Inline (Space, Str),
                                  Pandoc (Pandoc), def, readMarkdown, runPure)

validBulletList = do
  Pandoc _ blks <- either undefined id
                   . runPure
                   . readMarkdown def <$> (TIO.readFile $ testFile "valid.md")
  return . head . filter isBulletList $ blks

validBulletListBlocks = bulletListBlocksOrError <$> validBulletList

pandocSimpleBidirectionalCard
  = [ Str "ver", Space
    , Str "--" , Space
    , Str "to" , Space, Str "watch;"
    , Space, Str "to", Space, Str "see;"
    , Space, Str "to", Space, Str "view"]

testFile = (takeDirectory __FILE__ </>)

main = hspec $ do
  describe "simpleCard" $ do
    it "creates unidirectional card correctly" $
      simpleCard pandocSimpleBidirectionalCard
      `shouldBe`
      Right
      ( Card
        { cardUnidirectional = False
        , cardFront = [ Str "ver" ]
        , cardBack = Para
            [ Str "to"
            , Space
            , Str "watch;"
            , Space
            , Str "to"
            , Space
            , Str "see;"
            , Space
            , Str "to"
            , Space
            , Str "view"
            ]
        }
      )

