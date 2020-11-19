{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO    as TIO
import           ListParser      (Card (Card, cardBack, cardBidirectional,
                                        cardFront),
                                  Deck (Deck, deckCards, deckName),
                                  bulletListBlocksOrError, decks, isBulletList,
                                  simpleCard)
import           System.FilePath (takeDirectory, (</>))
import           Test.Hspec      (describe, hspec, it, shouldBe, shouldReturn)
import           Text.Pandoc     (Block (BulletList, Para, Plain),
                                  Inline (Space, Str, Strong), Pandoc (Pandoc),
                                  def, readMarkdown, runPure)

validPandoc
  = either undefined id
    . runPure
    . readMarkdown def <$> (TIO.readFile $ testFile "valid.md")

validPandocBlocks = do
  Pandoc _ blks <- validPandoc
  return blks

validBulletList = do
  Pandoc _ blks <- validPandoc
  return . head . filter isBulletList $ blks

validBulletListBlocks = bulletListBlocksOrError <$> validBulletList

validDeck
  = Deck
    { deckName =
        [ Str "Irregular"
        , Space
        , Str "verbs"
        ]
    , deckCards =
        [ Card
          { cardBidirectional = True
          , cardFront = [ Str "ver" ]
          , cardBack =
              [ Para
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
              ]
          }
        , Card
          { cardBidirectional = False
          , cardFront =
              [ Str "Presente"
              , Space
              , Str "do"
              , Space
              , Str "indicativo"
              , Space
              , Str "de"
              , Space
              , Str "\"ver\""
              ]
          , cardBack =
              [ BulletList
                [
                  [ Plain
                    [ Str "eu"
                    , Space
                    , Strong [ Str "vejo" ]
                    ]
                  ]
                ,
                  [ Plain
                    [ Str "tu"
                    , Space
                    , Strong [ Str "vês" ]
                    ]
                  ]
                ,
                  [ Plain
                    [ Str "você/ele/ela"
                    , Space
                    , Strong [ Str "vê" ]
                    ]
                  ]
                ,
                  [ Plain
                    [ Str "nós"
                    , Space
                    , Str "vemos"
                    ]
                  ]
                ,
                  [ Plain
                    [ Str "vocês/eles/elas"
                    , Space
                    , Strong [ Str "vêem" ]
                    ]
                  ]
                ]
              ]
          }
        , Card
          { cardBidirectional = False
          , cardFront =
              [ Str "eu"
              , Space
              , Strong [ Str "vejo" ]
              , Space
              , Str "/p"
              ]
          , cardBack =
              [ Para [ Str "[ˈvɐjʒu]" ] ]
          }
        , Card
          { cardBidirectional = False
          , cardFront =
              [ Str "tu"
              , Space
              , Strong [ Str "vês" ]
              , Space
              , Str "/p"
              ]
          , cardBack =
              [ Para [ Str "[ˈveʃ]" ] ]
          }
        , Card
          { cardBidirectional = True
          , cardFront =
              [ Str "revista"
              , Space
              , Str "do"
              , Space
              , Str "Expresso"
              ]
          , cardBack =
              [ Para
                [ Str "Expresso"
                , Space
                , Str "magazine"
                ]
              ]
          }
        , Card
          { cardBidirectional = False
          , cardFront =
              [ Str "ele"
              , Space
              , Strong [ Str "vê" ]
              , Space
              , Str "/p"
              ]
          , cardBack =
              [ Para [ Str "[ˈve]" ] ]
          }
        ]
    }

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
        { cardBidirectional = True
        , cardFront = [ Str "ver" ]
        , cardBack
          = [ Para
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
            ]
        }
      )
  describe "decks" $ do
    it "creates single valid deck correctly" $
      decks <$> validPandocBlocks `shouldReturn` Right [validDeck]
