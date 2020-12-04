{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module PandocDecksParserSpec where

import PandocDecksParser (Card (Card, cardBack, cardBidirectional, cardFront),
                          Deck (Deck, deckCards, deckName), dashStr, decks,
                          pandocBlocksOrError, simpleCard)
import System.FilePath   (takeDirectory, (</>))
import Test.Hspec        (describe, hspec, it, shouldBe, shouldReturn)
import Text.Pandoc       (Block (BulletList, Para, Plain),
                          Inline (Quoted, Space, Str, Strong),
                          QuoteType (DoubleQuote))

validPandocBlocks = pandocBlocksOrError $ testFile "valid.md"

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
              , Quoted DoubleQuote [ Str "ver" ]
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
                    , Strong [Str "vemos"]
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
    , Str dashStr, Space
    , Str "to" , Space, Str "watch;"
    , Space, Str "to", Space, Str "see;"
    , Space, Str "to", Space, Str "view"]

testFile = (takeDirectory __FILE__ </>)

pandocDecksParserSpec = hspec $ do
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
