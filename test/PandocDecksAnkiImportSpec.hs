{-# LANGUAGE OverloadedStrings #-}

module PandocDecksAnkiImportSpec where

import PandocDecksAnkiImport (Import (bidirectional, unidirectional),
                              deckImport)
import PandocDecksParserSpec (validDeck)
import Test.Hspec            (describe, hspec, it, shouldBe)

pandocDecksAnkiImportSpec = hspec $ do
  describe "deckImport" $ do
    it "outputs bidirectional cards import correctly" $
      (bidirectional . deckImport $ validDeck)
      `shouldBe`
      "\"ver\"\t\"<p>to watch; to see; to view</p>\"\n\
      \\"revista do Expresso\"\t\"<p>Expresso magazine</p>\""
    it "outputs unidirectional cards import correctly" $
      (unidirectional . deckImport $ validDeck)
      `shouldBe`
      "\"Presente do indicativo de \"\"ver\"\"\"\t\"<ul>\n\
      \<li>eu <strong>vejo</strong></li>\n\
      \<li>tu <strong>vês</strong></li>\n\
      \<li>você/ele/ela <strong>vê</strong></li>\n\
      \<li>nós vemos</li>\n\
      \<li>vocês/eles/elas <strong>vêem</strong></li>\n\
      \</ul>\"\n\
      \\"eu <strong>vejo</strong> /p\"\t\"<p>[ˈvɐjʒu]</p>\"\n\
      \\"tu <strong>vês</strong> /p\"\t\"<p>[ˈveʃ]</p>\"\n\
      \\"ele <strong>vê</strong> /p\"\t\"<p>[ˈve]</p>\""


