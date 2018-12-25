- 20181222.1: Full markdown support in cards
- Stop parsing at the end of empty deck
- Create `anki-parse-deck` function

    It reads prefix from deck separation header and calls
    `anki-write-vocabulary-to-buffers` with an argument.

- Deck separation by Markdown H1 header with `#`
