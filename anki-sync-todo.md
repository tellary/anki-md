- Create `anki-parse-deck` function

    It reads prefix from deck separation header and calls
    `anki-write-vocabulary-to-buffers` with an argument.

- Stop parsing on the end of empty deck
- Deck separation by Markdown H1 header with `#`
