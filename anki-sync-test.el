(ert-deftest anki-test-parse-valid ()
  (find-file "valid.md")
  (switch-to-buffer "valid1.md")
  (beginning-of-buffer)
  (anki-parse-vocabulary
   (lambda (front back uni))
   )
  )

(ert-deftest anki-test-format-card-dash-newline ()
  (should
   (equal
    (anki--format-card "abc
- revista do Expresso")
    "abc
- revista do Expresso")
   )
  )

(ert-deftest anki-test-format-card-each-dash-newline ()
  (should
   (equal
    (anki--format-card "- abc
- revista do Expresso")
    "- abc<br/>- revista do Expresso")
   )
  )
