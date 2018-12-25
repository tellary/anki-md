(ert-deftest anki-test-parse-valid ()
  (find-file "valid.md")
  (switch-to-buffer "valid.md")
  (beginning-of-buffer)
  (anki-parse-vocabulary
   (lambda (front back uni))
   )
  )

(ert-deftest anki-test-fail-on-noback-nodash ()
  (find-file "invalid-noback-nodash.md")
  (switch-to-buffer "invalid-noback-nodash.md")
  (beginning-of-buffer)
  (should
   (equal
    (cadr
     (should-error
      (anki-parse-vocabulary
       (lambda (front back uni))
       )
      )
     )
    "Front back side separator is expected, but new card found, position: 296"
    )
   )
  )

(ert-deftest anki-test-fail-on-nobackcard ()
  (find-file "invalid-noback.md")
  (switch-to-buffer "invalid-noback.md")
  (beginning-of-buffer)
  (should
   (equal
    (cadr
     (should-error
      (anki-parse-vocabulary
       (anki--format-card-proxy
        (lambda (front back uni)))
       )
      )
     )
    "Empty card found, position: 285"
    )
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
