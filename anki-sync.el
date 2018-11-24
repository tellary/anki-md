;;; -*- lexical-binding: t -*-

(defun anki-get (key alist)
  (cdr (assq key alist)))

(defun anki-parse-vocabulary--print-cb
    (card-front card-back unidirectional)
  (message "card front:\n%s" card-front)
  (message "card back:\n%s" card-back)
  (message "unidirectional: %s" unidirectional)
  (message "------------------")
  )

(defun anki--format-card (card)
  (let* (
         ;; Remove trailing whitespace of entire card
         (card (replace-regexp-in-string
                "\\`[\n\t ]*\\|[\n\t ]*\\'" "" card))
         (l (split-string card "^[\t ]*-")) (result ()))
    (if (equal 1 (length l))
        ;; non-list card, just remove extra whitespace
        (replace-regexp-in-string "[\n\t ]+" " " card)
      (dolist (line
               (cdr l)
               (mapconcat
                (lambda (s) (concat "- " s))
                result "\n"))
        (setq result
              (append
               result
               (list
                (let* (
                       ;; remove extra whitespace in each line
                       (line
                        (replace-regexp-in-string
                         "[\n\t ]+" " " line))
                       ;; Remove trailing waitspace in each line
                       (line
                        (replace-regexp-in-string
                         "^[\n\t ]*\\|[\n\t ]*$" "" line))
                       )
                  line
                  )
                )
               )
              )
        )
      )
    )
  )

(defun anki--format-card-proxy (cb)
  (lambda (card-front card-back unidirectional)
    (funcall cb
             (anki--format-card card-front)
             (anki--format-card card-back)
             unidirectional)
    )
  )

(defun anki-parse-vocabulary--card-front-beginning (card-info)
  (search-forward-regexp
   "\\( \?-- \?\\)\\|\\(\n\\)\\|\\( \?-\> \?\\)")
  (list
   (cons 'card-front-beginning
         (anki-get 'card-front-beginning card-info))
   (cons 'card-front-end (match-beginning 0))
   (cons 'unidirectional (match-beginning 3))
   (cons 'card-back-beginning (match-end 0)))
)

(defun anki-parse-vocabulary--handle-card (cb card-info)
  (let ((card-front
         (buffer-substring-no-properties
          (anki-get 'card-front-beginning card-info)
          (anki-get 'card-front-end card-info)))
        (card-back
         (buffer-substring-no-properties
          (anki-get 'card-back-beginning card-info)
          (- (match-beginning 0) 1))))
    (funcall
     cb card-front card-back (anki-get 'unidirectional card-info))
    )
  )


(defun anki-parse-vocabulary (cb)
  (let (card-info)
    (while
        (and (search-forward-regexp "^-" nil t)
             (not (equal "-" (string (following-char))))
             (not (eobp)))
      (when card-info
        ;; card end state/next card beginning state here
        (anki-parse-vocabulary--handle-card cb card-info))
      (cond
       ((equal " " (string (following-char)))
        (setq
         card-info
         (let ((new-card-info
                (list
                 (cons 'card-front-beginning (+ 1 (point))))))
           (anki-parse-vocabulary--card-front-beginning new-card-info)
           ;; card back beginning state after this function is complete
           )
         )
        )
       (t (error
           "New card or end of cards is expected, position: %s"
           (point))))
      )
    ;; last card end state
    (search-forward-regexp "\\'" nil t)
    (anki-parse-vocabulary--handle-card cb card-info)
    t
    )
  )

(defun anki-sync-line-to (buffer)
  (beginning-of-line)
  (forward-char 2)
  (set-mark-command nil)
  (search-forward-regexp " \?-- \?")
  (forward-char -4)
  (let ((front
         (buffer-substring-no-properties
          (region-beginning) (region-end))))
    (with-current-buffer buffer
      (end-of-buffer)
      (insert front)
      (insert-char (aref "	" 0))
      )
    )
  (forward-char 4)  
  (set-mark-command nil)
  (end-of-line 1)
  (let ((back
         (buffer-substring-no-properties
          (region-beginning) (region-end))))
    (with-current-buffer buffer
      (end-of-buffer)
      (insert back)
      (insert-char (aref "	" 0))
      (newline)
      )
    )
  (move-beginning-of-line 2)
  (beginning-of-line)
  )

