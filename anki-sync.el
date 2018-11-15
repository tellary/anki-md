(defun anki-get (key alist)
  (cdr (assq key alist)))

(defun anki-parse-vocabulary--print-cb
    (card-front card-back unidirectional)
  (message "card front: %s" card-front)
  (message "card back: %s" card-back)
  (message "unidirectional: %s" unidirectional)
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

(defun anki-parse-vocabulary (cb)
  (setq card-info nil)
  (while
      (and (search-forward-regexp "^-" nil t)
           (not (equal "-" (string (following-char))))
           (not (eobp)))
    (when card-info
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
    (cond
     ((equal " " (string (following-char)))
      (setq
       card-info
       (let ((new-card-info
              (list
               (cons 'card-front-beginning (+ 1 (point))))))
         (anki-parse-vocabulary--card-front-beginning new-card-info)
         )
       )
      )
     (t (error
         "New card or end of cards is expected, position: %s"
         (point))))
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

