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

(defun anki--append-card-to-buffer (buffer-or-name)
  (lambda (card-front card-back unidirectional)
    (with-current-buffer buffer-or-name
      (end-of-buffer)
      (insert card-front)
      (insert-char (aref "	" 0))
      (insert card-back)
      (insert-char (aref "	" 0))
      (newline)
      )
    )
  )

(defun anki--append-card-to-buffers (bi-buffer uni-buffer)
  (lambda (card-front card-back unidirectional)
    (if unidirectional
        (funcall
         (anki--append-card-to-buffer uni-buffer)
         card-front card-back unidirectional)
      (funcall
       (anki--append-card-to-buffer bi-buffer)
       card-front card-back unidirectional)
      )
    )
  )

(defun anki-parse-vocabulary--card-front-beginning (card-info)
  (search-forward-regexp
   "\\( \?-- \?\\)\\|\\(\n\n\\)\\|\\( \?-\> \?\\)")
  (goto-char (match-end 0))
  (list
   (cons 'card-front-beginning
         (anki-get 'card-front-beginning card-info))
   (cons 'card-front-end (match-beginning 0))
   (cons 'unidirectional
         (or (match-beginning 2) (match-beginning 3)))
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
          (anki-get 'card-back-end card-info))))
    (funcall
     cb card-front card-back (anki-get 'unidirectional card-info))
    )
  )


(defun anki-parse-vocabulary (cb)
  (let (card-info state)
    (while
        (and (search-forward-regexp "^-" nil t)
             (not (equal "-" (string (following-char))))
             (not (eobp)))
      (cond
       ((equal " " (string (following-char)))
        (setq state 'card-front-beginning)
        (when card-info
          ;; start of the next card, we should handle the previous one
          (setq
           card-info
           (cons
            (cons 'card-back-end (- (match-beginning 0) 1))
            card-info))
          (anki-parse-vocabulary--handle-card cb card-info))
        (setq
         card-info
         (let ((new-card-info
                (list
                 (cons 'card-front-beginning (+ 1 (point))))))
           (anki-parse-vocabulary--card-front-beginning new-card-info)
           )
         )
        (setq state 'card-back-beginning)
        )
       (t (error
           "New card or end of cards is expected, position: %s"
           (point)))
       )
      )
    (when (eq state 'card-back-beginning)
      ;; move to the end of current card
      (if (equal "-" (string (following-char)))
          (setq
           card-info
           (cons
            (cons 'card-back-end (- (match-beginning 0) 1))
            card-info))
        (end-of-buffer)
        (setq
         card-info
         (cons
          (cons 'card-back-end (point))
          card-info))
        )
      ;; handle the card once we are at the end of card
      (anki-parse-vocabulary--handle-card cb card-info)
      )
    t
    )
  )

(defun anki-parse-vocabulary-to-buffers (&optional prefix)
  (if (not prefix)
      (anki-parse-vocabulary-to-buffers "vocabulary"))
  (let ((bi  (get-buffer-create (concat prefix "-bi")))
        (uni (get-buffer-create (concat prefix "-uni"))))
    (anki-parse-vocabulary
      (anki--format-card-proxy
       (anki--append-card-to-buffers bi uni)))
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

