;;; -*- lexical-binding: t -*-

(defun anki-get (alist key)
  (cdr (assq key alist))
  )

(defun anki-put (alist key value)
  (let ((pair (assq key alist)))
    (if pair
        (let ((old-value (cdr pair)))
          (setcdr pair value)
          old-value
          )
      (setq pair (cons key value))
      (setcdr (last alist) (cons pair nil))
      nil
      )
    )
  )

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
      (setq result-str
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
      (setq result-str
            (replace-regexp-in-string "\n" "<br/>" result-str))
      (setq result-str
            (replace-regexp-in-string
             "__\\([^_]*\\)__" "<b>\\1</b>" result-str nil nil))
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

(defun anki--append-card-to-buffers (uni-buffer bi-buffer)
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

(defun anki-parse-vocabulary--start (self)
  (cond
   ((search-forward-regexp "^-" nil t)
    (if (equal "-" (string (following-char)))
        ;; We found deck separator right from the start state,
        ;; thus we are still in the start state --
        ;; we'll keep searching
        (anki-put self 'state 'start)
      ;; We found beginning of card
      (when (equal " " (string (following-char)))
        (forward-char)
        )
      (anki-put self 'state 'card-front-beginning)
      (anki-put self 'card-front-beginning (point))
      )
    )
   (t (anki-put self 'state 'end))
   )
  )

(defun anki-parse-vocabulary--card-front-beginning (self)
  (if (search-forward-regexp
       "\\( \?-- \?\\)\\|\\(\n\n\\)\\|\\( \?-\> \?\\)"
       nil t)
      (progn
        (goto-char (match-end 0))
        (anki-put self 'state 'card-back-beginning)
        (anki-put self 'card-front-end (match-beginning 0))
        (anki-put self 'unidirectional
                  (or (match-beginning 2) (match-beginning 3)))
        (anki-put self 'card-back-beginning (match-end 0))
        )
    (error
     "Front back side separator is expected , position: %s" (point))
    )
)

(defun anki-parse-vocabulary--card-back-beginning (self)
  (cond
   ((search-forward-regexp "^-" nil t)
    (beginning-of-line)
    (anki-put self 'state 'card-back-end)
    (anki-put self 'card-back-end (- (point) 1)))
   (t
    (end-of-buffer)
    (anki-put self 'state 'card-back-eob)
    (anki-put self 'card-back-end (point)))
   )
  )

(defun anki-parse-vocabulary--card-back-end (self)
  (anki-parse-vocabulary--handle-card self)
  (search-forward-regexp "^-" nil t)
  (if (equal "-" (string (following-char)))
      ;; We found deck separator -- done.
      (anki-put self 'state 'end)
    ;; We found beginning of next card
    (when (equal " " (string (following-char)))
      (forward-char)
      )
    (anki-put self 'state 'card-front-beginning)
    (anki-put self 'card-front-beginning (point))
    )
  )

(defun anki-parse-vocabulary--card-back-eob (self)
  (anki-parse-vocabulary--handle-card self)
  (anki-put self 'state 'end)
  )

(defun anki-parse-vocabulary--handle-card (self)
  (let ((card-front
         (buffer-substring-no-properties
          (anki-get self 'card-front-beginning)
          (anki-get self 'card-front-end)))
        (card-back
         (buffer-substring-no-properties
          (anki-get self 'card-back-beginning)
          (anki-get self 'card-back-end)))
        (unidirectional
         (anki-get self 'unidirectional))
        (cb (anki-get self 'cb)))
    (funcall
     cb card-front card-back unidirectional)
    )
  )

(defun anki-parse-vocabulary (cb)
  (let (self)
    (setq self
          (list
           (cons 'state 'start)
           (cons 'cb cb)))
    (while (not (eq 'end (anki-get self 'state)))
      (cond
       ((eq 'start
            (anki-get self 'state))
        (anki-parse-vocabulary--start self))
       ((eq 'card-front-beginning
            (anki-get self 'state))
        (anki-parse-vocabulary--card-front-beginning self))
       ((eq 'card-back-beginning
            (anki-get self 'state))
        (anki-parse-vocabulary--card-back-beginning self))
       ((eq 'card-back-end
            (anki-get self 'state))
        (anki-parse-vocabulary--card-back-end self))
       ((eq 'card-back-eob
            (anki-get self 'state))
        (anki-parse-vocabulary--card-back-eob self))
       (t (error "Unknown state: %s" (anki-get self 'state)))
       )
      )
    )
  )

(defun anki-parse-vocabulary-to-buffers (&optional prefix)
  (if (not prefix)
      (anki-parse-vocabulary-to-buffers "vocabulary")
    (let ((bi  (get-buffer-create (concat prefix "-bi")))
          (uni (get-buffer-create (concat prefix "-uni"))))
      (with-current-buffer uni
        (erase-buffer))
      (with-current-buffer bi
        (erase-buffer))
      (anki-parse-vocabulary
       (anki--format-card-proxy
        (anki--append-card-to-buffers uni bi)))
      (list uni bi)
      )
    )
  )

(defun anki-write-vocabulary-to-buffers (&optional prefix)
  (interactive)
  (let* ((buffers (anki-parse-vocabulary-to-buffers prefix))
         (uni (car  buffers))
         (bi  (cadr buffers)))
    (with-current-buffer uni
      (write-file
       (concat default-directory (buffer-name uni))))
    (with-current-buffer bi
      (write-file
       (concat default-directory (buffer-name bi))))
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

