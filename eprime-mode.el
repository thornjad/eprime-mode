;;;;eprime-mode.el - An E-prime mode for Emacs, which warns users when they 
;;;;write something that does not conform to E-prime.
;;;;Read more here - https://en.wikipedia.org/wiki/E-prime
;;;;Naturally, all of this file that can do, conforms to E'

;; Try not to hate me -
(require 'cl)

(modify-syntax-entry ?' "w")
;;Counts ' as part of a word, required for conjunctions like I'm

(setq eprime-ignore-case t)

;;the banned words
(setq eprime-banned-words '("be" "being" "been" "am" "is" "isn't" "are" "aren't" "was" "wasn't" "were" "weren't" "I'm" "i'm" "you're" "we're" "they're" "he's" "she's" "it's" "there's" "here's" "where's" "how's" "what's" "who's" "what's" "ain't" "hain't" "whatcha" "yer"))

;;Note - FlySpell uses "OrangeRed" foreground
(defface eprime-banned-words-face
  '((((class color)) (:foreground "firebrick2" :weight bold :underline t))
      (t (:weight bold)))
  "Face used for marking a word banned by E-prime. For reference, FlySpell uses
  OrangeRed as its forground. The foreground for E' mode currently has the value \"firebrick2\"."
  :group 'eprime)

(defun eprime-check-thing (thing start)
  "Checks something returned by thing-at-point and corrects it if necessary.
  Whilst the variable eprime-ignore-case remains t, it will ignore case, else it won't.
  The default has the property of t."
  ;;can do it in less lines, but this way feels clearer to me
  (if eprime-ignore-case
      (when (member (downcase thing) eprime-banned-words)
	(let ((new-ov (make-overlay start (point))))
	  (overlay-put new-ov 'face 'eprime-banned-words-face)))
    (when (member thing eprime-banned-words)
      (let ((new-ov (make-overlay start (point))))
	(overlay-put new-ov 'face 'eprime-banned-words-face)))))

(defun eprime-check-buffer ()
  "Checks the current buffer for banned words and applies a face
   to them."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-word 1)
    (forward-word -1)
    (catch 'break
      (while (not (eobp))
	(let ((current (thing-at-point 'word))
	      (start-point-pos (point)))
	  (forward-word 1)
	  (eprime-check-thing current start-point-pos))
	(forward-word 1)
	(when (eobp) (throw 'break "Finished!"))
	(forward-word -1)))))

(defun eprime-check-word ()
  "Checks the word that's currently entering."
  (interactive)
  (save-excursion
    (forward-word -1)
    (let ((current (thing-at-point 'word))
	  (start-point-pos (point)))
      (forward-word 1)
      (eprime-check-thing current start-point-pos))))


(defun eprime-update (beg end length)
  "Scans around where the user types and informs if incorrect.
  Intended to invoke as the user types."
  (if (<= length 1)
      (eprime-check-word)
    (save-excursion
      (while (> (point) beg)
	(forward-word -1))
      (while (< (point) end)
	(eprime-check-word)
	(forward-word 1)))))


(defun eprime-remove-corrections ()
  "Removes the overlayed corrections on words."
  (interactive)
  (remove-overlays))

(defun eprime-init ()
  "Initialises the mode."
  (eprime-check-buffer)
  (add-hook 'after-change-functions 'eprime-update))

(defun eprime-cleanup ()
  "Cleans up after the mode."
  (eprime-remove-corrections)
  (remove-hook 'after-change-functions 'eprime-update))

(define-minor-mode eprime-mode
  "Minor mode for checking text conforms to E'. Change eprime-banned-words-face
  to change what banned words look like, and use (setq eprime-ignore-case nil) if you
  do not want it to match upper case words.
  (eprime-check-buffer), when invoked, can check a buffer without turning the mode on."
  :lighter " [E' Mode]"
  :init-value nil
  :keymap nil
  :global nil

  ;;the true = enabled, false = disabled
  (if eprime-mode 
      (eprime-init)
    (eprime-cleanup)))

;;Testing area! -
;;be being am is spaghetti
;; is hello go to the are hello whatcha
;; be toodlepip was going to the were I'm
;; spaghetti and meatballs he's a penis
;;AM BE BEING AM IS WHAT'S

(provide 'eprime-mode)

