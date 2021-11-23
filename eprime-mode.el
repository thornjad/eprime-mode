;;; eprime-mode.el --- An E′ checking mode for Emacs

;; Copyright (C) 2020-2021 Jade Michael Thornton
;; Copyright (C) 2014 Andrew Hynes

;; Filename: eprime-mode.el
;; URL: https://gitlab.com/thornjad/eprime-mode
;; Description: An E-prime checking mode for Emacs that highlights non-conforming text.
;; Version: 1.2.0
;; Keywords: E-prime, English, grammar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 only.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor mode for Emacs informing users of words not conforming to E′, as you type or on demand.

;; Installation:
;;
;; Install manually, or using a library like `straight.el':
;;
;;    (use-package eprime-mode
;;     :straight (:host gitlab :repo "thornjad/eprime-mode" :branch "main")
;;     :hook ((text-mode) . eprime-mode))

;; E prime? What?:
;;
;; E′ (or E-prime) refers to a subset of the English language excluding all forms of the verb "to
;; be". Such a practice may strengthen writing skills and may provide a path for greater mindfulness
;; in writing. Only shaky evidence really exists, but it can provide some fun and challenge.
;;
;; Check out the [the Wikipedia page](https://en.wikipedia.org/wiki/E-Prime) to learn more.

;; License:
;;
;; Copyright (C) 2020-2021 Jade Michael Thornton\
;; Copyright (C) 2014 Andrew Hynes
;;
;; This program is free software; you may redistribute it and/or modify it under the terms of the
;; GNU General Public License version 3 only, as published by the Free Software Foundation. This
;; program carries no warranty whatsoever, without even the implied warranty of merchantability or
;; fitness for a particular purpose. See <https://www.gnu.org/licenses/> for more details.


;;; Code:

(require 'cl)

(defvar eprime-ignore-case t
  "Defines whether eprime-mode should ignore case. Defaults to true.
  Set to \"nil\" if you want to turn this off.")

(defvar eprime-banned-words
  '("be" "being" "been" "am" "is" "isn't" "are" "aren't" "was" "wasn't" "were" "weren't"
    "I'm" "i'm" "you're" "we're" "they're" "he's" "she's" "it's" "there's" "here's"
    "where's" "how's" "what's" "who's" "what's" "ain't" "hain't" "whatcha" "yer")
  "The default banned words for eprime-mode, used by all of the functions in the mode.")

;;Note - FlySpell uses "OrangeRed" foreground
(defface eprime-banned-words-face
  '((((class color)) (:foreground "firebrick2" :weight bold :underline t))
    (t (:weight bold)))
  "Face used for marking a word banned by E-prime."
  :group 'eprime)

(defun eprime-check-thing (thing start)
  "Checks something returned by thing-at-point and corrects it if necessary."
  (when thing
    (let ((end (point)))
      (if eprime-ignore-case
	        (when (member (downcase thing) eprime-banned-words)
	          (let ((new-ov (make-overlay start end)))
	            (overlay-put new-ov 'face 'eprime-banned-words-face)))

        ;; if not eprime-ignore-case
        (when (member thing eprime-banned-words)
	        (let ((new-ov (make-overlay start end)))
	          (overlay-put new-ov 'face 'eprime-banned-words-face)))))))

;;;###autoload
(defun eprime-check-buffer ()
  "Checks the current buffer for banned words and applies a face
   to them."
  (interactive)
  (let* ((orig-syntax (char-to-string (char-syntax ?'))))
    (modify-syntax-entry ?' "w")
    (unwind-protect
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
              (forward-word -1))))
      (modify-syntax-entry ?' orig-syntax))))

;; TODO useless if mode not active?
;;;###autoload
(defun eprime-check-word ()
  "Checks the word that's currently entering."
  (interactive)
  (let* ((orig-syntax (char-to-string (char-syntax ?'))))
    (save-excursion
      (forward-word -1)
      (let ((current (thing-at-point 'word))
	          (start-point-pos (point)))
        (forward-word 1)
        (eprime-check-thing current start-point-pos)))
    (modify-syntax-entry ?' orig-syntax)))

(defun eprime-update (beg end length)
  "Scans around where the user types and informs if incorrect.
  Intended to invoke as the user types."
  (if (<= length 1)
      (eprime-check-word)
    (save-excursion
      (setf (point) beg)
      (forward-word -1)
      (while (< (point) end)
	      (eprime-check-word)
	      (forward-word 1)))))

;;;###autoload
(defun eprime-remove-corrections ()
  "Removes the overlayed corrections on words."
  (interactive)
  (remove-overlays))

(defun eprime-init ()
  "Initialises the mode."
  (eprime-check-buffer)
  (add-hook 'after-change-functions 'eprime-update)
  (modify-syntax-entry ?' "w"))

(defun eprime-cleanup (old-syntax)
  "Cleans up after the mode."
  (eprime-remove-corrections)
  (remove-hook 'after-change-functions 'eprime-update)
  (modify-syntax-entry ?' old-syntax))

;;;###autoload
(define-minor-mode eprime-mode
  "Minor mode for checking text conforms to E'. Change eprime-banned-words-face
  to change what banned words look like, and use (setq eprime-ignore-case nil) if you
  do not want it to match upper case words.
  (eprime-check-buffer), when invoked, can check a buffer without turning the mode on."
  :lighter " [E']"
  :init-value nil
  :keymap nil
  :global nil

  (let* ((orig-syntax (char-to-string (char-syntax ?'))))
    ;;the true = enabled, false = disabled
    (if eprime-mode 
	      (eprime-init)
      (eprime-cleanup orig-syntax))))

(provide 'eprime-mode)

;;; eprime-mode.el ends here

