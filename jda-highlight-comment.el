;;; jda-highlight-comment.el --- highlight comment(TODO, FIXME, region, ...)

;; Copyright (C) 2017 Jong-gyu Lee<jglee1027@gmail.com>

;; Authors: Jonggyu Lee<jglee1027@gmail.com>
;; Maintainer: Jonggyu Lee<jglee1027@gmail.com>
;; Created: 12 Jul 2017
;; Version: 0.1.0
;; Keywords: languages, tools
;; Repository: git://github.com/jglee1027/jda-minor-mode.git

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with it.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; JDA(Jong Gyu Development Assistant) is a minor mode for developers
;; who use various programming languages. The major aim of JDA is to
;; offer pleasure for developers. No doubt you will always enjoy
;; writing a source code in any programming language if you use it.

;; * Installation
;;   Edit your ~/.emacs file to add the line:
;;     (add-to-list 'load-path "/path/to/jda-minor-mode")
;;     (require 'jda-highlight-comment)
;;     (jda-hl-comment-mode)

;; * Major commands:
;; ** List highlight comments
;;    Press "C-c j /" to show highlight comments in message at the bottom of the
;;    screen.

;;; Code:

(require 'hi-lock)

(defgroup jda-hl-comment nil
  "JDA(Jonggyu Development Assistant) highlight comment"
  :group 'applications
  :prefix "jda-hl-comment-")

(defcustom jda-hl-comment-region-regexp-alist
  '(("C/l"          .   ("c-mode-hook" "\\(/\\*\\*\\*.*\\*\\*\\*/\\|#region\\|#pragma[[:space:]]+region\\)"))
    ("C++/l"        .   ("c++-mode-hook" "\\(/\\*\\*\\*.*\\*\\*\\*/\\|#region\\|#pragma[[:space:]]+region\\)"))
    ("Emacs-Lisp"   .   ("lisp-mode-hook" "^[[:space:]]*;;;.*$"))
    ("Java/l"       .   ("java-mode-hook" "\\(/\\*\\*\\*.*\\*\\*\\*/\\|//[[:space:]]*region\\)"))
    ("ObjC/l"       .   ("objc-mode-hook" "#pragma[[:space:]]*mark[[:space:]]+.\\{2,\\}"))
    (nil            .   (nil "/\\*\\*\\*.*\\*\\*\\*/")))
  "Regular expression to search region comment"
  :type 'alist
  :group 'jda-hl-comment)

(defcustom jda-hl-comment-todo-regexp-alist
  '((nil            .   (nil "\\b\\(TODO\\|NOTE\\|FIXME\\|BUG\\|WARNING\\)\\b")))
  "Regular expression to search todo comment"
  :type 'alist
  :group 'jda-hl-comment)

(defvar jda-hl-comment-ring-max 20)
(defvar jda-hl-comment-ring (make-ring jda-hl-comment-ring-max))
(defvar jda-hl-comment-ring-iterator -1)

;;; functions

(defun jda-hl-comment-match-markers-in-buffer (regexp &optional buffer append)
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp regexp nil t 1)
              (add-to-list 'matches (point-marker) append)))))
      matches)))

(defun jda-hl-comment-region-regexp ()
  (let ((regexp-list (assoc mode-name jda-hl-comment-region-regexp-alist)))
    (if (null regexp-list)
        (car (cddr (assoc nil jda-hl-comment-region-regexp-alist)))
      (car (cddr regexp-list)))))

(defun jda-hl-comment-todo-regexp ()
  (let ((regexp-list (assoc mode-name jda-hl-comment-todo-regexp-alist)))
    (if (null regexp-list)
        (car (cddr (assoc nil jda-hl-comment-todo-regexp-alist)))
      (car (cddr regexp-list)))))

(defun jda-hl-comment-mode-initialize ()
  (interactive)
  (hi-lock-line-face-buffer (jda-hl-comment-region-regexp) 'hi-green)
  (hi-lock-face-buffer (jda-hl-comment-todo-regexp) 'hi-pink))

(defun jda-hl-comment-mode-finalize ()
  (interactive)
  (hi-lock-unface-buffer (jda-hl-comment-region-regexp))
  (hi-lock-unface-buffer (jda-hl-comment-todo-regexp)))

(defun jda-hl-comment-line-number-at (marker)
  (save-excursion
    (with-current-buffer (marker-buffer marker)
      (line-number-at-pos marker))))

;; jump-to-register in register.el
(defun jda-hl-comment-jump (marker)
  (cond
   ((and (consp marker) (frame-configuration-p (car marker)))
    (set-frame-configuration (car marker) (not delete))
    (goto-char (cadr marker)))
   ((and (consp marker) (window-configuration-p (car marker)))
    (set-window-configuration (car marker))
    (goto-char (cadr marker)))
   ((markerp marker)
    (or (marker-buffer marker)
        (error "That marker's buffer no longer exists"))
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker))
   ((and (consp marker) (eq (car marker) 'file))
    (find-file (cdr marker)))
   ((and (consp marker) (eq (car marker) 'file-query))
    (or (find-buffer-visiting (nth 1 marker))
        (y-or-n-p (format "Visit file %s again? " (nth 1 marker)))
        (error "marker access aborted"))
    (find-file (nth 1 marker))
    (goto-char (nth 2 marker)))
   (t
    (error "marker doesn't contain a buffer position or configuration"))))

(defun jda-hl-comment-push-marker (&optional use-jda-hl-comment-list)
  (interactive)
  (setq jda-hl-comment-ring-iterator -1)
  (let ((last-marker nil)
        (curr-marker (point-marker)))
    (condition-case nil
        (cond ((ring-empty-p jda-hl-comment-ring)
               (ring-insert jda-hl-comment-ring curr-marker)
               (if use-jda-hl-comment-list
                   (jda-hl-comment-list)))
              (t
               (setq last-marker (ring-ref jda-hl-comment-ring 0))
               (cond ((not (equal last-marker curr-marker))
                      (ring-insert jda-hl-comment-ring curr-marker)
                      (if use-jda-hl-comment-list
                          (jda-hl-comment-list))))))
      (error nil))))

(defun jda-hl-comment-push-marker-ring (curr-marker &optional use-jda-hl-comment-list)
  (interactive)
  (setq jda-hl-comment-ring-iterator -1)
  (let ((last-marker nil))
    (condition-case nil
        (cond ((ring-empty-p jda-hl-comment-ring)
               (ring-insert jda-hl-comment-ring curr-marker)
               (if use-jda-hl-comment-list
                   (jda-hl-comment-list)))
              (t
               (setq last-marker (ring-ref jda-hl-comment-ring 0))
               (cond ((not (equal last-marker curr-marker))
                      (ring-insert jda-hl-comment-ring curr-marker)
                      (if use-jda-hl-comment-list
                          (jda-hl-comment-list))))))
      (error nil))))

(defun jda-hl-comment-push-match-markers ()
  (interactive)
  (setq jda-hl-comment-ring (make-ring jda-hl-comment-ring-max))
  (setq jda-hl-comment-ring-iterator -1)
  (dolist (marker (jda-hl-comment-match-markers-in-buffer
                   (jda-hl-comment-todo-regexp)))
    (jda-hl-comment-push-marker-ring marker))
  (dolist (marker (jda-hl-comment-match-markers-in-buffer
                   (jda-hl-comment-region-regexp)))
    (jda-hl-comment-push-marker-ring marker)))

(defun jda-hl-comment-push-marker-menu ()
  (interactive)
  (jda-hl-comment-push-marker t))

(defun jda-hl-comment-next ()
  (interactive)
  (cond ((equal jda-hl-comment-ring-iterator -1)
         (setq jda-hl-comment-ring-iterator 0)))
  (condition-case nil
      (let* ((prev-iterator (mod (1+ jda-hl-comment-ring-iterator)
                                 (ring-length jda-hl-comment-ring)))
             (prev-marker (ring-ref jda-hl-comment-ring prev-iterator)))
        (jda-hl-comment-jump prev-marker)
        (setq jda-hl-comment-ring-iterator prev-iterator))
    (error nil)))

(defun jda-hl-comment-next-ui ()
  (interactive)
  (jda-hl-comment-next)
  (jda-hl-comment-list))

(defun jda-hl-comment-prev ()
  (interactive)
  (cond ((equal jda-hl-comment-ring-iterator -1)
         (message "Should run the command 'jda-hl-comment-next'"))
        (t
         (condition-case nil
             (let* ((next-iterator (mod (1- jda-hl-comment-ring-iterator)
                                        (ring-length jda-hl-comment-ring)))
                    (next-marker (ring-ref jda-hl-comment-ring next-iterator)))
               (jda-hl-comment-jump next-marker)
               (setq jda-hl-comment-ring-iterator next-iterator))
           (error nil)))))

(defun jda-hl-comment-prev-ui ()
  (interactive)
  (jda-hl-comment-prev)
  (jda-hl-comment-list))

(defun jda-hl-comment-get-line-string (marker)
  (interactive)
  (cond ((and (markerp marker)
              (marker-buffer marker))
         (save-excursion
           (with-current-buffer (marker-buffer marker)
             (goto-char marker)
             (let (begin end str)
               (beginning-of-line)
               (setq begin (point))
               (end-of-line)
               (setq end (point))
               (setq str (buffer-substring begin end))
               (chomp str)
               (if (> (length str) 0)
                   str
                 marker)))))
        (t
         marker)))

(defun jda-hl-comment-list-message ()
  (let* ((length (ring-length jda-hl-comment-ring))
         (message (format "Comment list(prev: ',', next: '.', jump: 'a~%c')\n\n"
                          (+ ?a (- length 1))))
         marker)
    (dotimes (i length)
      (setq marker (ring-ref jda-hl-comment-ring i))
      (setq message (concat message
                            (format "%s[%c] %s:%d:%s\n"
                                    (if (equal i jda-hl-comment-ring-iterator)
                                        "=>"
                                      "  ")
                                    (+ ?a i)
                                    (marker-buffer marker)
                                    (jda-hl-comment-line-number-at marker)
                                    (jda-hl-comment-get-line-string marker)))))
    message))

(defun jda-hl-comment-list ()
  (interactive)
  (jda-hl-comment-push-match-markers)
  (let ((length (ring-length jda-hl-comment-ring))
        index
        (c ?,))
    (cond ((> length 0)
           (setq max-mini-window-height (+ length 3))
           (while (or (char-equal c ?,)
                      (char-equal c ?.))
             (message (jda-hl-comment-list-message))
             (setq c (read-char))
             (cond ((char-equal c ?.)
                    (jda-hl-comment-next))
                   ((char-equal c ?,)
                    (jda-hl-comment-prev))))
           (setq index (- c ?a))
           (if (and (>= index 0)
                    (< index length))
               (jda-hl-comment-jump (ring-ref jda-hl-comment-ring index)))
           (setq max-mini-window-height 0.25))
          (t
           (message "There is no comment list !!!")))))

(defun jda-hl-comment-finish-jump ()
  (interactive)
  (setq jda-hl-comment-ring-iterator -1)
  (condition-case nil
      (progn
        (jda-hl-comment-jump (ring-ref jda-hl-comment-ring 0))
        (jda-hl-comment-list))
    (error nil)))

(defvar jda-hl-comment-mode-map nil)

(defun jda-hl-comment-add-hook ()
  (mapcar (lambda (entry)
            (let ((mode (cadr entry)))
              (if (not (null mode))
                  (add-hook (intern mode)
                            'jda-hl-comment-mode-initialize))))
          jda-hl-comment-region-regexp-alist))

(defun jda-hl-comment-remove-hook ()
  (mapcar (lambda (entry)
            (let ((mode (cadr entry)))
              (if (not (null mode))
                  (remove-hook (intern mode)
                               'jda-hl-comment-mode-initialize))))
          jda-hl-comment-region-regexp-alist))

;;;###autoload
(define-minor-mode jda-hl-comment-mode
  "Toggle JDA(Jonggyu Development Assistant) highlight comment mode

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix arugment turns off the mode.

Key bindings:
\\{jda-hl-comment-mode-map}."
  ;; init-value
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " hl-c"
  ;; The minor mode bindings.
  :keymap jda-hl-comment-mode-map
  :group 'jda-hl-comment
  :global t
  (cond (jda-hl-comment-mode
         ;; initialize
         (define-key global-map (kbd "C-c j /") 'jda-hl-comment-list)
         (jda-hl-comment-add-hook)
         (message "jda-hl-comment-mode enabled"))
        (t
         ;; finalize
         (define-key global-map (kbd "C-c j /") nil)
         (jda-hl-comment-remove-hook)
         (jda-hl-comment-mode-finalize)
         (message "jda-hl-comment-mode disabled"))))

;;;###autoload
(defun jda-hl-comment-customize ()
  "Customize JDA(Jonggyu Development Assistant) highlight comment"
  (interactive)
  (customize-group 'jda-hl-comment))

(provide 'jda-highlight-comment)

;;; jda-highlight-comment.el ends here
