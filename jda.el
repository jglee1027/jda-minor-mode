;;; jda.el --- Jong-Gyu Development Assistant minor mode for Developers

;; Copyright (C) 2011 Lee Jong-Gyu<jglee1027@gmail.com>

;; Authors: Lee Jong-Gyu<jglee1027@gmail.com>
;; Maintainer: Lee Jong-Gyu<jglee1027@gmail.com>
;; Created: 18 Mar 2011
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
;;     (require 'jda)
;;     (jda-minor-mode)

;; * Major commands:
;; ** Build
;;    If there is Makefile or .xcodeproj in your project , you can build
;;    a project easily by pressing "C-c c" in any subdirectory of your
;;    project.

;; ** Find development doucmentation
;;    Press "C-c h" at the symbol which you want to find in a development
;;    documentation(Android or Xcode).

;; ** Open a counterpart file
;;    You can quickly open the counterpart of the current buffer by
;;    pressing "C-c j p". In other words, if the current buffer is c or
;;    cpp source file, you can easily visit the header file related it by
;;    pressing "C-c j p".

;; ** Find a symbol in the project
;;    You can simply search positions which a symbol at point is used in
;;    your project by pressing "C-c j s". It uses (grep-find).

;; ** Find a file in the project
;;    You can rapidly find a file in your project by pressing "C-c j f".

;; ** Visit a file in the project
;;    You can easily open the file you want to visit in your project by
;;    pressing "C-c j i" or "C-c i". It supports incremental search like
;;    TextMate or Visual Assist. As you type text, one or more possible
;;    matches for the text are found and immediately displayed.

;; ** Find a symbol in the project
;;    If you want to find a symbol in your project, press "C-c j s".
;;    After finding it, you can easily navigate the symbols by pressing
;;    "M-g p" or "M-g n".

;; ** Go to a symbol in the current buffer
;;    Press "C-c j m" or "C-c m" if you want to go to a function. You can
;;    see all functions defined in the current buffer. It supports
;;    incremental search like TextMate or Visual Assist.

;; ** Replace a string in several files
;;    If you want to replace a string in several files, press "C-c j 5"
;;    or "C-c j %". You can easily replace a string in specified files in
;;    your project.

;; ** Create a TAG in your project
;;    Press "C-c j t" and you can easily create TAG file in your project
;;    using 'find' and 'etags'.

;; ** Go to previous or next marker
;;    Press "C-c ," to go to the previous marker and Press "C-c ." to go
;;    to the next marker.
;;    Press "C-x <down>" to save the current marker.

;; ** Highlight symbol
;;    If you want to see highlighted symbol at point, press "C-c j h".
;;    After specified idle time, the current symbol at point is
;;    highlighted.  It only works in file buffer.

;; ** Insert or Delete a bracket in Objective-C mode
;;    Press "C-c ]" to insert a right bracket to pair.
;;    Press "C-c [" to delete left and right brackets to pair.

;;   See `jda-minor-keymap' for more details.

;;; Code:

(require 'cl)
(require 'imenu)
(require 'ido)
(require 'hi-lock)
(require 'thingatpt)
(require 'dired-aux)

(defgroup jda nil
  "Jong-Gyu Development Assistant"
  :group 'applications
  :prefix "jda-")

(defvar jda-create-tags-command nil)
(defvar jda-create-tags-command-history nil)
(defvar jda-create-tags-directory nil)
(defvar jda-create-tags-directory-history nil)
(defvar jda-etags-tag-info-alist nil)
(defvar jda-get-extensions-alist '(("c"     . ("h"))
                                   ("cc"    . ("h"))
                                   ("cpp"   . ("h"))
                                   ("cxx"   . ("h"))
                                   ("m"     . ("h"))
                                   ("mm"    . ("h"))
                                   ("h"     . ("c" "cc" "cpp" "cxx" "m" "mm"))))
(defvar jda-gf-symbol-history nil)
(defvar jda-gf-symbol-command-history nil)
(defvar jda-gf-find-file-history nil)
(defvar jda-gf-find-file-command-history nil)
(defvar jda-gf-replace-file-history nil)
(defvar jda-gf-replace-file-command-history nil)
(defvar jda-gf-project-root nil)
(defvar jda-gf-project-root-history nil)
(defvar jda-gf-grep-query-command-history nil)
(defvar jda-gf-exclusive-path-history nil)
(defvar jda-gf-grep-query-replace-buffers-alist nil)

(defvar jda-git-grep-dir-history nil)
(defvar jda-git-grep-regexp-history nil)
(defvar jda-git-grep-command-history nil)

(defvar jda-ido-find-file-files-alist nil)
(defvar jda-ido-find-file-files-alist-root nil)

(defvar jda-marker-ring-max 26)
(defvar jda-marker-ring (make-ring jda-marker-ring-max))
(defvar jda-marker-ring-iterator -1)
(defvar jda-marker-bookmark-max 26)
(defvar jda-marker-bookmark (make-vector jda-marker-bookmark-max nil))

(defvar jda-make-command-history nil)
(defvar jda-xcode-doc-text-history nil)
(defvar jda-xcode-sdk-history nil)
(defvar jda-xcode-project-history nil)
(defvar jda-xcode-configuration-history nil)
(defvar jda-xcode-target-history nil)
(defvar jda-xcode-scheme-history nil)
(defvar jda-xcode-build-history nil)

(defvar jda-highlight-symbol-color 'hi-blue)
(defvar jda-highlight-symbol-timer-interval 0.7)
(defvar jda-highlight-symbol-timer nil)
(defvar jda-highlight-symbol-buffers-alist nil)
(defvar jda-highlight-symbol-regex nil)
(make-variable-buffer-local 'jda-highlight-symbol-regex)

(defvar jda-kill-ring-save-is-set t)

(defcustom jda-gf-assoc-extension-alist
  '(("c"        . "*.[cChH] *.[cC][pP][pP] *.[mM] *.[mM][mM]")
    ("cpp"      . "*.[cChH] *.[cC][pP][pP] *.[mM] *.[mM][mM]")
    ("h"        . "*.[cChH] *.[cC][pP][pP] *.[mM] *.[mM][mM]")
    ("m"        . "*.[cChH] *.[cC][pP][pP] *.[mM] *.[mM][mM]")
    ("mm"       . "*.[cChH] *.[cC][pP][pP] *.[mM] *.[mM][mM]")
    ("java"     . "*.java")
    ("el"       . "*.el")
    ("lisp"     . "*.lisp")
    ("rb"       . "*.rb")
    (nil        . "*.*"))
  "Counterpart extensions"
  :type 'alist
  :group 'jda)

(defcustom jda-gf-exclusive-path
  "*.git* *.svn* *.cvs* *.class *.obj *.o *.a *.so *~ *# *.cache *TAGS *cscope.out"
  "Paths to exclude while find command runs"
  :type 'string
  :group 'jda)

(defcustom jda-android-sdk-dir
  "~/sdk/android-sdk-linux_86"
  "Android SDK directory"
  :type 'directory
  :group 'jda)

(defcustom jda-xcode-available-sdks
  nil
  "Lists all available SDKs in Xcode"
  :type 'list
  :group 'jda)

(defcustom jda-xcodebuild-command
  "xcrun xcodebuild"
  "Xcodebuild default command"
  :type 'string
  :group 'jda)

(defcustom jda-compilation-buffer-name
  "*compilation*"
  "Compilation buffer name"
  :type 'string
  :group 'jda)

(defcustom jda-find-command
  "find -L"
  "find command"
  :type 'string
  :group 'jda)

(defcustom jda-git-grep-command
  "cd %s && GIT_PAGER='' git grep -nH --color -i \"%s\"" ; git-grep-dir regexp
  "git-grep command"
  :type 'string
  :group 'jda)

(defcustom jda-visit-file-super-dir-depth
  3
  "the value of super-dir-depth parameter of (jda-visit-file-in-dirs)"
  :type 'integer
  :group 'jda)

(defcustom jda-visit-file-sub-dir-depth
  3
  "the value of sub-dir-depth parameter of (jda-visit-file-in-dirs)"
  :type 'integer
  :group 'jda)

;;;; common functions

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun jda-advice-completing-read (prompt
                                   collection
                                   &optional
                                   predicate
                                   require-match
                                   initial-input
                                   history)
  (cond ((functionp 'helm)
         (let* ((helm-source
                 `((name . ,prompt)
                   (candidates . ,collection)
                   (action . (lambda (candidate) candidate)))))
           (helm :sources '(helm-source))))
        ((functionp 'ido-completing-read)
         (ido-completing-read prompt collection predicate require-match
                              initial-input history))
        (t
         (completing-read prompt collection predicate require-match
                          initial-input history))))

(defun jda-icompleting-read (prompt choices)
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt)))

(defun jda-read-shell-command (prompt initial-contents &optional history)
  (if (functionp 'read-shell-command)
      (read-shell-command prompt
                          initial-contents
                          history)
    (read-from-minibuffer prompt
                          initial-contents
                          nil
                          nil
                          history)))

(defun jda-is-directory (path)
  (equal (car (file-attributes path)) t))

(defun jda-get-super-directory (path)
  (replace-regexp-in-string "/[^/]*$"
                            ""
                            path))

(defmacro jda-advice-ffap-read-file-or-url (prompt initial-dir &optional history)
  `(directory-file-name
    (if (functionp 'helm-advice--ffap-read-file-or-url)
        (helm-advice--ffap-read-file-or-url ,prompt ,initial-dir)
      (completing-read ,prompt
                       'ffap-read-file-or-url-internal
                       nil
                       nil
                       ,initial-dir
                       ,history))))

(defmacro jda-set-default-directory (prompt dir history)
  `(or ,dir (setq ,dir default-directory))
  `(setq ,dir (jda-advice-ffap-read-file-or-url ,prompt ,dir ,history)))

;;;; jda-highlight

;; callback function
(defun jda-highlight-symbol-callback ()
  (save-excursion
    (let* ((symbol (symbol-at-point))
           (symbol-regex (cond ((and symbol)
                                (concat "\\<"
                                        (regexp-quote (symbol-name symbol))
                                        "\\>"))
                               (t
                                nil))))
      (cond ((and symbol-regex
                  (not (string= symbol-regex jda-highlight-symbol-regex))
                  (buffer-file-name))
             (hi-lock-unface-buffer jda-highlight-symbol-regex)
             (hi-lock-face-buffer symbol-regex
                                  jda-highlight-symbol-color)
             (setq jda-highlight-symbol-buffers-alist
                   (assq-delete-all (current-buffer)
                                    jda-highlight-symbol-buffers-alist))
             (add-to-list 'jda-highlight-symbol-buffers-alist
                          (list (current-buffer) symbol-regex) t)
             (setq jda-highlight-symbol-regex symbol-regex))))))

;; start function
(defun jda-highlight-symbol-run-toggle ()
  (interactive)
  (cond ((timerp jda-highlight-symbol-timer)
         (cancel-timer jda-highlight-symbol-timer)
         (while jda-highlight-symbol-buffers-alist
           (let* ((entry (pop jda-highlight-symbol-buffers-alist))
                  (buffer (pop entry))
                  (symbol-regex (pop entry)))
             (condition-case nil
                 (progn
                   (set-buffer buffer)
                   (hi-lock-unface-buffer jda-highlight-symbol-regex))
               (error nil))))
         (message "jda-highlight-symbol off.")
         (setq jda-highlight-symbol-timer nil))
        (t
         (unless hi-lock-mode (hi-lock-mode 1))
         (setq jda-highlight-symbol-timer
               (run-with-idle-timer jda-highlight-symbol-timer-interval
                                    t
                                    'jda-highlight-symbol-callback))
         (message "jda-highlight-symbol on."))))

;;;; jda-marker

(defun jda-marker-line-number-at (marker)
  (save-excursion
    (with-current-buffer (marker-buffer marker)
      (line-number-at-pos marker))))

;; jump-to-register in register.el
(defun jda-marker-jump (marker)
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

(defun jda-marker-push-marker (&optional use-jda-marker-list)
  (interactive)
  (setq jda-marker-ring-iterator -1)
  (let ((last-marker nil)
        (curr-marker (point-marker)))
    (condition-case nil
        (cond ((ring-empty-p jda-marker-ring)
               (ring-insert jda-marker-ring curr-marker)
               (if use-jda-marker-list
                   (jda-marker-list)))
              (t
               (setq last-marker (ring-ref jda-marker-ring 0))
               (cond ((not (equal last-marker curr-marker))
                      (ring-insert jda-marker-ring curr-marker)
                      (if use-jda-marker-list
                          (jda-marker-list))))))
      (error nil))))

(defun jda-marker-push-marker-menu ()
  (interactive)
  (jda-marker-push-marker t))

(defun jda-marker-prev ()
  (interactive)
  (cond ((equal jda-marker-ring-iterator -1)
         (jda-marker-push-marker)
         (setq jda-marker-ring-iterator 0)))
  (condition-case nil
      (let* ((prev-iterator (mod (1+ jda-marker-ring-iterator)
                                 (ring-length jda-marker-ring)))
             (prev-marker (ring-ref jda-marker-ring prev-iterator)))
        (jda-marker-jump prev-marker)
        (setq jda-marker-ring-iterator prev-iterator))
    (error nil)))

(defun jda-marker-prev-ui ()
  (interactive)
  (jda-marker-prev)
  (jda-marker-list))

(defun jda-marker-next ()
  (interactive)
  (cond ((equal jda-marker-ring-iterator -1)
         (message "Should run the command 'jda-marker-prev'"))
        (t
         (condition-case nil
             (let* ((next-iterator (mod (1- jda-marker-ring-iterator)
                                        (ring-length jda-marker-ring)))
                    (next-marker (ring-ref jda-marker-ring next-iterator)))
               (jda-marker-jump next-marker)
               (setq jda-marker-ring-iterator next-iterator))
           (error nil)))))

(defun jda-marker-next-ui ()
  (interactive)
  (jda-marker-next)
  (jda-marker-list))

(defun jda-marker-get-line-string (marker)
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

(defun jda-marker-list-message ()
  (let* ((length (ring-length jda-marker-ring))
         (message (format "Marker list(push: C-x ?, prev: \"C-x ,\", next: \"C-x .\", last: C-x /, jump: a~%c)\n\n"
                          (+ ?a (- length 1))))
         marker)
    (dotimes (i length)
      (setq marker (ring-ref jda-marker-ring i))
      (setq message (concat message
                            (format "%s[%c] %s:%d:%s\n"
                                    (if (equal i jda-marker-ring-iterator)
                                        "=>"
                                      "  ")
                                    (+ ?a i)
                                    (marker-buffer marker)
                                    (jda-marker-line-number-at marker)
                                    (jda-marker-get-line-string marker)))))
    message))

(defun jda-marker-list ()
  (interactive)
  (let ((length (ring-length jda-marker-ring))
        index
        (c ?,))
    (setq max-mini-window-height (+ length 3))
    (while (or (char-equal c ?,)
               (char-equal c ?.))
      (message (jda-marker-list-message))
      (setq c (read-char))
      (cond ((char-equal c ?,)
             (jda-marker-prev))
            ((char-equal c ?.)
             (jda-marker-next))))
    (setq index (- c ?a))
    (if (and (>= index 0)
             (< index length))
        (jda-marker-jump (ring-ref jda-marker-ring index)))
    (setq max-mini-window-height 0.25)))

(defun jda-marker-finish-jump ()
  (interactive)
  (setq jda-marker-ring-iterator -1)
  (condition-case nil
      (progn
        (jda-marker-jump (ring-ref jda-marker-ring 0))
        (jda-marker-list))
    (error nil)))

(defun jda-marker-bookmark-message (mode)
  (let ((message (format "Press key(a~%c) to %s the current marker:\n\n"
                         (+ ?a (- jda-marker-bookmark-max 1))
                         mode))
        marker)
    (dotimes (i jda-marker-bookmark-max)
      (setq marker (elt jda-marker-bookmark i))
      (setq message (concat message
                            (if (or (null marker) (null (marker-buffer marker)))
                                (format " [%c]\n" (+ ?a i))
                              (format " [%c] %s:%d:%s\n"
                                      (+ ?a i)
                                      (marker-buffer marker)
                                      (jda-marker-line-number-at marker)
                                      (jda-marker-get-line-string marker))))))
    message))

(defun jda-marker-bookmark-save ()
  (interactive)
  (let ((c ?a)
        (max-mini-window-height-old max-mini-window-height)
        (curr-marker (point-marker))
        i)
    (setq max-mini-window-height (+ jda-marker-bookmark-max 3))
    (setq c (read-char-exclusive (jda-marker-bookmark-message "save")))
    (setq i (- c ?a))
    (setq max-mini-window-height max-mini-window-height-old)
    (cond ((and (>= i 0) (< i jda-marker-bookmark-max))
           (aset jda-marker-bookmark i curr-marker)
           (message (format "[%c] %s was saved" c curr-marker))))))

(defun jda-marker-bookmark-restore ()
  (interactive)
  (let ((c ?a)
        (max-mini-window-height-old max-mini-window-height)
        (curr-marker (point-marker))
        i)
    (setq max-mini-window-height (+ jda-marker-bookmark-max 3))
    (setq c (read-char-exclusive (jda-marker-bookmark-message "restore")))
    (setq i (- c ?a))
    (setq max-mini-window-height max-mini-window-height-old)
    (cond ((and (>= i 0) (< i jda-marker-bookmark-max))
           (jda-marker-jump (elt jda-marker-bookmark i))))))

;;;; utility functions
(defun jda-file-name (filename)
  (if (memq system-type '(ms-dos windows-nt cygwin))
      (replace-regexp-in-string "^/\\([a-zA-Z]\\)/" "\\1:/" filename)
    filename))

(defun jda-get-sub-directory-list (dir)
  (let ((entries (directory-files dir t))
        (sub-dirs '()))
    (mapcar (lambda (entry) (if (jda-is-directory entry)
                                (add-to-list 'sub-dirs entry t)))
            entries)
    sub-dirs))

(defun jda-current-file-name-extension ()
  (let (extension)
    (cond ((null (buffer-file-name))
           nil)
          ((null (setq extension (file-name-extension (buffer-file-name))))
           nil)
          (t
           (setq extension (downcase extension))))))

(defun jda-get-extensions-to-visit ()
  (let ((extension (jda-current-file-name-extension)))
    (cond ((null extension)
           nil)
          (t
           (cdr (assoc extension jda-get-extensions-alist))))))

(defun jda-visit-file (file-name-sans-ext extensions)
  (let (file-name file-ext)
    (while (not (equal (setq file-ext (pop extensions)) nil))
      (setq file-name (concat file-name-sans-ext "." file-ext))
      (if (file-exists-p file-name)
          (progn
            (find-file file-name)
            (throw 'visit-file-exception file-name))))))

(defun jda-visit-file-in-sub-dirs (sub-dir-list file-name-non-dir extensions)
  (let (file-name-sans-ext)
    (mapcar (lambda (entry)
              (setq file-name-sans-ext (concat entry "/" file-name-non-dir))
              (jda-visit-file file-name-sans-ext extensions))
            sub-dir-list)))

(defun jda-visit-file-in-dirs (super-dir-depth sub-dir-depth)
  (let ((extensions-to-visit (jda-get-extensions-to-visit))
        file-name-sans-ext
        file-name-non-dir
        current-dir
        sub-dir-list)

    (if (or (equal extensions-to-visit nil)
            (equal (buffer-file-name) nil))
        (throw 'visit-file-exception "Not supported!"))

    (setq file-name-sans-ext (file-name-sans-extension (buffer-file-name)))
    (setq file-name-non-dir (file-name-nondirectory file-name-sans-ext))

    ;; in current directory
    ;; in super directory
    ;; in sub-dir of super-dir
    ;; ...
    (setq current-dir (file-name-directory file-name-sans-ext))
    (setq sub-dir-list (jda-get-sub-directory-list current-dir))
    (dotimes (i super-dir-depth)
      (setq current-dir (jda-get-super-directory current-dir))
      (if (equal current-dir "")
          (cl-return))
      (setq sub-dir-list (jda-get-sub-directory-list current-dir))
      (jda-visit-file-in-sub-dirs sub-dir-list
                                  file-name-non-dir
                                  extensions-to-visit))

    ;; in sub-dir of sub-dir
    ;; ...
    (setq current-dir (file-name-directory file-name-sans-ext))
    (setq sub-dir-list (jda-get-sub-directory-list current-dir))
    (let ((sub-dir-all-list nil))
      (dotimes (i sub-dir-depth)
        (mapcar (lambda (entry)
                  (let ((dirs (jda-get-sub-directory-list entry)))
                    (mapcar (lambda (x)
                              (delq x sub-dir-all-list)
                              (add-to-list 'sub-dir-all-list x t))
                            dirs))
                  entry)
                sub-dir-list)
        (setq sub-dir-list sub-dir-all-list))
      (jda-visit-file-in-sub-dirs sub-dir-all-list
                                  file-name-non-dir
                                  extensions-to-visit))))

(defun jda-visit-file-in-project ()
  (interactive)
  (let ((file-name-sans-ext (file-name-sans-extension (buffer-name)))
        (same-name-files-list nil)
        (extensions-to-visit (jda-get-extensions-to-visit))
        (same-name-files-count 0)
        file-name
        file-ext)

    (if (null jda-ido-find-file-files-alist)
        (throw 'visit-file-exception "Not found! Set project root directory"))

    (while (not (null (setq file-ext (pop extensions-to-visit))))
      (setq file-name (concat file-name-sans-ext "." file-ext))
      (cond ((not (null (assoc file-name jda-ido-find-file-files-alist)))
             (mapcar (lambda (x)
                       (cond ((equal file-name (car x))
                              (add-to-list 'same-name-files-list
                                           (car (cdr x)))
                              (setq same-name-files-count
                                    (1+ same-name-files-count)))))
                     jda-ido-find-file-files-alist)
             (cond ((equal same-name-files-count 1)
                    (throw 'visit-file-exception
                           (buffer-file-name
                            (find-file (car same-name-files-list)))))
                   ((> same-name-files-count 1)
                    (throw 'visit-file-exception
                           (buffer-file-name
                            (find-file
                             (jda-advice-completing-read "Find file: "
                                                         same-name-files-list)))))))))))

(defun jda-open-counterpart-file ()
  "open the header or source file related with the current file."
  (interactive)
  (message (catch 'visit-file-exception
             (jda-visit-file-in-dirs jda-visit-file-super-dir-depth
                                     jda-visit-file-sub-dir-depth)
             (jda-visit-file-in-project)
             (throw 'visit-file-exception "Not found!"))))

(defun jda-kill-ring-save-toggle ()
  (interactive)
  (cond (jda-kill-ring-save-is-set
         (message "jda-kill-ring-save off")
         (setq jda-kill-ring-save-is-set nil))
        (t
         (message "jda-kill-ring-save on")
         (setq jda-kill-ring-save-is-set t))))

(defun jda-kill-ring-save (begin end)
  (interactive "r")
  (cond ((null (functionp 'use-region-p))
         (kill-ring-save begin end))
        ((and jda-kill-ring-save-is-set
              (not (use-region-p)))
         (let ((bounds (bounds-of-thing-at-point 'symbol)))
           (if bounds
               (copy-region-as-kill (car bounds) (cdr bounds))
             (kill-ring-save begin end))))
        (t
         (kill-ring-save begin end))))

(defun jda-delete-trailing-whitespace (begin end)
  (interactive "r")
  (if mark-active
      (replace-regexp "^[[:space:]]+$\\|[[:space:]]+$" "" nil begin end)
    (query-replace-regexp "^[[:space:]]+$\\|[[:space:]]+$" "")))

(defun jda-gf-get-find-exclusive-path-options ()
  (let (path-list path-option)
    (if (equal jda-gf-exclusive-path "")
        (setq path-option "")
      (progn
        (setq path-list (mapcar (lambda (x) (format "-path '%s'" x))
                                (split-string jda-gf-exclusive-path)))
        (setq path-option (pop path-list))
        (while path-list
          (setq path-option (concat path-option " -o " (pop path-list))))
        (setq path-option (concat "! \\( " path-option " \\)"))))))

(defun jda-gf-get-find-name-options (files)
  (let (name-list name-option)
    (if (equal files "")
        (setq name-option "")
      (progn
        (setq name-list (mapcar (lambda (x) (format "-name '%s'" x))
                                (split-string files)))
        (setq name-option (pop name-list))
        (while name-list
          (setq name-option (concat name-option " -o " (pop name-list))))
        (setq name-option (concat "\\( " name-option " \\)"))))))

(defun jda-gf-get-assoc-find-name-options ()
  (let (extension assoc-extensions)
    (setq extension (jda-current-file-name-extension))
    (setq assoc-extensions (cdr (assoc extension jda-gf-assoc-extension-alist)))
    (setq assoc-extensions (read-from-minibuffer "Files: " assoc-extensions))
    (jda-gf-get-find-name-options assoc-extensions)))

(defun jda-gf-set-exclusive-path ()
  (interactive)
  (setq jda-gf-exclusive-path
        (read-from-minibuffer "Exclusive paths: "
                              jda-gf-exclusive-path
                              nil
                              nil
                              'jda-gf-exclusive-path-history)))

(defun jda-gf-set-project-root ()
  "set a project root directory for grep-find"
  (interactive)
  (jda-set-default-directory "Project root: "
                             jda-gf-project-root
                             'jda-gf-project-root-history))

(defun jda-gf-set-project-root-with-default-directory ()
  "set a project root directory for grep-find"
  (interactive)
  (let ((project-root nil))
    (setq jda-gf-project-root
          (jda-set-default-directory "Project root: "
                                     project-root
                                     'jda-gf-project-root-history))))

(defun jda-pop-to-compilation-buffer ()
  (interactive)
  (pop-to-buffer jda-compilation-buffer-name))

(defun jda-pop-to-compilation-buffer-fill ()
  (interactive)
  (pop-to-buffer jda-compilation-buffer-name)
  (delete-other-windows))

(defun jda-gf-select-grep-buffer (current-buffer msg)
  (condition-case nil
      (progn
        (select-window (get-buffer-window current-buffer))
        (forward-line 4)
        (setq compilation-finish-function nil))
    (error nil)))

(defun jda-gf-symbol-at-point ()
  "grep-find with symbol at current point."
  (interactive)
  (jda-marker-push-marker)
  (let (symbol)
    (jda-gf-set-project-root)
    (setq symbol (symbol-at-point))
    (if (null symbol)
        (setq symbol ""))
    (setq symbol (read-from-minibuffer "Find symbol: "
                                       (format "%s" symbol)
                                       nil
                                       nil
                                       'jda-gf-symbol-history))
    (setq compilation-finish-function 'jda-gf-select-grep-buffer)
    (grep-find (jda-read-shell-command "Command: "
                                       (format "%s %s -type f %s %s -print0 | xargs -0 grep -nH -e '\\<%s\\>'"
                                               jda-find-command
                                               jda-gf-project-root
                                               (jda-gf-get-assoc-find-name-options)
                                               (jda-gf-get-find-exclusive-path-options)
                                               symbol)
                                       'jda-gf-symbol-command-history))))

(defun jda-gf-text-at-point ()
  "grep-find with text at current point."
  (interactive)
  (jda-marker-push-marker)
  (let (symbol)
    (jda-gf-set-project-root)
    (setq symbol (symbol-at-point))
    (if (null symbol)
        (setq symbol ""))
    (setq symbol (read-from-minibuffer "Find text: "
                                       (format "%s" symbol)
                                       nil
                                       nil
                                       'jda-gf-symbol-history))
    (setq compilation-finish-function 'jda-gf-select-grep-buffer)
    (grep-find (jda-read-shell-command "Command: "
                                       (format "%s %s -type f %s %s -print0 | xargs -0 grep -nH -e '%s'"
                                               jda-find-command
                                               jda-gf-project-root
                                               (jda-gf-get-assoc-find-name-options)
                                               (jda-gf-get-find-exclusive-path-options)
                                               symbol)
                                       'jda-gf-symbol-command-history))))

(defun jda-git-grep ()
  "git-grep with regexp in git repository"
  (interactive)
  (jda-marker-push-marker)
  (let (regexp git-grep-dir)
    (setq git-grep-dir (chomp
                        (shell-command-to-string "git rev-parse --show-toplevel")))
    (jda-set-default-directory "Git grep dir: "
                               git-grep-dir
                               'git-grep-dir-history)
    (setq regexp (symbol-at-point))
    (if (null regexp)
        (setq regexp ""))
    (cond ((functionp 'helm-grep-git-1)
           (helm-grep-git-1 git-grep-dir t (format "%s" regexp)))
          (t
           (setq regexp (read-from-minibuffer "Search in git repo: "
                                              (format "%s" regexp)
                                              nil
                                              nil
                                              'jda-git-grep-regexp-history))
           (setq compilation-finish-function 'jda-gf-select-grep-buffer)
           (grep-find (jda-read-shell-command "$ "
                                              (format jda-git-grep-command
                                                      git-grep-dir
                                                      regexp)
                                              'jda-git-grep-command-history))))))


(defun jda-gf-grep-query-replace-in-current-line (from to buffer)
  (let (begin end)
    (with-current-buffer buffer
      (save-excursion
        (beginning-of-line)
        (setq begin (point))
        (end-of-line)
        (setq end (point)))
      (replace-regexp from to nil begin end))))

(defun jda-gf-grep-query-replace-ui (from to &optional delimited)
  (interactive
   (let ((common
          (query-replace-read-args
           "Query replace regexp in found files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))

  (condition-case nil
      (next-error)
    (error "Query replace finished"))
  (condition-case nil
      (previous-error)
    (error nil))

  (let ((done nil)
        (all nil)
        (count 0)
        key
        buffer)
    (message "")

    (while (not done)
      (with-current-buffer "*grep*"
        (setq buffer (get-buffer
                      (file-name-nondirectory
                       (car
                        (car (nth 2 (car (compilation-next-error 0)))))))))

      (with-current-buffer buffer
        (setq jda-gf-grep-query-replace-buffers-alist
              (assq-delete-all buffer
                               jda-gf-grep-query-replace-buffers-alist))
        (add-to-list 'jda-gf-grep-query-replace-buffers-alist
                     (list buffer from) t)
        (hi-lock-face-buffer from 'query-replace))

      (message (format "Query replacing '%s' with '%s' (y/n/a/q)?" from to))
      (setq key (read-event))

      (cond ((equal key ?y)
             (setq count (+ 1 count))
             (jda-gf-grep-query-replace-in-current-line from to buffer)
             (condition-case nil
                 (next-error)
               (error
                (setq done t))))
            ((equal key ?n)
             (condition-case nil
                 (next-error)
               (error
                (setq done t))))
            ((equal key ?a)
             (setq all t)
             (setq done t))
            ((equal key ?q)
             (setq done t))))

    (setq done nil)
    (cond (all
           (while (not done)
             (jda-gf-grep-query-replace-in-current-line from to buffer)
             (setq count (+ 1 count))
             (condition-case nil
                 (next-error)
               (error
                (setq done t))))))

    (while jda-gf-grep-query-replace-buffers-alist
      (let* ((entry (pop jda-gf-grep-query-replace-buffers-alist))
             (buffer (pop entry))
             (symbol-regex (pop entry)))
        (condition-case nil
            (progn
              (set-buffer buffer)
              (hi-lock-unface-buffer symbol-regex))
          (error nil))))

    (message "Replaced %d occurrences" count)))

(defun jda-gf-grep-query-replace-old (from to &optional delimited)
  (interactive
   (let ((common
          (query-replace-read-args
           "Query replace regexp in files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))

  (jda-marker-push-marker)
  (jda-gf-set-project-root)
  (let (name-option
        command
        extension)
    (cond ((null (buffer-file-name))
           (setq name-option ""))
          (t
           (setq extension (jda-current-file-name-extension))
           (setq name-option (cdr (assoc
                                   extension
                                   jda-gf-assoc-extension-alist)))))
    (setq name-option (jda-gf-get-find-name-options
                       (read-from-minibuffer "Find file: "
                                             name-option)))
    (setq command (jda-read-shell-command
                   "Command: "
                   (format "%s %s -type f %s %s -print0 | xargs -0 grep -nH -e '%s'"
                           jda-find-command
                           jda-gf-project-root
                           name-option
                           (jda-gf-get-find-exclusive-path-options)
                           from)
                   'jda-gf-grep-query-command-history))

    (shell-command command "*grep*")
    (with-current-buffer "*grep*"
      (grep-mode)
      (hi-lock-face-buffer from
                           'match))
    (jda-gf-grep-query-replace-ui from to)))

(defun jda-buffer-file-name ()
  (interactive)
  (message (buffer-file-name)))

;;;; dired-mode

;; dired-aux.el
;;;###autoload
(defun dired-do-query-replace-regexp-jda (from to &optional delimited)
  "Do `query-replace-regexp' of FROM with TO, on all marked files.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue]."
  (interactive
   (let ((common
          (query-replace-read-args
           "Query replace regexp in marked files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (dolist (file (dired-get-marked-files nil nil 'dired-nondirectory-p))
    (let ((buffer (get-file-buffer file)))
      (if (and buffer (with-current-buffer buffer
                        buffer-read-only))
          (error "File `%s' is visited read-only" file))))

  (let ((dired-buffer (current-buffer)))
    (beginning-of-buffer)
    (dolist (file (dired-get-marked-files nil nil 'dired-nondirectory-p))
      (pop-to-buffer dired-buffer)
      (dired-next-marked-file 1)
      (hl-line-mode 1)
      (find-file-other-window file)
      (beginning-of-buffer)
      (query-replace-regexp from to delimited))))

(defun jda-dired-mode-keymap ()
  (define-key dired-mode-map "%%" 'dired-do-query-replace-regexp-jda))

(defun jda-gf-grep-query-replace (from to &optional delimited)
  (interactive
   (let ((common
          (query-replace-read-args
           "Query replace regexp in files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))

  (jda-marker-push-marker)
  (jda-gf-set-project-root)
  (let (name-option
        find-args
        extension)
    (cond ((null (buffer-file-name))
           (setq name-option ""))
          (t
           (setq extension (jda-current-file-name-extension))
           (setq name-option (cdr (assoc
                                   extension
                                   jda-gf-assoc-extension-alist)))))
    (setq name-option (jda-gf-get-find-name-options
                       (read-from-minibuffer "Find file: "
                                             name-option)))
    (find-dired jda-gf-project-root
                (concat "-type f " name-option
                        " -exec " grep-program " " find-grep-options " -e "
                        (shell-quote-argument from)
                        " "
                        (shell-quote-argument "{}")
                        " "
                        (shell-quote-argument ";")))))

(defun jda-gf-find-file ()
  "search a file."
  (interactive)
  (jda-marker-push-marker)
  (let (files)
    (jda-gf-set-project-root)
    (setq files (read-from-minibuffer "Find file: "
                                      nil
                                      nil
                                      nil
                                      'jda-gf-find-file-history))
    (setq compilation-finish-function 'jda-gf-select-grep-buffer)
    (grep-find (jda-read-shell-command "Command: "
                                       (format "%s %s -type f %s "
                                               jda-find-command
                                               jda-gf-project-root
                                               (jda-gf-get-find-name-options files))
                                       'jda-gf-find-file-command-history))))

(defun jda-gf-find-file-dired ()
  "search a file."
  (interactive)
  (jda-marker-push-marker)
  (let (files)
    (jda-gf-set-project-root)
    (setq files (read-from-minibuffer "Find file: "
                                      nil
                                      nil
                                      nil
                                      'jda-gf-find-file-history))
    (setq compilation-finish-function 'jda-gf-select-grep-buffer)
    (find-dired jda-gf-project-root
                (jda-gf-get-find-name-options files))))

(defun jda-gf-get-query-replace-files ()
  (let ((files nil))
    (with-current-buffer "*jda-query-replace*"
      (let (start end)
        (goto-char (point-min))
        (while (not (eobp))
          (setq start (point))
          (forward-line 1)
          (setq end (- (point) 1))
          (add-to-list 'files (buffer-substring start end) t)
          (setq start (point)))))

    (dolist (file files)
      (let ((buffer (get-file-buffer file)))
        (if (and buffer (with-current-buffer buffer
                          buffer-read-only))
            (error "File `%s' is visited read-only" file))))
    files))

(defun jda-gf-find-query-replace (from to &optional delimited)
  (interactive
   (let ((common
          (query-replace-read-args
           "Query replace regexp in files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (jda-marker-push-marker)
  (let (files)
    (jda-gf-set-project-root)

    (cond ((null (buffer-file-name))
           (setq files ""))
          (t
           (setq files (cdr (assoc (jda-current-file-name-extension)
                                   jda-gf-assoc-extension-alist)))))

    (setq files (read-from-minibuffer "Query replace file: "
                                      files
                                      nil
                                      nil
                                      'jda-gf-replace-file-history))
    (shell-command (jda-read-shell-command "Command: "
                                           (format "%s %s -type f %s "
                                                   jda-find-command
                                                   jda-gf-project-root
                                                   (jda-gf-get-find-name-options files))
                                           'jda-gf-replace-file-command-history)
                   "*jda-query-replace*")
    (delete-other-windows)
    (condition-case err
        (tags-query-replace from to delimited '(jda-gf-get-query-replace-files))
      (error
       (kill-buffer "*jda-query-replace*")
       (message "%s" (error-message-string err))))))

(defun jda-create-tags ()
  "create TAG file."
  (interactive)
  (jda-set-default-directory "Create tags: "
                             jda-create-tags-directory
                             'jda-create-tags-directory-history)
  (shell-command (jda-read-shell-command "Command: "
                                         (format "%s %s -type f %s %s -print | etags - -o %s/TAGS"
                                                 jda-find-command
                                                 jda-create-tags-directory
                                                 (jda-gf-get-assoc-find-name-options)
                                                 (jda-gf-get-find-exclusive-path-options)
                                                 jda-create-tags-directory)
                                         'jda-create-tags-command-history)
                 "*jda-create-tag*"))

(defun jda-create-tags-in-default-directory ()
  (interactive)
  (setq jda-create-tags-directory default-directory)
  (jda-create-tags))

(defun jda-etags-make-tag-info-alist (file)
  (goto-char (point-min))
  (when (re-search-forward (concat "\f\n" "\\(" file "\\)" ",") nil t)
    (let ((path (save-excursion (forward-line 1) (file-of-tag)))
          tag-info)
      (forward-line 1)
      (while (not (or (eobp) (looking-at "\f")))
        (setq tag-info (save-excursion (etags-snarf-tag t)))
        (add-to-list 'jda-etags-tag-info-alist tag-info t)
        (forward-line 1))
      t)))

(defun jda-etags-goto-tag-in-file ()
  (interactive)
  (setq jda-etags-tag-info-alist nil)
  (let ((file (buffer-file-name)))
    (save-excursion
      (let ((first-time t)
            (gotany nil))
        (while (visit-tags-table-buffer (not first-time))
          (setq first-time nil)
          (if (jda-etags-make-tag-info-alist file)
              (setq gotany t)))
        (or gotany
            (error "File %s not in current tags tables" file))))
    (let ((tags (mapcar (lambda (x) (car x))
                        jda-etags-tag-info-alist))
          line
          tag-info)
      (setq tag-info (assoc (jda-advice-completing-read "Goto tag in file: "
                                                        tags)
                            jda-etags-tag-info-alist))
      (setq line (car (cdr tag-info)))
      (goto-line line))))

;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(defun jda-ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (jda-advice-completing-read
       "Project file: " (tags-table-files) nil t)))))

;; http://www.emacswiki.org/cgi-bin/wiki/ImenuMode
(defun jda-ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the current buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (jda-ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (jda-advice-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (jda-ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(defun jda-goto-symbol ()
  (interactive)
  (jda-marker-push-marker)
  (jda-ido-goto-symbol)
  (jda-marker-push-marker t))

(defun jda-ido-find-file ()
  (interactive)
  (jda-marker-push-marker)
  (let (chosen-name
        find-command
        same-name-files-list
        (same-name-files-count 0))
    (jda-gf-set-project-root)
    (setq find-command
          (format "%s %s -type f %s"
                  jda-find-command
                  jda-gf-project-root
                  (jda-gf-get-find-exclusive-path-options)))
    (message "Finding...")
    ;; if the previous project root directory equals to the current one,
    ;; use the previous jda-ido-find-file-files-alist to improve speed.
    (cond ((not (equal jda-gf-project-root
                       jda-ido-find-file-files-alist-root))
           (setq jda-ido-find-file-files-alist
                 (mapcar (lambda (x)
                           (list (file-name-nondirectory x) (jda-file-name x)))
                         (split-string
                          (shell-command-to-string find-command))))
           (setq jda-ido-find-file-files-alist-root jda-gf-project-root)))
    (setq chosen-name
          (jda-advice-completing-read "Project file: "
                                      (mapcar (lambda (x) (car x))
                                              jda-ido-find-file-files-alist)))
    (mapcar (lambda (x)
              (cond ((equal chosen-name (car x))
                     (add-to-list 'same-name-files-list
                                  (car (cdr x)))
                     (setq same-name-files-count
                           (1+ same-name-files-count)))))
            jda-ido-find-file-files-alist)
    (cond ((equal same-name-files-count 1)
           (find-file (car same-name-files-list)))
          ((> same-name-files-count 1)
           (find-file (jda-advice-completing-read "Find file: "
                                                  same-name-files-list))))))


(defun jda-ido-find-file-reset-root ()
  (interactive)
  (setq jda-ido-find-file-files-alist-root nil)
  (jda-ido-find-file))

;;; xcode

(defun jda-xcode-doc ()
  (interactive)
  (let ((text (symbol-at-point))
        (xcode-doc-fmt (concat
                        "tell application \"Xcode\" to activate \n"
                        "tell application \"System Events\"\n"
                        "   tell process \"Xcode\"\n"
                        "       keystroke \"0\" using {command down, shift down}\n"
                        "       keystroke \"%s\"\n"
                        "   end tell\n"
                        "end tell\n")))
    (if (null text)
        (setq text ""))
    (setq text (read-from-minibuffer "Find text in Xcode Doc: "
                                     (format "%s" text)
                                     nil
                                     nil
                                     'jda-xcode-doc-text-history))
    (do-applescript (format xcode-doc-fmt text))))

(defun jda-xcode-set-available-sdks ()
  (setq jda-xcode-available-sdks nil)
  (dolist (line
           (split-string (with-temp-buffer
                           (shell-command (format "%s -showsdks"
                                                  jda-xcodebuild-command)
                                          t)
                           (buffer-string))
                         "\n" t))
    (let ((items (split-string line "-sdk " t)))
      (if (> (length items) 1)
          (add-to-list 'jda-xcode-available-sdks (car (last items)))))))

(defun jda-xcode-get-xcodeprojs ()
  (let ((current-dir default-directory)
        (xcodeprojs nil))
    (while (and (null xcodeprojs)
                (not (or (equal current-dir "")
                         (equal current-dir "~"))))
      (setq xcodeprojs (directory-files current-dir t "\\.xcodeproj$"))
      (setq current-dir (jda-get-super-directory current-dir)))
    xcodeprojs))

(defun jda-xcode-get-xcodeproj-info-plist (xcodeproj)
  (let ((xcodeproj-info-plist '(targets nil configurations nil schmes))
        (pname nil)
        (targets nil)
        (configurations nil)
        (schemes nil)
        item)
    (dolist (line
             (split-string (with-temp-buffer
                             (shell-command (format "%s -project %s -list"
                                                    jda-xcodebuild-command
                                                    xcodeproj)
                                            t)
                             (buffer-string))
                           "\n" nil))
      (setq item (chomp line))
      (cond ((equal "Targets:" item)
             (setq pname 'targets))
            ((equal "Build Configurations:" item)
             (setq pname 'configurations))
            ((equal "Schemes:" item)
             (setq pname 'schemes))
            ((and (not (null pname))
                  (> (length item) 1))
             (add-to-list pname item t))
            (t
             (setq pname nil))))
    (setq xcodeproj-info-plist (plist-put xcodeproj-info-plist
                                          'targets
                                          targets))
    (setq xcodeproj-info-plist (plist-put xcodeproj-info-plist
                                          'configurations
                                          configurations))
    (setq xcodeproj-info-plist (plist-put xcodeproj-info-plist
                                          'schemes
                                          schemes))))

(defun jda-xcode-build ()
  (interactive)
  ;; initialize xcode sdks
  (if (null jda-xcode-available-sdks)
      (jda-xcode-set-available-sdks))
  (let ((sdk (or (car jda-xcode-sdk-history)
                 (car jda-xcode-available-sdks)))
        (xcodeproj "")
        (xcodeproj-info-plist nil)
        (configuration "")
        (target "")
        (scheme ""))
    ;; select sdk
    (setq sdk (completing-read "Xcode SDK: "
                               jda-xcode-available-sdks
                               nil
                               nil
                               sdk
                               'jda-xcode-sdk-history))
    ;; select xcode project
    (let ((xcodeprojs (jda-xcode-get-xcodeprojs)))
      (if (not (null xcodeprojs))
          (setq xcodeproj (completing-read "Xcode Project: "
                                           xcodeprojs
                                           nil
                                           nil
                                           (car xcodeprojs)
                                           'jda-xcode-project-history))))

    (setq xcodeproj-info-plist (jda-xcode-get-xcodeproj-info-plist
                                xcodeproj))

    ;; select configuration
    (if (not (null xcodeproj-info-plist))
        (setq configuration (completing-read "Configuration: "
                                             (plist-get xcodeproj-info-plist
                                                        'configurations)
                                             nil
                                             nil
                                             (car (plist-get xcodeproj-info-plist
                                                             'configurations))
                                             'jda-xcode-configuration-history)))
    ;; select target
    (if (not (null xcodeproj-info-plist))
        (setq target (completing-read "Target: "
                                      (plist-get xcodeproj-info-plist
                                                 'targets)
                                      nil
                                      nil
                                      (car (plist-get xcodeproj-info-plist
                                                      'targets))
                                      'jda-xcode-target-history)))
    ;; select scheme
    (if (and (not (null xcodeproj-info-plist))
             (= (length target) 0))
        (setq scheme (completing-read "Scheme: "
                                      (plist-get xcodeproj-info-plist
                                                 'schemes)
                                      nil
                                      nil
                                      (car (plist-get xcodeproj-info-plist
                                                      'schemes))
                                      'jda-xcode-scheme-history)))
    ;; build
    (compile (jda-read-shell-command "Compile command: "
                                     (format "%s -sdk %s -project %s -configuration %s %s"
                                             jda-xcodebuild-command
                                             sdk
                                             xcodeproj
                                             configuration
                                             (if (> (length target) 1)
                                                 (format "-target %s" target)
                                               (format "-scheme %s" scheme))
                                             'jda-xcode-build-history)))))

;;;; make

(defun jda-get-makefile-dir ()
  "return the directory(project root directory) where Makefile exist."
  (interactive)
  (let (makefile-dir)
    (setq makefile-dir (buffer-file-name))
    (if (equal makefile-dir nil)
        (setq makefile-dir default-directory) ; not filebuffer
      (setq makefile-dir (jda-get-super-directory makefile-dir)))
    (catch 'while-exit
      (while (not (or (equal makefile-dir "")
                      (equal makefile-dir "~")))
        (cond ((file-exists-p (concat makefile-dir "/Makefile"))
               (throw 'while-exit makefile-dir)))
        (setq makefile-dir (jda-get-super-directory makefile-dir)))
      (throw 'while-exit ""))))

(defun jda-make ()
  "make compile-string like following if the directory in which Makefile exist is found.
ex) make -C project/root/directory"
  (interactive)
  (let (compile-string makefile-dir)
    (setq makefile-dir (jda-get-makefile-dir))
    (if (equal makefile-dir "")
        (setq compile-string "make -C ")
      (setq compile-string (format "make -C %s " makefile-dir)))
    (compile (jda-read-shell-command "Compile command: "
                                     compile-string
                                     'jda-make-command-history))))

(defun jda-build ()
  "Build a project after finding Xcode project(.xcodeproj) or Makefile"
  (interactive)
  (cond ((equal mode-name "ObjC/l")
         (jda-xcode-build))
        (t
         (jda-make))))

;;;; android-doc

(defun* jda-android-doc-source-candidates (&key class-name sdk-dir)
  (split-string
   (shell-command-to-string
    (format "find %s/docs/reference -name '*%s*'"
            sdk-dir
            class-name))))

(defun jda-android-doc-source ()
  `((name . ,jda-android-sdk-dir)
    (candidates . (lambda ()
                    (jda-android-doc-source-candidates
                     :class-name anything-pattern
                     :sdk-dir jda-android-sdk-dir)))
    (volatile)
    (delayed)
    (requires-pattern . 2)
    (action . browse-url)))

(defun jda-android-doc ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (null symbol)
        (setq symbol "")
      (setq symbol (symbol-name symbol)))
    (anything (list (jda-android-doc-source)) symbol)))

(defun jda-doc ()
  (interactive)
  (unless (featurep 'anything)
    (require 'anything))
  (cond ((equal mode-name "ObjC/l")
         (if (featurep 'docsetutil)
             (docsetutil-search-api)
           (jda-xcode-doc)))
        ((or (equal mode-name "JDE")
             (equal mode-name "Java/l"))
         (jda-android-doc))))

;;;; objc

(defun jda-insert-objc-parenthesis ()
  (interactive)
  (let ((depth 0)
        (is-exist-left-bracket nil)
        (is-exist-right-bracket nil)
        (current-point (point))
        (beginning-of-defun-point nil)
        (should-insert-brakets nil))    ; (left right)

    (c-beginning-of-defun)
    (setq beginning-of-defun-point (point))
    (goto-char current-point)

    (setq is-exist-right-bracket (looking-back "\\][ \t\n]*"))

    ;; search parentheses and move the position
    (setq should-insert-brakets
          (catch 'while-exit
            (while t
              (backward-char 1)
              (cond ((>= beginning-of-defun-point
                         (point))
                     (throw 'while-exit '(nil nil)))
                    ((looking-at "\\[")
                     (setq depth (1+ depth))
                     (setq is-exist-left-bracket t))
                    ((looking-at "\\]")
                     (setq depth (1- depth))))

              (cond ((and (equal depth 0)
                          (or (looking-back "[={};&|][ \t\n]*")
                              (looking-back "\\(if\\|switch\\|while\\)[ \t\n]*(")))
                     (if (and is-exist-left-bracket
                              is-exist-right-bracket)
                         (throw 'while-exit '(nil nil))
                       (throw 'while-exit '(t t))))
                    ((equal depth 1)
                     (throw 'while-exit '(nil t)))))))

    ;; insert "["
    (cond ((and (equal depth 0)
                (< beginning-of-defun-point
                   (point))
                (not (looking-back "\\[[ \t\n]*"))
                (car should-insert-brakets))
           (insert "[")
           (setq current-point (1+ current-point))))

    (goto-char current-point)
    (if (nth 1 should-insert-brakets)
        (insert "]"))))

(defun jda-delete-objc-parenthesis ()
  (interactive)
  (cond ((looking-back "\\][ \t\n]*")
         (let ((left-bracket-pos nil)
               (right-bracket-pos nil))
           (backward-list)
           (setq left-bracket-pos (point))
           (forward-list)
           (setq right-bracket-pos (point))

           (goto-char left-bracket-pos)
           (delete-char 1)

           (goto-char right-bracket-pos)
           (backward-char 2)
           (delete-char 1)))))

(defun jda-objc-keymap ()
  (define-key objc-mode-map (kbd "C-c [") 'jda-delete-objc-parenthesis)
  (define-key objc-mode-map (kbd "C-c ]") 'jda-insert-objc-parenthesis))

;;;; rails

(defun jda-rinari-web-server-debug ()
  (interactive)
  (let ((default-directory (rinari-root)))
    (if (null default-directory)
        (message (format "%s is not a part of Rails project"
                         (buffer-name)))
      (rdebug (jda-read-shell-command "Run rdebug(like this): "
                                      (format "rdebug --emacs 3 %sscript/server"
                                              default-directory))))))

(defun jda-rinari-keymap ()
  (define-key rinari-minor-mode-map (kbd "C-c ; W") 'jda-rinari-web-server-debug)
  (define-key rinari-minor-mode-map (kbd "C-c ' W") 'jda-rinari-web-server-debug))

;;;; jda-minor mode functions

(defun jda-minor-keymap ()
  (let ((map (make-sparse-keymap)))
    (easy-menu-define jda-minor-mode-menu
      map
      "Menu used when jda-minor-mode is ative."
      '("JDA"
        ["Build..." jda-build
         :help "Run make or xcodebuild command"]
        ["Find Doc.." jda-doc
         :help "Find documentation for a symbol"]
        ["Open Counterpart File" jda-open-counterpart-file
         :help "Open a counterpart file(.h .c .cpp .m .mm)"]
        "----"
        ["Set Project Root Directory..." jda-gf-set-project-root-with-default-directory
         :help "Set a project root directory"]
        ["Set Exclusive Path..." jda-gf-set-exclusive-path
         :help "Set exclusive paths in find command"]
        ["Find File in Project..." jda-gf-find-file
         :help "Find a file in the proejct"]
        ["Find File in Project (Incremental Search)..." jda-ido-find-file
         :help "Find a file in the proejct using incremental search"]
        ["Find Symbol in Project..." jda-gf-symbol-at-point
         :help "Find a symbol in the project"]
        ["Find Text in Project..." jda-gf-text-at-point
         :help "Find a text in the project"]
        ["Find Text in Git Repository..." jda-git-grep
         :help "Find a text in Git repository"]
        ["Goto Symbol in Current Buffer..." jda-goto-symbol
         :help "Goto a symbol in the current buffer"]
        ["Query Replace in Proejct..." jda-gf-grep-query-replace
         :help "Query replace in the project using *grep* buffer"]
        ["Query Replace in Project (Built-In)..." jda-gf-find-query-replace
         :help "Query replace in the proejct using built-in query replace"]
        "----"
        ["Create TAGS..." jda-create-tags
         :help "Create TAGS file"]
        ["Create TAGS in default-directory..." jda-create-tags-in-default-directory
         :help "Create TAGS file in default-directory"]
        ["Visit TAGS..." visit-tags-table
         :help "Visit a TAGS table file"]
        ["Display All Tags Regexp Matches..." tags-apropos
         :help "Display list of all tags in tags table REGEXP magtches"]
        "----"
        ["Goto Previous Marker" jda-marker-prev-ui
         :help "Goto the previous marker"]
        ["Goto Next Marker" jda-marker-next-ui
         :help "Goto the next marker"]
        ["Finish Jumping Saved Markers" jda-marker-finish-jump
         :help "Finish jumping saved markers and Goto the last marker"]
        ["Push Current Marker" jda-marker-push-marker-menu
         :help "Push the current marker"]
        "---"
        ["Save Current Marker to Mini Bookmark" jda-marker-bookmark-save
         :help "Save current marker to Mini Bookmark"]
        ["Jump the Marker in Mini Bookmark" jda-marker-bookmark-restore
         :help "Jump the marker in Mini Bookmark"]
        "---"
        ("Objective-C"
         ["Insert ']'" jda-insert-objc-parenthesis
          :help "Insert ']' bracket parenthetically"
          :active (equal mode-name "ObjC/l")]
         ["Delete '['" jda-delete-objc-parenthesis
          :help "Delete '[' bracket parenthetically"
          :active (equal mode-name "ObjC/l")])
        ("Rails"
         ["Debug" jda-rinari-web-server-debug
          :help "Debug the current Rails project"
          :active (assoc 'rinari-minor-mode minor-mode-alist)])
        ["Align" align
         :help "Align a region"]
        ["Align Regexp..." align-regexp
         :help "Align the current region using an ad-hoc rule"]
        ["whitespace-mode" whitespace-mode
         :help "Toggle whitespace visualization (Whitespace mode)."]
        ["hs-minor-mode" hs-minor-mode
         :help "hs-minor-mode on/off"]
        ["Highlight Symbol" jda-highlight-symbol-run-toggle
         :help "Highlight the symbol at current point with a idle timer"
         :style toggle
         :selected (timerp jda-highlight-symbol-timer)]
        ["jda-kill-ring-save" jda-kill-ring-save-toggle
         :help "Improved kill-ring-save"
         :style toggle
         :selected jda-kill-ring-save-is-set]
        "----"
        ["Customize JDA" jda-customize
         :help "Customize jda-minor-mode"]
        ["About JDA" jda-about
         :help "Display brief information about JDA"]))

    ;; key map
    (define-key map (kbd "C-c c")       'jda-build)
    (define-key map (kbd "C-c j d")     'jda-doc)
    (define-key map (kbd "C-c j p")     'jda-open-counterpart-file)
    (define-key map (kbd "C-c j r")     'jda-gf-set-project-root-with-default-directory)
    (define-key map (kbd "C-c j e")     'jda-etags-goto-tag-in-file)
    (define-key map (kbd "C-c j s")     'jda-gf-symbol-at-point)
    (define-key map (kbd "C-c j S")     'jda-gf-text-at-point)
    (define-key map (kbd "C-c j f")     'jda-gf-find-file)
    (define-key map (kbd "C-c j F")     'jda-gf-find-file-dired)
    (define-key map (kbd "C-c j g")     'jda-git-grep)
    (define-key map (kbd "C-c j i")     'jda-ido-find-file)
    (define-key map (kbd "C-c j I")     'jda-ido-find-file-reset-root)
    (define-key map (kbd "C-c i")       'jda-ido-find-file)
    (define-key map (kbd "C-c I")       'jda-ido-find-file-reset-root)
    (define-key map (kbd "C-c j m")     'jda-goto-symbol)
    (define-key map (kbd "C-c j o")     'jda-pop-to-compilation-buffer)
    (define-key map (kbd "C-c j O")     'jda-pop-to-compilation-buffer-fill)
    (define-key map (kbd "C-c m")       'jda-goto-symbol)
    (define-key map (kbd "C-c j 5")     'jda-gf-grep-query-replace)
    (define-key map (kbd "C-c j $")     'delete-trailing-whitespace)
    (define-key map (kbd "C-c j %")     'jda-gf-find-query-replace)
    ;; tag
    (define-key map (kbd "C-c j T")     'jda-create-tags-in-default-directory)
    (define-key map (kbd "C-c j t")     'jda-create-tags)
    (define-key map (kbd "C-c j v")     'visit-tags-table)
    (define-key map (kbd "C-c j .")     'tags-apropos)
    ;; marker
    (define-key map (kbd "C-x ,")       'jda-marker-prev-ui)
    (define-key map (kbd "C-x .")       'jda-marker-next-ui)
    (define-key map (kbd "C-x /")       'jda-marker-finish-jump)
    (define-key map (kbd "C-x ?")       'jda-marker-push-marker-menu)
    ;; bookmark
    (define-key map (kbd "C-c p")       'jda-marker-bookmark-restore)
    (define-key map (kbd "C-c P")       'jda-marker-bookmark-save)
    ;; align
    (define-key map (kbd "C-c |")       'align)
    (define-key map (kbd "C-c M-|")     'align-regexp)
    ;; etc
    (define-key map (kbd "C-c j w")     'whitespace-mode)
    (define-key map (kbd "C-c j [")     'hs-minor-mode)
    (define-key map (kbd "C-c j h")     'jda-highlight-symbol-run-toggle)
    (define-key map (kbd "C-c j k")     'jda-kill-ring-save-toggle)
    (define-key map (kbd "C-c j M-?")   'jda-buffer-file-name)
    map))

(defvar jda-minor-mode-map (jda-minor-keymap))

;;;###autoload
(define-minor-mode jda-minor-mode
  "Toggle Jong-Gyu Development Assistant mode.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix arugment turns off the mode.

Key bindings:
\\{jda-minor-mode-map}."
  ;; init-value
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " jda"
  ;; The minor mode bindings.
  :keymap jda-minor-mode-map
  :group 'jda
  :global t
  (cond (jda-minor-mode
         ;; initialize
         (ido-mode t)
         (if (boundp 'ffap-bindings)
             (ffap-bindings))
         (if (eq system-type 'windows-nt)
             (setq jda-find-command "find"))
         (add-hook 'emulation-mode-map-alists 'yas/direct-keymaps)
         (add-hook 'objc-mode-hook 'jda-objc-keymap)
         (add-hook 'rinari-minor-mode-hook 'jda-rinari-keymap)
         (add-hook 'isearch-mode-hook 'jda-marker-push-marker)
         (add-hook 'isearch-mode-end-hook 'jda-marker-push-marker)
         (add-hook 'dired-mode-hook 'jda-dired-mode-keymap)
         (define-key global-map (kbd "M-w") 'jda-kill-ring-save)
         (message "jda minor mode enabled"))
        (t
         ;; finalize
         (remove-hook 'objc-mode-hook 'jda-objc-keymap)
         (remove-hook 'rinari-minor-mode-hook 'jda-rinari-keymap)
         (remove-hook 'isearch-mode-hook 'jda-marker-push-marker)
         (remove-hook 'isearch-mode-end-hook 'jda-marker-push-marker)
         (remove-hook 'emulation-mode-map-alists 'yas/direct-keymaps)
         (remove-hook 'dired-mode-hook 'jda-dired-mode-keymap)
         (define-key global-map (kbd "M-w") 'kill-ring-save)
         (message "jda minor mode disabled"))))

;;;###autoload
(defun jda-customize ()
  "Customize jda"
  (interactive)
  (customize-group 'jda))

;;;###autoload
(defun jda-about ()
  "About JDA"
  (interactive)
  (message "jda-minor-mode(version 0.1.0) Jong-Gyu <jglee1027@gmail.com>"))

(provide 'jda)

;;; jda.el ends here
