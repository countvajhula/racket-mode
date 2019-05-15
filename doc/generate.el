;;; racket-make-doc.el --- Major mode for Racket language.

;; Copyright (c) 2013-2018 by Greg Hendershott.

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

;;; Generate a markdown format file for Reference documentation.

(require 'racket-mode)
(require 'racket-debug)
(require 'racket-profile)
(require 'racket-edit)
(require 'racket-util)
(require 'racket-unicode-input-method)
(require 'cl-lib)
(require 's)

(defun racket-make-doc ()
  (find-file "reference.org")
  (delete-region (point-min) (point-max))
  (insert (racket-make-doc/commands))
  (insert (racket-make-doc/variables))
  (insert (racket-make-doc/faces))
  (save-buffer))

;;; Commands

(defconst racket-make-doc/commands
  '("Run"
    racket-run
    racket-racket
    racket-profile
    racket-profile-mode
    racket-logger
    racket-logger-mode
    racket-debug-mode
    "Test"
    racket-test
    racket-raco-test
    "Eval"
    racket-send-region
    racket-send-definition
    racket-send-last-sexp
    "Visit"
    racket-visit-definition
    racket-visit-module
    racket-unvisit
    racket-open-require-path
    racket-find-collection
    "Learn"
    racket-describe
    racket-doc
    "Edit"
    racket-fold-all-tests
    racket-unfold-all-tests
    racket-tidy-requires
    racket-trim-requires
    racket-base-requires
    racket-indent-line
    racket-smart-open-bracket
    racket-cycle-paren-shapes
    racket-backward-up-list
    racket-check-syntax-mode
    racket-unicode-input-method-enable
    racket-align
    racket-unalign
    racket-complete-at-point
    "Macro expand"
    racket-stepper-mode
    racket-expand-file
    racket-expand-region
    racket-expand-definition
    racket-expand-last-sexp
    "Other"
    racket-mode-start-faster)
  "Commands to include in the Reference.")

(defun racket-make-doc/commands ()
  (apply #'concat
         (cons "* Commands\n\n"
               (mapcar #'racket-make-doc/command racket-make-doc/commands))))

(defun racket-make-doc/command (s)
  (if (stringp s)
      (format "** %s\n\n" s)
    (concat (format "*** %s\n" s)
            (and (interactive-form s)
                 (racket-make-doc/bindings-as-kbd s))
            (racket-make-doc/tweak-quotes
             (racket-make-doc/linkify
              ;; Supply RAW t to leave e.g. \\{racket-profile-mode-map} in string.
              ;; We'll call substitute-command-keys ourselves, and produce something
              ;; nice for org => texi => {info html}
              (or (documentation s t) "No documentation.\n\n")))
            "\n\n")))

(defun racket-make-doc/bindings-as-kbd (symbol)
  (let* ((bindings (racket-make-doc/bindings symbol))
         (strs (and
                bindings
                (cl-remove-if-not
                 #'identity
                 (mapcar (lambda (binding)
                           (unless (eq (aref binding 0) 'menu-bar)
                             (let ((desc (key-description binding)))
                               ;; I don't know how to escape { or }
                               ;; for texi
                               (unless (string-match-p "[{}]" desc)
                                 (format "{{{kbd(%s)}}}" desc)))))
                         bindings))))
         (str (if strs
                  (mapconcat #'identity strs " or ")
                (format "{{{kbd(M-x)}}} ~%s~ {{{kbd(RET)}}}" symbol))))
    (concat str "\n\n")))

(defun racket-make-doc/bindings (symbol)
  (where-is-internal symbol racket-mode-map))

;;; Variables

(defconst racket-make-doc/variables
  '("General"
    racket-program
    racket-command-port
    racket-command-startup
    racket-command-timeout
    racket-memory-limit
    racket-error-context
    racket-retry-as-skeleton
    racket-user-command-line-arguments
    racket-path-from-emacs-to-racket-function
    racket-path-from-racket-to-emacs-function
    "REPL"
    racket-history-filter-regexp
    racket-images-inline
    racket-images-keep-last
    racket-images-system-viewer
    racket-pretty-print
    "Other"
    racket-indent-curly-as-sequence
    racket-indent-sequence-depth
    racket-pretty-lambda
    racket-smart-open-bracket-enable
    racket-logger-config
    "Experimental debugger"
    racket-debuggable-files)
  "Variables to include in the Reference.")

(defun racket-make-doc/variables ()
  (apply #'concat
         (cons "* Variables\n\n"
               (mapcar #'racket-make-doc/variable racket-make-doc/variables))))

(defun racket-make-doc/variable (s)
  (if (stringp s)
      (format "** %s\n\n" s)
    (concat (format "*** %s\n" s)
            (racket-make-doc/tweak-quotes
             (racket-make-doc/linkify
              (or (documentation-property s 'variable-documentation)
                  "No documentation.\n\n")))
            "\n\n")))

;;; Faces

(defconst racket-make-doc/faces
  '(racket-keyword-argument-face
    racket-selfeval-face
    racket-here-string-face
    racket-check-syntax-def-face
    racket-check-syntax-use-face
    racket-logger-config-face
    racket-logger-topic-face
    racket-logger-fatal-face
    racket-logger-error-face
    racket-logger-warning-face
    racket-logger-info-face
    racket-logger-debug-face)
  "Faces to include in the Reference.")

(defun racket-make-doc/faces ()
  (apply #'concat
         (cons "* Faces\n\n"
               (mapcar #'racket-make-doc/face racket-make-doc/faces))))

(defun racket-make-doc/face (symbol)
  (concat (format "** %s\n" symbol)
          (racket-make-doc/tweak-quotes
           (racket-make-doc/linkify
            (or (documentation-property symbol 'face-documentation)
                "No documentation.\n\n")))
          "\n\n"))

;;; Utility

(defun racket-make-doc/linkify (s)
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward (rx ?\`
                                  (group "racket-" (+ (or (syntax word)
                                                          (syntax symbol))))
                                  ?\')
                              nil t)
      (let* ((name (buffer-substring-no-properties (match-beginning 1)
                                                   (match-end 1)))
             (sym (intern-soft name)))
        (when (or (member sym racket-make-doc/commands)
                  (member sym racket-make-doc/variables)
                  (member sym racket-make-doc/faces))
          (replace-match (format "@@texinfo:@ref{%s}@@" name)
                         nil nil))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun racket-make-doc/tweak-quotes (s)
  "Change \` \' style quotes to ~~ style."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward (rx (or (seq ?\`
                                           (group (+ (or (syntax word)
                                                         (syntax symbol))))
                                           (or ?\` ?\'))))
                              nil t)
      (let ((name (buffer-substring-no-properties (match-beginning 1)
                                                  (match-end 1))))
        (replace-match (format "~%s~" name)
                       nil nil)))
    (buffer-substring-no-properties (point-min) (point-max))))

;;; racket-make-doc.el ends here
