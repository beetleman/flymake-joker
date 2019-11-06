;;; flymake-joker.el --- Add Clojure syntax checker (via Joker) to flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Mateusz Probachta <mateusz.probachta@gmail.com>
;;
;; Author: Mateusz Probachta <mateusz.probachta@gmail.com>
;; Created: 12 February 2019
;; Version: 0.0.1
;; Package-Requires: ((flymake-quickdef "0.1.1"))

;;; Commentary:

;; This package adds Clojure syntax checker (via Joker) to flycheck.
;; Make sure Joker binary is on your path.
;; Joker installation instructions are here: https://github.com/candid82/joker#installation

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'flymake-quickdef)

(setf flymake-check-joker-regexp
      "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]\\ ]+\\): \\(.+\\)$")

(defun severity-to-type (severity)
  (cond
   ((string= severity "Read error") :error)

   ((string= severity "Parse warning") :warning)
   ((string= severity "Parse error") :error)

   ((string= severity "Exception") :error)

   (t :note)))

(defmacro defn-joker-backend (name dialect)
  `(flymake-quickdef-backend ,name
     :pre-let ((joker-exec (executable-find "joker")))
     :pre-check (unless joker-exec (error "Cannot find joker executable"))
     :write-type 'pipe
     :proc-form (list joker-exec "--lint" "--dialect" ,dialect "-")
     :search-regexp flymake-check-joker-regexp
     :prep-diagnostic
     (let* ((lnum (string-to-number (match-string 1)))
            (lcol (string-to-number (match-string 2)))
            (severity (match-string 3))
            (msg (match-string 4))
            (pos (flymake-diag-region fmqd-source lnum lcol))
            (beg (car pos))
            (end (cdr pos))
            (type (severity-to-type severity)))
       (list fmqd-source beg end type msg))))

(defn-joker-backend flymake-check-joker-clj "clj")
(defn-joker-backend flymake-check-joker-edn "edn")
(defn-joker-backend flymake-check-joker-cljs "cljs")

(defun setup-flymake-backend-clj ()
  (let ((ext (file-name-extension (buffer-file-name))))
    (when (not (string= "cljs" ext))
      (flymake-mode 1)
      (if (string= "edn" ext)
          (add-hook 'flymake-diagnostic-functions 'flymake-check-joker-edn nil t)
        (add-hook 'flymake-diagnostic-functions 'flymake-check-joker-clj nil t)))))

(defun setup-flymake-backend-cljs ()
  (flymake-mode 1)
  (add-hook 'flymake-diagnostic-functions 'flymake-check-joker-cljs nil t))

(provide 'flymake-joker)

;;; flymake-joker.el ends here
