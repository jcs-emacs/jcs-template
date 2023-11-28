;;; jcs-template.el --- Template module for jcs-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-emacs/jcs-template
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (file-header "0.1.0") (f "0.20.0"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Template module for jcs-emacs.
;;

;;; Code:

(require 'file-header)
(require 'f)

(defgroup jcs-template nil
  "Template module for jcs-emacs."
  :prefix "jcs-template-"
  :group 'faces
  :link '(url-link :tag "Github" "https://github.com/jcs-emacs/jcs-etemplate"))

;;
;; (@* "Insertion" )
;;

;;;###autoload
(defun jcs-insert-header-if-empty (insert-func &optional ci)
  "Execute INSERT-FUNC if empty, CI means `call-interactively'."
  (when (jcs-current-file-empty-p)
    (if ci (call-interactively insert-func) (funcall insert-func))
    (goto-char (point-min))))

;;;###autoload
(cl-defun jcs-insert-header-if-valid (reg-lst insert-func &key interactive success fail)
  "Insert the header if certain conditions met.

REG-LST is extension list represent by regular expression.
INSERT-FUNC is the function that will be use to call inserting header content.
INTERACTIVE is boolean check if called function interactively instead.
SUCCESS is callback after successfully inserted header content.
FAILED is callback if does NOT successfully inserted header content."
  (jcs-template-reload)
  (let (result)
    (when (and buffer-file-name
               (not (file-exists-p buffer-file-name))
               (jcs-contain-list-type-str (f-filename buffer-file-name) reg-lst 'regex))
      (setq result (jcs-insert-header-if-empty insert-func interactive)))
    (if result
        (when (functionp success) (funcall success))
      (when (functionp fail) (funcall fail)))
    result))

;;
;; (@* "Buffer String" )
;;

(defvar jcs-template--header-double-colon nil
  "Preload the double colon file info template.")

(defvar jcs-template--header-double-dash nil
  "Preload the double dash file info template.")

(defvar jcs-template--header-double-quote nil
  "Preload the double quote file info template.")

(defvar jcs-template--header-double-semicolon nil
  "Preload the double semicolon file info template.")

(defvar jcs-template--header-double-slash nil
  "Preload the double slash file info template.")

(defvar jcs-template--header-double-percent nil
  "Preload the double percent file info template.")

(defvar jcs-template--header-triple-slash nil
  "Preload the triple slash file info template.")

(defvar jcs-template--header-c-style nil
  "Preload the global file info template.")

(defvar jcs-template--header-sharp nil
  "Preload the sharp file info template.")

(defvar jcs-template--header-semicolon nil
  "Preload the semicolon file info template.")

(defvar jcs-template--header-single-quote nil
  "Preload the single quote file info template.")

(defvar jcs-template--header-tag nil
  "Preload the tag file info template.")

(defvar jcs-template--header-percent nil
  "Preload the percent file info template.")

(defvar jcs-template--headers-loaded-p nil
  "Return non-nil, if headers are loaded as cache.")

;;;###autoload
(defun jcs-template-reload (&optional force)
  "Reload the header templates once.

If optional argument FORCE is non-nil, refresh cache once."
  (interactive)
  (when (or force (null jcs-template--headers-loaded-p))
    (setq jcs-template--header-double-colon (file-header-template-string "__header/d_colon.txt")
          jcs-template--header-double-dash (file-header-template-string "__header/d_dash.txt")
          jcs-template--header-double-quote (file-header-template-string "__header/d_quote.txt")
          jcs-template--header-double-semicolon (file-header-template-string "__header/d_semicolon.txt")
          jcs-template--header-double-slash (file-header-template-string "__header/d_slash.txt")
          jcs-template--header-double-percent (file-header-template-string "__header/d_percent.txt")
          jcs-template--header-triple-slash (file-header-template-string "__header/t_slash.txt")
          jcs-template--header-c-style (file-header-template-string "__header/c_style.txt")
          jcs-template--header-semicolon (file-header-template-string "__header/semicolon.txt")
          jcs-template--header-sharp (file-header-template-string "__header/sharp.txt")
          jcs-template--header-single-quote (file-header-template-string "__header/singlequote.txt")
          jcs-template--header-tag (file-header-template-string "__header/tag.txt")
          jcs-template--header-percent (file-header-template-string "__header/percent.txt")
          jcs-template--headers-loaded-p t)))

;;
;; (@* "Header" )
;;

;;;###autoload
(defun jcs-template-header-double-colon ()
  "Return the preloaded double colon file info template."
  (file-header-swap-keyword-template jcs-template--header-double-colon))

;;;###autoload
(defun jcs-template-header-double-dash ()
  "Return the preloaded double dash file info template."
  (file-header-swap-keyword-template jcs-template--header-double-dash))

;;;###autoload
(defun jcs-template-header-double-quote ()
  "Return the preloaded double quote file info template."
  (file-header-swap-keyword-template jcs-template--header-double-quote))

;;;###autoload
(defun jcs-template-header-double-semicolon ()
  "Return the preloaded double semicolon file info template."
  (file-header-swap-keyword-template jcs-template--header-double-semicolon))

;;;###autoload
(defun jcs-template-header-double-slash ()
  "Return the preloaded double slash file info template."
  (file-header-swap-keyword-template jcs-template--header-double-slash))

;;;###autoload
(defun jcs-template-header-triple-slash ()
  "Return the preloaded triple slash file info template."
  (file-header-swap-keyword-template jcs-template--header-triple-slash))

;;;###autoload
(defun jcs-template-header-c-style ()
  "Return the preloaded c-style file info template."
  (file-header-swap-keyword-template jcs-template--header-c-style))

;;
;;; ;

;;;###autoload
(defun jcs-template-header-semicolon ()
  "Return the preloaded semicolon file info template."
  (file-header-swap-keyword-template jcs-template--header-semicolon))

;;
;;; #

;;;###autoload
(defun jcs-template-header-sharp ()
  "Return the preloaded sharp file info template."
  (file-header-swap-keyword-template jcs-template--header-sharp))

;;
;;; '

;;;###autoload
(defun jcs-template-header-single-quote ()
  "Return the preloaded single quote file info template."
  (file-header-swap-keyword-template jcs-template--header-single-quote))

;;
;;; <!-- -->

;;;###autoload
(defun jcs-template-header-tag ()
  "Return the preloaded tag file info template."
  (file-header-swap-keyword-template jcs-template--header-tag))

;;
;;; %

;;;###autoload
(defun jcs-template-header-double-percent ()
  "Return the preloaded double percent file info template."
  (file-header-swap-keyword-template jcs-template--header-double-percent))

;;;###autoload
(defun jcs-template-header-percent ()
  "Return the preloaded percent file info template."
  (file-header-swap-keyword-template jcs-template--header-percent))

(provide 'jcs-template)
;;; jcs-template.el ends here
