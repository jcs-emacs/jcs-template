;;; jcs-template.el --- Template module for jcs-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Shen, Jen-Chieh

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

(defcustom jcs-template-path
  "__header"
  "Path to header relative to file-header."
  :type 'string
  :group 'jcs-template)

(defcustom jcs-template-headers
  '("d-colon"
    "d-dash"
    "d-double-quote"
    "d-semicolon"
    "d-slash"
    "d-percent"
    "t-slash"
    "c-style"
    "sharp"
    "semicolon"
    "d-single-quote"
    "tag"
    "percent")
  "List of template headers."
  :type 'list
  :group 'jcs-template)

;;
;; (@* "Externals" )
;;

(declare-function jcs-current-file-empty-p "ext:jcs-util.el")
(declare-function jcs-member "ext:jcs-util.el")

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
               (jcs-member (f-filename buffer-file-name) reg-lst 'regex))
      (setq result (jcs-insert-header-if-empty insert-func interactive)))
    (if result
        (when (functionp success) (funcall success))
      (when (functionp fail) (funcall fail)))
    result))

;;
;; (@* "Core" )
;;

(defvar jcs-template--headers-loaded-p nil
  "Return non-nil, if headers are loaded as cache.")

;;;###autoload
(defun jcs-template-reload (&optional force)
  "Reload the header templates once.

If optional argument FORCE is non-nil, refresh cache once."
  (interactive)
  (when (or force (null jcs-template--headers-loaded-p))
    (dolist (header jcs-template-headers)
      (set (intern (concat "jcs-template--header-" header))
           (file-header-template-string (format "%s/%s.txt" jcs-template-path header))))
    (setq jcs-template--headers-loaded-p t)))

;;
;; (@* "Header" )
;;

;;;###autoload
(defmacro jcs-template-define-header (name)
  "Define template header by NAME."
  (let ((var  (intern (concat "jcs-template--header-" name)))
        (func (intern (concat "jcs-template-header-" name))))
    `(progn
       (defvar ,var nil
         ,(format "Preload the %s header template." name))
       (defun ,func nil
         ,(format "Return the preload %s header template." name)
         (file-header-swap-keyword-template ,var)))))

(dolist (header jcs-template-headers)
  (eval `(jcs-template-define-header ,header)))

(provide 'jcs-template)
;;; jcs-template.el ends here
