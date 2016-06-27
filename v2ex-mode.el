;;; v2ex-mode.el --- Major mode for visit http://v2ex.com/ site.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Aborn Jiang

;; Author: Aborn Jiang <aborn.jiang@gmail.com>
;; Version: 0.1
;; Package-Requires: ((cl-lib "0.5") (request "0.2") (let-alist "1.0.3"))
;; Keywords: v2ex, v2ex.com
;; Homepage: https://github.com/aborn/v2ex-mode
;; URL: https://github.com/aborn/v2ex-mode

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; v2ex-mode code can be found here:
;;   http://github.com/aborn/v2ex-mode

;;; Commentary:

;; Visiting ve2x.com freely.
;; M-x v2ex
;; M-x v2ex-hot
;; M-x v2ex-latest

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'tabulated-list)
(require 'let-alist)

(defgroup v2ex-mode nil
  "Major mode for visiting v2ex.com site."
  :prefix "v2ex-"
  :group 'external)

(defvar v2ex-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "q" 'quit-window)
    (define-key map "r" 'v2ex)
    (define-key map "H" 'v2ex-hot)
    (define-key map "L" 'v2ex-latest)
    ;; vim-like hjkl for cursor move swiftly
    (define-key map "h" 'backward-char)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "l" 'forward-char)
    ;; n/p is shorter than C-n/p
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map)
  "Major mode for visit http://v2ex.com/.")

(defcustom v2ex-buffer-name "*v2ex*"
  "The buffer name of content display."
  :group 'v2ex-mode
  :type 'string)

(defcustom v2ex-hot-api-uri "https://www.v2ex.com/api/topics/hot.json"
  "The hot topic api."
  :group 'v2ex-mode
  :type 'string)

(defcustom v2ex-latest-api-uri "https://www.v2ex.com/api/topics/latest.json"
  "The url of latest topics api."
  :group 'v2ex-mode
  :type 'string)

(defvar v2ex-current-visit
  '(:name "latest" :url v2ex-latest-api-uri :desc "最新主题")
  "The current visit.")

(defcustom v2ex-request-timeout 10
  "Timeout control when connecting v2ex, in seconds."
  :group 'v2ex-mode
  :type 'number)

(defvar v2ex--json nil "JSON object in Emacs Lisp.")

(defun v2ex--render (json-content response)
  (setq v2ex--json json-content)
  (with-current-buffer (get-buffer-create v2ex-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (v2ex-mode)))
  (unless (get-buffer-window v2ex-buffer-name)
    (if (one-window-p)
        (switch-to-buffer v2ex-buffer-name)
      (switch-to-buffer-other-window v2ex-buffer-name))))

(define-button-type 'v2ex-button
  'action (lambda (b) (browse-url (button-get b 'link)))
  'follow-link t)

;; TODO: Need to indicate (plist-get v2ex-current-visit :desc) somewhere
(define-derived-mode v2ex-mode tabulated-list-mode "V2EX"
  "Major mode for browsing http://v2ex.com/.
Letters do not insert themselves; instead, they are commands.
\\<v2ex-mode-map>
\\{v2ex-mode-map}"
  (setq tabulated-list-format [("Member"  12 nil)
                               ("Node"    11 nil)
                               ("Created" 10 nil)
                               ("Replies" 8  nil)
                               ("Title"   0  nil)])
  (tabulated-list-init-header)
  (setq tabulated-list-entries
        (cl-loop for elt in (append v2ex--json nil)
                 collect
                 (let-alist elt
                   (list .id (vector .member.username
                                     .node.title
                                     (format-time-string
                                      "%a %H:%M"
                                      .created)
                                     (number-to-string .replies)
                                     (cons .title (list :type 'v2ex-button
                                                        'link .url)))))))
  (tabulated-list-print))

;;;###autoload
(defun v2ex (&optional async)
  "Open v2ex mode."
  (interactive "P")
  (message "open v2ex.com using %s way." (if async
                                             "async"
                                           "sync"))
  (request (symbol-value (plist-get v2ex-current-visit :url))
           :parser (lambda ()
                     (json-read-from-string (decode-coding-string (buffer-string) 'utf-8)))
           :sync (not async)
           :success (cl-function
                     (lambda (&key data &key response &allow-other-keys)
                       (message "Request http://v2ex.com/ success!")
                       (v2ex--render data response)))
           :complete (lambda (&rest _) (message "Request finished and *v2ex* updated!"))
           :error (cl-function
                   (lambda (&rest args &key error-thrown &allow-other-keys)
                     (error "Got errror: %S in request %s!Please retry!" error-thrown (plist-get v2ex-current-visit :url))))
           :timeout v2ex-request-timeout))

;;;###autoload
(defun v2ex-latest (&optional async)
  "Open v2ex latest topics."
  (interactive "P")
  (setq v2ex-current-visit
        '(:name "latest" :url v2ex-latest-api-uri :desc "最新主题"))
  (v2ex async))

;;;###autoload
(defun v2ex-hot (&optional async)
  "Open v2ex hot topics."
  (interactive "P")
  (setq v2ex-current-visit
        '(:name "hot" :url v2ex-hot-api-uri :desc "最热主题"))
  (v2ex async))

(provide 'v2ex-mode)
;;; v2ex-mode.el ends here
