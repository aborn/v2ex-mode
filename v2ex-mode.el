;;; v2ex-mode.el -- visiting v2ex.com site in emacs

;; Copyright (C) 2016 Aborn Jiang

;; Author: Aborn Jiang <aborn.jiang@gmail.com>
;; Version: 1.0
;; Keywords: v2ex, v2ex.com
;; Homepage: https://github.com/aborn/v2ex-mode

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

;;; Commentary:

;; visiting ve2x.com freely in emacs.
;; M-x v2ex

(require 'cl-lib)
(require 'json)

(defvar v2ex-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "r" 'v2ex)
    map)
  "major mode for visiting v2ex.com")

(defvar v2ex-entry-format "%N. %[%T%] (%U replies)\n")

(define-derived-mode v2ex-mode nil "v2ex-mode"
  "Major mode for visit http://v2ex.com/"
  (widen)
  :group 'v2ex-mode)

(defcustom v2ex/buffer-name "*v2ex*"
  "the content display buffer name"
  :group 'v2ex-mode
  :type 'string)

(defun v2ex/read-http-data-as-json (http-data)
  (with-temp-buffer
    (insert http-data)
    (goto-char (point-min))
    (re-search-forward "^$")
    (json-read-from-string (buffer-substring (point) (point-max)))))

(defun v2ex/do-ajax-action (url &optional type)
  "get build status info"
  (let* ((buffer (url-retrieve-synchronously url))
         (http-content nil)
         (json-data nil)
         (result-data nil))
    (if (not buffer)
        (error "请求%服务失败，请重试！" v2ex/build-status-url))
    (with-current-buffer buffer
      (unless (= 200 (url-http-parse-response)))
      (setq http-content (decode-coding-string (buffer-string) 'utf-8))
      ;; (message http-content)
      (if (string= type "html")
          (setq result-data http-content)
        (progn (setq json-data (v2ex/read-http-data-as-json http-content))
               ;; (princ json-data)
               (setq result-data json-data))))
    result-data))

(defun v2ex ()
  "open v2ex mode"
  (interactive)
  (message "open v2ex.com")
  (let* ((v2ex-buffer (get-buffer-create v2ex/buffer-name))
         (json-content nil)
         (num 0))
    (with-current-buffer v2ex-buffer
      (v2ex-mode)
      (erase-buffer)
      (setq json-content (v2ex/do-ajax-action "https://www.v2ex.com/api/topics/latest.json"))
      (while (< num (length json-content))
        (let* ((item (aref json-content num))
               (url (assoc-default 'url item))
               (replies (assoc-default 'replies item)))
          ;;(insert (assoc-default 'title item))
          (widget-create (v2ex/make-entry item num))
          )
        (setq num (1+ num))
        )
      (widget-setup)
      (goto-char (point-min))
      ;;(princ json-content)
      ;;(insert (json-encode json-content))
      ;; (dolist (item json-content)
      ;;   (insert (plist-get item 'title))
      ;;   )
      ))
  (message "done v2ex"))

(define-widget 'v2ex-entry 'url-link
  "A widget representing a v2ex entry."
  :format-handler 'v2ex-entry-format)

(defun v2ex/make-entry (data n)
  (let ()
    (v2ex/alet (title url replies id)
        data
      (list 'v2ex-entry
            :format v2ex-entry-format
            :value url
            :help-echo url
            :tab-order n
            :v2ex-n n
            :v2ex-title title
            :v2ex-replies replies))))

(defun v2ex-entry-format (widget char)
  (case char
    (?N (insert (format "%3d" (1+ (widget-get widget :v2ex-n)))))
    (?T (insert (truncate-string-to-width (widget-get widget :v2ex-title) 80 nil nil t)))
    (?U (insert (format "%d" (widget-get widget :v2ex-replies))))
    (t (widget-default-format-handler widget char))))

(defmacro v2ex/alet (vars alist &rest forms)
  (let ((alist-var (make-symbol "alist")))
    `(let* ((,alist-var ,alist)
            ,@(loop for var in vars
                    collecting `(,var (assoc-default ',var ,alist-var))))
       ,@forms)))

(provide 'v2ex-mode)
