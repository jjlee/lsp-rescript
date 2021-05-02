;;; lsp-rescript.el --- LSP client configuration for lsp-mode and rescript-vscode -*-lexical-binding: t-*-
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
;; Copyright (C) 2021 John Lee <jjl@pobox.com>

;; Version: 0.1.0
;; Author: John Lee
;; Url: https://github.com/jjlee/lsp-rescript
;; Keywords: languages
;; Package-Requires: ((lsp-mode "3.0") (emacs "24.3"))

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This project provides lsp-mode client configuration for rescript-vscode for
;; the ReScript programming language

;; Exported names start with "lsp-rescript-"; private names start with
;; "lsp-rescript--".

;;; Code:

(require 'lsp-mode)

(defgroup lsp-rescript nil
  "lsp-mode client configuration for rescript-vscode LSP server for ReScript code."
  :link '(url-link "https://rescript-lang.org/")
  :group 'lsp-mode)

(defcustom lsp-rescript-server-command '()
  "Full command to run the ReScript language server.

Should be something like '(\"node\" \"/path/to/rescript-vscode/server/out/server.js\" \"--stdio\")"
  :group 'lsp-rescript
  :risky t
  :type '(repeat string))

(defcustom lsp-rescript-prompt-for-build t
  "If nil, suppress the prompt to start a build."
  :group 'lsp-rescript
  :risky t
  :type 'boolean)

(defun lsp-rescript--hash-table-symbol-value-items (table)
  "Return an alist suitable for sending as an LSP response.
Argument TABLE hash table."
  (let (results)
    (maphash
     (lambda (key value)
       (setf results (append results (list (intern (concat ":" key)) value))))
     table)
    results))

(defun lsp-rescript--propertize (str type)
  "Propertize STR as per TYPE."
  (propertize str 'face (alist-get type lsp--message-type-face)))

(lsp-defun lsp-rescript--window-log-message-request ((&ShowMessageRequestParams :message :type :actions?))
  "Display a message request to the user and send the user's selection back to the server."
  ;; rescript-vscode arranges via an LSP request to give you an interactive
  ;; prompt about whether you want to start a build.  This differs from the
  ;; upstream lsp-mode implementation in also sending back any additional
  ;; parameters sent with the request.  In particular, the rescript-vscode LSP
  ;; server wants to see projectRootPath when it processes the response from the
  ;; client.  This sends back all of the parameters instead of only `title' as
  ;; vanilla lsp-mode.el does.
  (let* ((message (lsp-rescript--propertize message type))
          (choices (--map (gethash "title" it) actions?)))
    (if choices
        (let* ((selected (completing-read (concat message " ") choices nil t))
                (ht (car (--filter (equal (gethash "title" it) selected) (append actions? nil))))
                (response (lsp-rescript--hash-table-symbol-value-items ht)))
          response)
      (lsp-log message))))

(defun lsp-rescript--handle-show-message-request (_workspace request)
  "Handle a show message REQUEST in lsp WORKSPACE."
  (if lsp-rescript-prompt-for-build
      (lsp-rescript--window-log-message-request request)))

(add-to-list 'lsp-language-id-configuration '(rescript-mode . "rescript"))
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-rescript-server-command))
  :major-modes '(rescript-mode)
  :notification-handlers (ht ("client/registerCapability" #'ignore))
  :request-handlers (ht("window/showMessageRequest" #'lsp-rescript--handle-show-message-request))
  :priority 1
  :language-id "rescript"
  :server-id 'rescript-vscode))

(provide 'lsp-rescript)
;;; lsp-rescript.el ends here
