;;; guildai.el --- Run agents and manage Guild sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Chris Waterson
;; Author: Chris Waterson
;; Assisted-by: Codex:GPT-6
;; URL: https://github.com/waterson/guildai-el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, applications
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; Run `guildai' to select a workspace and start a session.  Use
;; `guildai-sessions' to resume one.  Credentials come from auth-source;
;; use the API host with user "guildai" and password "KEY-ID:SECRET".
;; `guildai-send-region' and `guildai-send-buffer' create editable drafts.
;; See README.org for setup and key scope instructions.

;;; Code:

(require 'auth-source)
(require 'json)
(require 'url)
(require 'url-http)
(require 'tabulated-list)
(require 'subr-x)

(defvar url-http-response-status)

(defgroup guildai nil "Run Guild agents from Emacs." :group 'tools)
(defcustom guildai-api-url "https://app.guild.ai/api"
  "Guild API base URL, including /api." :type 'string)
(defcustom guildai-account nil
  "Account name or ID used to list workspaces." :type '(choice (const nil) string))
(defcustom guildai-workspace nil
  "Default workspace ID or owner~name." :type '(choice (const nil) string))
(defcustom guildai-agent nil
  "Optional agent identifier for new sessions." :type '(choice (const nil) string))
(defcustom guildai-poll-interval 3
  "Seconds between event requests in an open session." :type 'natnum)
(defcustom guildai-credential-function #'guildai--auth-source-secret
  "Function returning the API credential as KEY-ID:SECRET." :type 'function)

(defun guildai--auth-source-secret ()
  "Return the credential for the configured API host."
  (let* ((host (url-host (url-generic-parse-url guildai-api-url)))
         (entry (car (auth-source-search :host host :user "guildai"
                                         :require '(:secret) :max 1)))
         (secret (plist-get entry :secret)))
    (or (if (functionp secret) (funcall secret) secret)
        (user-error "Add an auth-source entry for %s, user guildai" host))))

(defun guildai--segment (value)
  "Encode VALUE as a single URL path segment."
  (url-hexify-string value))

(defun guildai--request (method path body success failure)
  "Request METHOD PATH with BODY; call SUCCESS or FAILURE asynchronously."
  (let ((url-request-method method)
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " (funcall guildai-credential-function)))
           ("Content-Type" . "application/json") ("Accept" . "application/json")))
        (url-request-data (and body (encode-coding-string (json-encode body) 'utf-8)))
        ;; Never forward a credential to another host through a redirect.
        (url-max-redirections 0))
    (url-retrieve
     (concat (string-remove-suffix "/" guildai-api-url) path)
     (lambda (status)
       (let (value problem)
         (unwind-protect
             (condition-case err
                 (if (plist-get status :error)
                     (setq problem
                           (if url-http-response-status
                               (format "Guild HTTP %s; check credentials and scopes"
                                       url-http-response-status)
                             "Guild connection failed; check the API URL and network"))
                   (let ((code url-http-response-status))
                     (goto-char (point-min))
                     (unless (re-search-forward "\r?\n\r?\n" nil t)
                       (error "Missing HTTP response headers"))
                     (if (and code (<= 200 code) (< code 300))
                         (setq value (unless (eobp)
                                       (json-parse-buffer :object-type 'alist
                                                          :array-type 'list
                                                          :false-object nil)))
                       (setq problem (format "Guild HTTP %s; check credentials and scopes" code)))))
               (error (setq problem (format "Guild response error: %s"
                                             (error-message-string err)))))
           (kill-buffer (current-buffer)))
         (if problem (funcall failure problem) (funcall success value))))
     nil t t)))

(defun guildai--failure (problem)
  "Display request failure PROBLEM."
  (message "%s" problem))

(defun guildai--all (path success &optional failure offset collected)
  "Fetch paginated PATH and pass items to SUCCESS.
FAILURE handles errors.  OFFSET and COLLECTED carry pagination state."
  (guildai--request
   "GET" (format "%s%slimit=100&offset=%d" path
                 (if (string-match-p "?" path) "&" "?") (or offset 0)) nil
   (lambda (response)
     (let* ((items (append collected (alist-get 'items response)))
            (pagination (alist-get 'pagination response))
            (limit (alist-get 'limit pagination)))
       (if (alist-get 'has_more pagination)
           (if (and (numberp limit) (> limit 0))
               (guildai--all path success failure (+ (or offset 0) limit) items)
             (funcall (or failure #'guildai--failure) "Guild returned invalid pagination"))
         (funcall success items))))
   (or failure #'guildai--failure)))

(defun guildai--with-workspace (callback)
  "Call CALLBACK with a configured or interactively selected workspace."
  (if guildai-workspace (funcall callback guildai-workspace)
    (let ((account (or guildai-account (read-string "Guild account: "))))
      (guildai--all
       (format "/accounts/%s/workspaces?archived=false" (guildai--segment account))
       (lambda (items)
         (if (null items) (message "No accessible Guild workspaces")
           (let* ((choices (mapcar (lambda (item)
                                    (cons (format "%s [%s]" (alist-get 'name item)
                                                  (alist-get 'id item))
                                          (alist-get 'id item))) items))
                  (selected (completing-read "Workspace: " choices nil t)))
             (funcall callback (cdr (assoc selected choices))))))))))

(defvar-local guildai--session-id nil)
(defvar-local guildai--workspace-id nil)
(defvar-local guildai--timer nil)
(defvar-local guildai--cursor nil)
(defvar-local guildai--busy nil)
(defvar-local guildai--seen nil)
(defvar-local guildai--target nil)
(defvar-local guildai--sending nil)

(defun guildai--stop ()
  "Stop polling in this buffer."
  (when guildai--timer (cancel-timer guildai--timer))
  (setq guildai--timer nil))

(defvar guildai-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'guildai-compose)
    (define-key map (kbd "g") #'guildai-refresh)
    (define-key map (kbd "i") #'guildai-interrupt)
    (define-key map (kbd "b") #'guildai-open-in-browser)
    map))
(define-derived-mode guildai-session-mode special-mode "Guild"
  "Read a Guild session; c composes, g refreshes, i interrupts."
  (setq guildai--seen (make-hash-table :test #'equal))
  (add-hook 'kill-buffer-hook #'guildai--stop nil t)
  (add-hook 'change-major-mode-hook #'guildai--stop nil t))

(defun guildai--text (content)
  "Render event CONTENT as readable text."
  (cond ((stringp content) content)
        ((equal (alist-get 'type content) "multipart/mixed")
         (mapconcat #'guildai--text (alist-get 'parts content) "\n"))
        ((equal (alist-get 'type content) "text")
         (or (alist-get 'data content) (alist-get 'text content) ""))
        (t (json-encode content))))

(defun guildai--events (items)
  "Append unseen persisted event ITEMS in server order."
  (let ((inhibit-read-only t)
        (follow (= (point) (point-max))))
    (save-excursion
      (goto-char (point-max))
      (dolist (event items)
        (let ((id (alist-get 'id event)))
          (when (and id (not (gethash id guildai--seen)))
            (puthash id t guildai--seen)
            (insert (format "%s\n%s\n\n" (alist-get 'type event)
                            (guildai--text (or (alist-get 'content event)
                                              (alist-get 'message event) event))))
            (setq guildai--cursor id)))))
    (when follow (goto-char (point-max)))))

;;;###autoload
(defun guildai-refresh ()
  "Fetch new events in the current session."
  (interactive)
  (unless guildai--session-id (user-error "This is not a Guild session"))
  (unless guildai--busy
    (let ((buffer (current-buffer)))
      (setq guildai--busy t)
      (condition-case err
          (guildai--all
           (concat "/sessions/" (guildai--segment guildai--session-id)
                   "/events?sort_by=id&types=user_message,agent_notification_message,agent_notification_progress,agent_notification_error,system_error,runtime_error,runtime_done,interrupted"
                   (when guildai--cursor (concat "&from_id=" (guildai--segment guildai--cursor))))
           (lambda (items)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq guildai--busy nil)
                 (guildai--events items))))
           (lambda (problem)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq guildai--busy nil)
                 (guildai--stop)
                 (let ((inhibit-read-only t))
                   (save-excursion (goto-char (point-max))
                                   (insert problem "\nPress g to retry.\n")))))
             (guildai--failure problem)))
        (error (setq guildai--busy nil) (guildai--stop)
               (signal (car err) (cdr err)))))))

(defun guildai--open (id workspace)
  "Open session ID belonging to WORKSPACE."
  (let ((buffer (get-buffer-create (format "*guildai:%s*" id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'guildai-session-mode)
        (guildai-session-mode)
        (setq guildai--session-id id guildai--workspace-id workspace)
        (let ((inhibit-read-only t)) (insert (format "Guild session %s\n\n" id))))
      (guildai-refresh)
      (unless guildai--timer
        (setq guildai--timer
              (run-at-time guildai-poll-interval guildai-poll-interval
                           (lambda ()
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (condition-case err (guildai-refresh)
                                   (error (guildai--stop)
                                          (guildai--failure (error-message-string err)))))))))))
    (pop-to-buffer buffer)))

(defvar guildai-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'guildai-send)
    map))
(define-derived-mode guildai-compose-mode text-mode "Guild-Draft"
  "Edit a Guild prompt; use \\[guildai-send] to send it.")

(defun guildai--draft (workspace session &optional text)
  "Compose TEXT for WORKSPACE and optional SESSION."
  (let ((buffer (generate-new-buffer "*guildai draft*")))
    (with-current-buffer buffer
      (guildai-compose-mode)
      (setq guildai--workspace-id workspace guildai--target session)
      (insert (or text "")))
    (pop-to-buffer buffer)
    (message "Review the draft; C-c C-c sends it to Guild")))

;;;###autoload
(defun guildai ()
  "Select a workspace and compose a new Guild session."
  (interactive)
  (guildai--with-workspace (lambda (workspace) (guildai--draft workspace nil))))

(defun guildai-compose ()
  "Compose a follow-up message for this session."
  (interactive)
  (unless guildai--session-id (user-error "This is not a Guild session"))
  (guildai--draft guildai--workspace-id guildai--session-id))

(defun guildai-send ()
  "Send the current draft once, preserving it on failure."
  (interactive)
  (unless (derived-mode-p 'guildai-compose-mode) (user-error "This is not a Guild draft"))
  (when guildai--sending (user-error "This draft is already being sent"))
  (let* ((buffer (current-buffer))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (workspace guildai--workspace-id)
         (target guildai--target)
         (body (if target `((content . ,text) (mode . "text"))
                 `((session_type . "chat") (initial_prompt . ,text)))))
    (when (string-empty-p (string-trim text)) (user-error "The draft is empty"))
    (unless target (when guildai-agent (push (cons 'agent_id guildai-agent) body)))
    (setq guildai--sending t buffer-read-only t)
    (let ((failed (lambda (problem)
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (setq guildai--sending nil buffer-read-only nil)))
                    (guildai--failure problem))))
      (condition-case err
          (guildai--request
           "POST" (if target (format "/sessions/%s/events" (guildai--segment target))
                    (format "/workspaces/%s/sessions" (guildai--segment workspace)))
           body
           (lambda (response)
             (let ((id (or target (alist-get 'id response))))
               (if (not id) (funcall failed "Guild returned no session ID; draft preserved")
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer (set-buffer-modified-p nil))
                   (kill-buffer buffer))
                 (guildai--open id workspace))))
           failed)
        (error (funcall failed (error-message-string err)))))))

;;;###autoload
(defun guildai-send-region (begin end)
  "Compose a prompt containing the region from BEGIN to END."
  (interactive "r")
  (let ((text (format "Context from %s:\n\n%s\n\nRequest: "
                      (buffer-name) (buffer-substring-no-properties begin end)))
        (session guildai--session-id)
        (workspace guildai--workspace-id))
    (if workspace (guildai--draft workspace session text)
      (guildai--with-workspace (lambda (id) (guildai--draft id nil text))))))

;;;###autoload
(defun guildai-send-buffer ()
  "Compose a prompt containing the accessible current buffer."
  (interactive)
  (guildai-send-region (point-min) (point-max)))

(defun guildai-interrupt ()
  "Interrupt the current Guild session."
  (interactive)
  (unless guildai--session-id (user-error "This is not a Guild session"))
  (guildai--request "POST" (format "/sessions/%s/interrupt"
                                  (guildai--segment guildai--session-id))
                    (make-hash-table) (lambda (_) (message "Guild interrupt requested"))
                    #'guildai--failure))

(defun guildai-open-in-browser ()
  "Open this session in the Guild web application."
  (interactive)
  (unless guildai--session-id (user-error "This is not a Guild session"))
  (browse-url (concat (string-remove-suffix "/api" (string-remove-suffix "/" guildai-api-url))
                      "/sessions/" (guildai--segment guildai--session-id))))

(defvar guildai-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'guildai-session-open)
    map))
(define-derived-mode guildai-sessions-mode tabulated-list-mode "Guild-Sessions"
  "Browse Guild sessions; RET opens the selected session."
  (setq tabulated-list-format [("Session" 38 t) ("Title" 50 t) ("Type" 15 t)])
  (tabulated-list-init-header))

(defun guildai-session-open ()
  "Open the session at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No session at point"))
    (guildai--open id guildai--workspace-id)))

;;;###autoload
(defun guildai-sessions ()
  "Select a workspace and browse its sessions."
  (interactive)
  (guildai--with-workspace
   (lambda (workspace)
     (guildai--all
      (format "/workspaces/%s/sessions" (guildai--segment workspace))
      (lambda (items)
        (let ((buffer (get-buffer-create (format "*guildai sessions:%s*" workspace))))
          (with-current-buffer buffer
            (guildai-sessions-mode)
            (setq guildai--workspace-id workspace
                  tabulated-list-entries
                  (mapcar (lambda (item)
                            (let ((id (alist-get 'id item)))
                              (list id (vector id (or (alist-get 'title item) "")
                                               (or (alist-get 'session_type item) ""))))) items))
            (tabulated-list-print t))
          (pop-to-buffer buffer)))))))

(provide 'guildai)
;;; guildai.el ends here
