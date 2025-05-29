;;; password-store-menu.el --- A better, more complete UI for password-store -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Reindert-Jan Ekker <info@rjekker.nl>

;; Author: Reindert-Jan Ekker <info@rjekker.nl>
;; Maintainer: Reindert-Jan Ekker <info@rjekker.nl>
;; Version: 1.1.0
;; URL: https://github.com/rjekker/password-store-menu
;; Package-Requires: ((emacs "29.1") (password-store "2.3.2") (transient "0.8.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Keywords: convenience data files

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds a transient user interface for working with
;; pass ("the standard Unix password manager"); extending and improving
;; the user interface already implemented by password-store.el

;; For complete documentation, see https://github.com/rjekker/password-store-menu

;;; Code:

(require 'password-store)
(require 'transient)
(require 'vc)
(require 'epa)
(require 'cl-seq)


(defcustom password-store-menu-edit-auto-commit t
  "Automatically commit edited password files to version control."
  :group 'password-store
  :type 'boolean)


(defcustom password-store-menu-key "C-c p"
  "Key to bind to the `password-store-menu' command.

This is used by the `password-store-menu-enable' command."
  :group 'password-store
  :type 'key)

(defcustom password-store-menu-verification-skip-dirs
  '(".git" ".extensions")
  "List of directory names to skip when verifying a directory."
  :group 'password-store
  :type '(repeat string))

(defconst password-store-menu--insert-buffer-name "*password-store-insert*")

;;; Public functions

;;;###autoload
(defun password-store-menu-get (entry &optional callback)
  "Return password for ENTRY.

Returns the first line of the password data.  When CALLBACK is
non-`NIL', call CALLBACK with the first line instead.

This is a re-implementation of `password-store-get', which has the
unfortunate tendency to decrypt twice."
  (let ((secret (let ((inhibit-message t))
                  (auth-source-pass-get 'secret entry))))
    (if (not callback) secret
      (funcall callback secret))))

;;;###autoload
(defun password-store-menu-get-field (entry field &optional callback)
  "Return FIELD for ENTRY.

Returns the first line of the password data.  When CALLBACK is
non-`NIL', call CALLBACK with the first line instead.

This is a re-implementation of `password-store-get-field', which has the
unfortunate tendency to decrypt twice."
  (let ((secret (auth-source-pass-get field entry)))
    (if (not callback) secret
      (and secret (funcall callback secret)))))


;;;###autoload
(defun password-store-menu-copy (entry)
  "Add password for ENTRY into the kill ring.

Clear previous password from the kill ring.  Pointer to the kill
ring is stored in `password-store-kill-ring-pointer'.  Password
is cleared after `password-store-time-before-clipboard-restore'
seconds.

This is a re-implementation of `password-store-copy', which has the
unfortunate tendency to decrypt twice."
  (interactive (list (password-store--completing-read t)))
  (password-store-menu-get
   entry
   (lambda (password)
     (password-store--save-field-in-kill-ring entry password 'secret))))


;;;###autoload
(defun password-store-menu-copy-field (entry field)
  "Add FIELD for ENTRY into the kill ring.

This is a re-implementation of `password-store-copy-field', which has the
unfortunate tendency to decrypt twice."
  (interactive
   (let ((entry (password-store--completing-read)))
     (list entry (password-store-read-field entry))))
  (password-store-menu-get-field
   entry
   field
   (lambda (secret-value)
     (password-store--save-field-in-kill-ring entry secret-value field))))


;;;###autoload (autoload 'password-store-menu-view "password-store-menu")
(defun password-store-menu-view (entry)
  "Show the contents of the selected password file ENTRY."
  (interactive (list (password-store--completing-read)))
  (view-file (password-store--entry-to-file entry)))

;;;###autoload (autoload 'password-store-menu-browse-and-copy "password-store-menu")
(defun password-store-menu-browse-and-copy (entry)
  "Browse ENTRY using `password-store-url', and copy the secret to the kill ring."
  (interactive (list (password-store--completing-read)))
  (password-store-menu-copy entry)
  (password-store-url entry))

;;;###autoload (autoload 'password-store-menu-dired "password-store-menu")
(defun password-store-menu-dired ()
  "Open the password store directory in Dired."
  (interactive)
  (dired (password-store-dir)))

;;;###autoload (autoload 'password-store-menu-visit "password-store-menu")
(defun password-store-menu-visit (entry)
  "Visit file for ENTRY."
  (interactive (list (password-store--completing-read)))
  (with-current-buffer
      (find-file (password-store--entry-to-file entry))
    (password-store-menu-edit-mode)))

;;;###autoload (autoload 'password-store-menu-rename "password-store-menu")
(defun password-store-menu-rename (entry new-entry)
  "Rename ENTRY to NEW-ENTRY."
  (interactive (let* ((e (password-store--completing-read t))
                      (ne (read-string "Rename entry to: " e)))
                 (list e ne)))
  (message "%s" (password-store--run-rename entry new-entry t)))

;;;###autoload (autoload 'password-store-menu-pull "password-store-menu")
(defun password-store-menu-pull ()
  "Pull password store files from version control."
  (interactive)
  (let ((default-directory (password-store-dir)))
    (vc-pull)))

;;;###autoload (autoload 'password-store-menu-push "password-store-menu")
(defun password-store-menu-push ()
  "Push password store files to version control."
  (interactive)
  (let ((default-directory (password-store-dir)))
    (vc-push)))

;;;###autoload (autoload 'password-store-menu-diff "password-store-menu")
(defun password-store-menu-diff ()
  "Show vc diff for password store."
  (interactive)
  (vc-dir (password-store-dir)))

;;;###autoload (autoload 'password-store-menu-qr "password-store-menu")
(defun password-store-menu-qr ()
  "Show QR for given password ENTRY."
  (interactive)
  (if (password-store-menu--qrencode-available-p)
      (password-store-menu--qr-transient)
    (message "Please install qrencode to create QR Codes.")))

;;;###autoload (autoload 'password-store-menu-grp "password-store-menu")
(defun password-store-menu-grep ()
  "Search for text in password files."
  (interactive)
  (password-store-menu--grep-transient))


;;; Inserting new entries
;;;###autoload (autoload 'password-store-menu-insert "password-store-menu")
(defun password-store-menu-insert (entry password)
  "Insert a new ENTRY containing PASSWORD.

This wraps `password-store-insert' with some code to read a new entry."
  (interactive (let ((entry (password-store-menu--completing-read-new-entry)))
                 (list entry
                       (if entry
                           (read-passwd "Password: " t)
                         nil))))
  (when entry (password-store-insert entry password)))


;;;###autoload (autoload 'password-store-menu-insert-multiline "password-store-menu")
(defun password-store-menu-insert-multiline (entry)
  "Insert a multi-line password ENTRY."
  (interactive (list (password-store-menu--completing-read-new-entry)))
  (when entry
    (ignore-errors
      (kill-buffer password-store-menu--insert-buffer-name))
    (let ((buffer (get-buffer-create password-store-menu--insert-buffer-name)))
      (message "%s ""Please insert text for new pass entry, then press `C-c C-c' to save, or `C-c C-k' to cancel.")
      (with-current-buffer buffer
        (password-store-menu-insert-mode)
        (setq-local password-store-menu-new-entry entry))
      (pop-to-buffer buffer)
      "")))


(defvar-keymap password-store-menu-insert-mode-map
  :parent text-mode-map
  "C-c C-c" #'password-store-menu--insert-save
  "C-c C-k" #'password-store-menu--kill-insert-buffer)


(define-derived-mode password-store-menu-insert-mode text-mode "pass-insert"
  "Major mode for editing new password-store entries."
  (setq buffer-offer-save nil))



(defun password-store-menu--kill-insert-buffer (&optional force)
  "Kill buffer containing new pass entry.

Ask for confirmation unless FORCE is t."
  (interactive)
  (when (or force
            (yes-or-no-p "Cancel new pass entry?"))
    (kill-buffer password-store-menu--insert-buffer-name)))


(defun password-store-menu--insert-save ()
  "Save buffer to new password entry; kill the buffer."
  (interactive)
  (with-current-buffer (get-buffer password-store-menu--insert-buffer-name)
    (when (boundp 'password-store-menu-new-entry)
      (password-store-menu-insert password-store-menu-new-entry (buffer-string))))
  (password-store-menu--kill-insert-buffer t))


(defun password-store-menu--commit-on-save ()
  "Function to be called when saving changed password entries."
  (interactive)
  (when password-store-menu-edit-auto-commit
    (when-let ((backend (vc-responsible-backend (password-store-dir) t)))
      (let ((entry (password-store--file-to-entry (buffer-file-name))))
        (when (not (vc-registered (buffer-file-name)))
          (vc-register))
        (vc-call-backend backend 'checkin (list buffer-file-name)
                         (format "Edit password for %s using Emacs" entry) nil)))))

;;; Editing entries
(define-derived-mode password-store-menu-edit-mode text-mode "pass-edit"
  "Major mode for editing password-store entries, which auto-commits changes."
  (add-hook 'after-save-hook #'password-store-menu--commit-on-save nil t))


(defun password-store-menu--maybe-edit-mode ()
  "Start pass-edit mode, but only when we are in the password store."
  (when (file-in-directory-p (buffer-file-name) (password-store-dir))
    (password-store-menu-edit-mode)))


(defun password-store-menu--completing-read-new-entry ()
  "Prompt for name of new pass entry, ask confirmation if it exists."
  (let* ((entry (password-store--completing-read))
         (exists (file-exists-p (password-store--entry-to-file entry))))
    (when (or (not exists)
              (yes-or-no-p (format "Overwrite entry %s?" entry)))
      entry)))


(transient-define-suffix password-store-menu--generate-run-transient
  (entry)
  "Generate a new password for ENTRY."
  (interactive (list (password-store--completing-read)))
  (let ((transient-length-arg nil)
        (args nil)
        (length nil))
    (dolist
        ;; filter length out of the argument list
        (arg (transient-args transient-current-command))
      (if (string-prefix-p "--" arg)
          (push arg args)
        (setq transient-length-arg arg)))
    (setq length (or transient-length-arg password-store-password-length))
    (apply #'password-store--run-async `("generate" ,@args ,entry ,length))))


(defun password-store-menu--read-length (prompt initial-input history)
  "Read a number for the password length, or return default if input empty.

Arguments PROMPT, INITIAL-INPUT and HISTORY are passed to
transient--read-number."
  (let ((input (transient--read-number-N prompt initial-input history nil)))
    (if (string-equal input "")
        (int-to-string password-store-password-length)
      input)))

(transient-define-infix password-store-menu-generate-length ()
  "Password length: should always be set."
  :argument ""
  :key "l"
  :prompt "Password length: "
  :multi-value nil
  :always-read t
  :description "Length"
  :class 'transient-option
  :reader #'password-store-menu--read-length)

(transient-define-prefix password-store-menu-generate-transient ()
  "Generate new password using transient."
  :value `(nil nil nil ,(int-to-string password-store-password-length))
  :incompatible '(("--in-place" "--force"))
  [("i" "In place" "--in-place")
   ("f" "Force overwrite" "--force")
   ("n" "No symbols" "--no-symbols")
   (password-store-menu-generate-length)
   ("g" "Generate" password-store-menu--generate-run-transient)])


(defun password-store-menu--qrencode-ext-available-p ()
  "Return t when we can create qr codes with an external command."
  (executable-find "qrencode"))


(defun password-store-menu--qrencode-available-p ()
  "Return t when we can create qr codes."
  (or (require 'qrencode nil 'noerror)
      (password-store-menu--qrencode-ext-available-p)))


(defun password-store-menu--get-field (entry)
  "Let the user choose a field (excluding the first line)
from ENTRY and return it."
  (let* ((data (cdr (password-store-parse-entry entry)))
         (fields (mapcar #'car data))
         (selected-field (completing-read "Field: " fields nil 'match)))
    (alist-get selected-field data nil nil 'equal)))


(defun password-store-menu--qr-external (secret output-format)
  "Generate QR code for SECRET in OUTPUT-FORMAT using external script."
  (let* ((buf (generate-new-buffer "*password-store-qrcode*"))
         (cmd (format "qrencode %s -o - %s"
                      (if (string= output-format "image")
                          "-tPNG" "-tUTF8")
                      (shell-quote-argument secret))))
    (call-process-shell-command cmd nil buf)
    (with-current-buffer buf
      (if (string= "text" output-format)
          (view-mode t)
        (image-mode)))
    (pop-to-buffer buf)))


(declare-function qrencode-string "qrencode")

(transient-define-suffix password-store-menu--qr-dispatch (entry &rest args)
  "Show QR code for ENTRY."
  (interactive (list (password-store--completing-read)
                     (transient-args transient-current-command)))
  (let* ((content (caar args))
         (output-format (cadar args))
         (secret (if (string= content "secret")
                     (password-store-menu-get entry)
                   (password-store-menu--get-field entry))))
    (if (password-store-menu--qrencode-ext-available-p)
        (password-store-menu--qr-external secret output-format)
      (require 'qrencode)
      (qrencode-string secret))))


;;;###autoload (autoload 'transient-define-prefix "password-store-menu--qr-transient")
(transient-define-prefix password-store-menu--qr-transient ()
  "Generate qr codes for passwords using transient."
  :value '("secret" "text")
  :incompatible '(("secret" "field") ("text" "image"))
  [["What to encode"
    ("s" "Secret" "secret")
    ("f" "Field" "field")]
   ["Output format" :if password-store-menu--qrencode-ext-available-p
    ("t" "Text" "text")
    ("i" "Image" "image")]]
  [("q" "Create QR Code" password-store-menu--qr-dispatch)])


(defun password-store-menu--grep-entry (entry pattern grep-args output-buf)
  "Run grep on a single ENTRY, searching for PATTERN given GREP-ARGS.
Output will be sent to OUTPUT-BUF."
  (with-temp-buffer
    (let* ((cmd (format "%s show %s | grep -n --null %s %s"
                        password-store-executable
                        (shell-quote-argument entry)
                        (mapconcat 'identity grep-args " ")
                        (shell-quote-argument pattern)))
           (retval (call-process-shell-command cmd nil t)))
      (message "Grepping %s" entry)
      (when (eq 0 retval)
        (goto-char (point-min))
        (while (not (eobp))
          (insert (format "./%s.gpg:" entry))
          (when (member "--count" grep-args)
            ;; Fix output to match grep-mode expected format
            (insert "1:"))
          (forward-line))
        (with-current-buffer output-buf
          (setq buffer-read-only nil)
          (goto-char (point-max)))
        (insert-into-buffer output-buf)
        (with-current-buffer output-buf (setq buffer-read-only t))))))


;;;###autoload
(defvar password-store-menu-grep-history nil "History list for password-store-menu grep.")

(transient-define-suffix password-store-menu--grep (pattern args)
  "Run grep for all password entries searching for PATTERN with ARGS."
  (interactive (list
                (read-string "Search pattern: " nil password-store-menu-grep-history)
                (transient-args transient-current-command)))
  (let ((buf (get-buffer-create "*password-store-grep*"))
        (dir (car args))
        (grep-args (cdr args)))
    (with-current-buffer buf
      (setq-local default-directory (password-store-dir)
                  buffer-read-only nil)
      (fundamental-mode)
      (erase-buffer)
      (insert (format "-*- mode: grep; default-directory: \"%s\" -*-\n" default-directory))
      (insert (concat "grep: " (mapconcat 'identity grep-args " ") "\n\n"))
      (grep-mode))
    (pop-to-buffer buf)
    (dolist (entry (password-store-list dir))
      (password-store-menu--grep-entry entry pattern grep-args buf))
    (pop-to-buffer buf)))


(defun password-store-menu--grep-dir-reader (prompt &rest _)
  "Reader for folder to grep in."
  (read-directory-name prompt
                       (expand-file-name "./" (password-store-dir))
                       nil
                       t))


(transient-define-infix password-store-menu--grep-dir ()
  :description "Subdir"
  :class 'transient-option
  :key "d"
  :argument ""
  :prompt "Search folder: "
  :reader #'password-store-menu--grep-dir-reader
  :always-read t)


;;;###autoload (autoload 'transient-define-prefix "password-store-menu--grep-transient")
(transient-define-prefix password-store-menu--grep-transient ()
  "Search for text in password files."
  :incompatible '(("--count" "--files-with-matches" "--files-without-matches")
                  ("--extended-regexp" "--fixed-strings" "--basic-regexp")
                  ("--word-regexp" "--line-regexp"))
  :value `(,(password-store-dir) "--basic-regexp" "--count")
  ["Search in"
   (password-store-menu--grep-dir)]
  ["Grep options"
   ["Pattern"
    ("E" "Regex" "--extended-regexp")
    ("F" "Fixed string" "--fixed-strings")
    ("G" "Basic pattern" "--basic-regexp")]
   ["Match"
    ("i" "Ignore case" "--ignore-case")
    ("v" "Invert" "--invert-match")]
   ["Output"
    ("c" "Count only" "--count")]
   [("g" "Run grep" password-store-menu--grep)]])


(defun password-store-menu--get-key-info (key &optional context)
  "Get GPG key info for KEY, with optional egp CONTEXT.
KEY can be a key id or fingerprint. This is used to find information
about the encryption keys in .gpg-id.

Return a plist containing:
- :key : the main key identified by KEY
- :enc-key: the encryption subkey
- :enc-id : the id of the encryption subkey
= :description : the description string"
  (let ((epg-keys (epg-list-keys (or context (epg-make-context)) key)))
    (when (> (length epg-keys) 1)
      (error "Key id '%s' found in .gpg-id matches multiple keys" key))
    (unless epg-keys
      (error "Key id '%s' found in .gpg-id matches no keys" key))
    (let* ((epg-key (car epg-keys))
           (subkeys (epg-key-sub-key-list epg-key))
           (encryption-keys
            (seq-filter (lambda (subkey)
                          (memq 'encrypt (epg-sub-key-capability subkey)))
                        subkeys)))
      (when (> (length encryption-keys) 1)
        ;; This should be impossible
        (error "Key id '%s' found in .gpg-id has multiple encryption subkeys" key))
      (unless encryption-keys
        (error "Key id '%s' found in .gpg-id has no encryption subkeys" key))

      (list
       :key epg-key
       :enc-key (car encryption-keys)
       :enc-id (epg-sub-key-id (car encryption-keys))
       :description (epg-user-id-string (car (epg-key-user-id-list epg-key)))))))


(defun password-store-menu--get-gpg-id-info (file &optional context)
  "Get information about intended recipients for FILE, with optional epg CONTEXT.

This returns key information from .gpg-id, by calling
`password-store-menu--get-key-info' for each line and returning
a list of key-info results."
  (if-let* ((gpg-id-dir (locate-dominating-file file ".gpg-id" ))
            (gpg-id-file (concat gpg-id-dir ".gpg-id"))
            (key-ids (with-temp-buffer
                       (insert-file-contents gpg-id-file)
                       (split-string (buffer-string) "\n" :omit-nulls))))
      (mapcar (lambda (key-id)
                (password-store-menu--get-key-info key-id context))
              key-ids)
    (error "No .gpg-id found for \"%s\", is it a password folder?" file)))


(defun password-store-menu--keys-info-to-recipients (keys-info)
  "Convert list KEYS-INFO to list of recipient key ids."
  (mapcar (lambda (key-info) (plist-get key-info :enc-id))
          keys-info))

(defun password-store-menu--get-file-recipients (file context)
  "Retrieve encryption recipients for FILE using epg CONTEXT.

This returns the actual recipients the file is encrypted with.
This will signal an error when the file is not actually encrypted."
  (mapcan (lambda (s)  (last (split-string s " ")))
          (seq-filter (apply-partially #'string-prefix-p ":pubkey")
                      (process-lines
                       (epg-context-program (or context (epg-make-context)))
                       "--list-only"
                       "--list-packets"
                       file))))

(defun password-store-menu--verify-file (file recipients &optional context)
  "Verifies that FILE is encrypted for RECIPIENTS, with optional epg CONTEXT.

Return nil if recipients match and a string with a message otherwise.
This is NOT the same as a GPG signature verification."
  (condition-case-unless-debug nil
      (let* ((file-recipients
              (password-store-menu--get-file-recipients file
                                                        (or context (epg-make-context))))
             (diff (cl-set-exclusive-or recipients file-recipients :test 'equal)))
        (when diff
          (if-let ((extra (cl-set-difference file-recipients recipients :test 'equal)))
              (format "File has recipients not listed in .gpg-id: %s" extra)
            (format "File has missing recipients: %s" (cl-set-difference recipients file-recipients :test 'equal)))))
    (error "GPG failed - file not encrypted or corrupt")))

(defun password-store-menu--verify-file-p (file)
  "Return t when we want to verify FILE."
  (not (and
        (file-directory-p file)
        (or
         (member (file-name-nondirectory (directory-file-name file))
                 password-store-menu-verification-skip-dirs)
         ;; skip dirs with their own gpg-id
         (file-exists-p (expand-file-name ".gpg-id" file))))))

(defun password-store-menu--verify-dir (dir recipients)
  "Verify that all files in DIR are encrypted for RECIPIENTS."
  (let ((failed 0)
        (count 0))
    (dolist (file
             (mapcar #'expand-file-name
                     (directory-files-recursively dir "" nil
                                                  #'password-store-menu--verify-file-p)))
      (unless (equal file (expand-file-name ".gpg-id" dir))
        (setq count (+ 1 count))
        (when-let ((message (password-store-menu--verify-file
                             file
                             recipients)))
          (progn (insert (format "Verification of %s FAILED: " file)
                         message "\n")
                 (setq failed (+ 1 failed))))))
    (insert "\n\n--------\n")
    (if (= 0 failed)
        (insert (format "Checked %s files: all good!" count))
      (insert (format "%s files failed verification (%s files ok)" failed count)))))

;;;###autoload
(defun password-store-menu-verify-recipients (file-or-dir &optional show-only)
  "Verify all files in FILE-OR-DIR are encrypted for recipients in .gpg-id.
When SHOW-ONLY is non-nil, just show the recipients."
  (interactive (list
                (read-directory-name "Choose password folder to verify: "
                                     (password-store-dir))))
  (if-let* ((gpg-id-dir (locate-dominating-file file-or-dir ".gpg-id" ))
            (gpg-id-file (concat gpg-id-dir ".gpg-id"))
            (key-ids (with-temp-buffer
                       (insert-file-contents gpg-id-file)
                       (split-string (buffer-string) "\n" :omit-nulls)))
            (keys-info (mapcar #'password-store-menu--get-key-info key-ids))
            (buf (get-buffer-create (format "*pass-verify: %s*" file-or-dir))))
      (with-current-buffer buf
        (view-mode t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Encryption key ids from " gpg-id-file "\n"
                  "------------------\n"
                  "(Note: these are the id's of the encryption subkeys,\n"
                  "       which may differ from the actual values in .gpg-id)\n\n")
          (dolist (key-info keys-info)
            (insert (plist-get key-info :enc-id) " : "
                    (plist-get key-info :description) "\n"))
          (insert "------------------\n")
          (unless show-only
            (let ((recipients (password-store-menu--keys-info-to-recipients
                               keys-info)))
              (if (file-directory-p file-or-dir)
                  (password-store-menu--verify-dir file-or-dir recipients)
                (if-let ((message (password-store-menu--verify-file file-or-dir recipients)))
                    (insert (format "Verification of %s FAILED:\n" file-or-dir)
                            message)
                  (insert (format "Verification of %s: OK" file-or-dir)))))))
        (pop-to-buffer buf))
    (error "No .gpg-id found in \"%s\", is it a password folder?" file-or-dir)))

;;;###autoload
(defun password-store-menu-verify-entry (entry)
  "Verify that ENTRY is encrypted with correct recipients."
  (interactive (list (password-store--completing-read)))
  (let ((file (password-store--entry-to-file entry)))
    (password-store-menu-verify-recipients file)))

;;;###autoload
(defun password-store-menu-show-recipients (dir)
  "Read recipients for DIR from .gpg-id and show them in a buffer."
  (interactive (list
                (read-directory-name "Choose password folder: "
                                     (password-store-dir))))
  (password-store-menu-verify-recipients dir :show-only))

;;;###autoload
(defun password-store-menu-verify-dir (dir)
  "Read recipients for DIR from .gpg-id and show them in a buffer."
  (interactive (list
                (read-directory-name "Choose password folder to verify: "
                                     (password-store-dir))))
  (password-store-menu-verify-recipients dir))

(defun password-store-menu--init (dir)
  "Read recipients for DIR and call pass init."
  (let* ((subdir (file-relative-name (expand-file-name dir) (password-store-dir)))
         (keys (epa-select-keys (epg-make-context)
                                (format "Please select keys for encrypting passwords in %s" subdir)))
         (fingerprints (mapcar (lambda (k)
                                 (epg-sub-key-fingerprint (car (epg-key-sub-key-list k))))
                               keys)))
    (apply #'password-store--run-1
           #'message
           (append (list "init" "-p" subdir) fingerprints))))

;;;###autoload
(defun password-store-menu-init (dir)
  "Read recipients for DIR, and (re)-initialize."
  (interactive (list
                (read-directory-name "Choose password folder to init: "
                                     (password-store-dir))))
  (if (not (file-exists-p dir))
      (password-store-menu--init dir)
    (if (not (file-directory-p dir))
        (error "%s is not a directory; cannot initialize" dir)
      (when (yes-or-no-p (format "Re-initialize recipients for %s (this will re-encrypt all files)?" dir))
        (password-store-menu--init dir)))))

;;;###autoload
(defun password-store-menu-kill-all-buffers ()
  (interactive)
  (dolist (buf (match-buffers '(derived-mode . password-store-menu-edit-mode)))
    (if (buffer-modified-p buf)
        (kill-buffer-ask buf)
      (kill-buffer buf))))

;;;###autoload (autoload 'transient-define-prefix "password-store-menu--recipients-transient")
(transient-define-prefix password-store-menu--recipients-transient ()
  "Transient for gpg-related operations"
  ["Recipients"
   ("s" "Show recipients" password-store-menu-show-recipients)
   ("e" "Verify entry recipients" password-store-menu-verify-entry)
   ("d" "Verify all entries in dir" password-store-menu-verify-dir)
   ("R" "Re-encrypt directory with new recipients" password-store-menu-init)])

;;;###autoload (autoload 'transient-define-prefix "password-store-menu")
(transient-define-prefix password-store-menu ()
  "Entry point for password store actions."
  ["Password Store"
   ["Use"
    ("b" "Browse" password-store-url)
    ("c" "Copy Secret" password-store-menu-copy)
    ("f" "Copy Field" password-store-menu-copy-field)
    ("o" "Browse and copy" password-store-menu-browse-and-copy)
    ("p" "Copy Secret" password-store-menu-copy)
    ("v" "View" password-store-menu-view)
    ("q" "QR code" password-store-menu--qr-transient :if password-store-menu--qrencode-available-p)
    ("q" :info "Install qrencode to enable QR codes" :if-not password-store-menu--qrencode-available-p)]
   ["Change"
    ("D" "Delete" password-store-remove)
    ("e" "Edit (visit file)" password-store-menu-visit)
    ("E" "Edit (pass command)" password-store-edit)
    ("i" "Insert password" password-store-menu-insert)
    ("I" "Insert multiline" password-store-menu-insert-multiline)
    ("g" "generate" password-store-menu-generate-transient :transient transient--do-exit)
    ("r" "Rename" password-store-menu-rename)]
   ["Store"
    ("+" "Init subfolder" password-store-menu-init)
    ("d" "Dired" password-store-menu-dired)
    ("G" "Grep" password-store-menu--grep-transient)
    ("R" "Recipients" password-store-menu--recipients-transient)]
   ["Version control" :if (lambda () (vc-responsible-backend (password-store-dir) t))
    ("V=" "Diff" password-store-menu-diff)
    ("Vp" "Pull" password-store-menu-pull)
    ("VP" "Push" password-store-menu-push)]]
  [("!" "Clear secret from kill ring" password-store-clear)
   ("X" "Kill all pass buffers" password-store-menu-kill-all-buffers)])

;;;###autoload (autoload 'password-store-menu-enable "password-store-menu")
(defun password-store-menu-enable ()
  "Run this to setup `auto-mode-alist' and keybinding for `password-store-menu'."
  (interactive)
  (add-to-list 'auto-mode-alist (cons epa-file-name-regexp 'password-store-menu--maybe-edit-mode))
  (define-key global-map (kbd password-store-menu-key) #'password-store-menu))

(provide 'password-store-menu)
;;; password-store-menu.el ends here
