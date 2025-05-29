(require 'password-store-menu)
(require 'ert)

(defun psmt-get-or-create-key (name)
  (let ((context (epg-make-context)))
    (call-process
     (epg-context-program context) nil nil nil
     "--quick-generate-key" "--batch" "--passphrase" "" name)
    (if-let ((keys-found (epg-list-keys context name)))
        (car keys-found))))


(defun psmt-get-fingerprint (key)
  (epg-sub-key-fingerprint
      (car (epg-key-sub-key-list key))))


(defun psmt-delete-key (key)
  (let ((context (epg-make-context)))
    (call-process
     (epg-context-program context) nil (get-buffer-create "*gpg-test*") nil
     "--delete-secret-and-public-key" "--yes" "--batch"
     (psmt-get-fingerprint key))))


(defun psmt-create-file (filename recipients)
  (with-temp-file filename
    (insert "test"))
  (epa-encrypt-file filename recipients))


(defun psmt-get-enc-id (key)
  (epg-sub-key-id (car (reverse (epg-key-sub-key-list key)))))



(defmacro psmt-gpg-fixture (body)
  `(unwind-protect
      (let* ((psmt-intended
             (list (psmt-get-or-create-key "psmt first intended rcpt")
                   (psmt-get-or-create-key "psmt second intended rcpt")))
            (psmt-unintended
             (list (psmt-get-or-create-key "psmt first UNintended rcpt")
                   (psmt-get-or-create-key "psmt second UNintended rcpt")))
            (intended-keys (mapcar #'psmt-get-enc-id psmt-intended))
            (unintended-keys (mapcar #'psmt-get-enc-id psmt-unintended))
            (pwd-dir (make-temp-file "" :dir))
            (gpg-id (file-name-concat pwd-dir ".gpg-id")))
        (with-temp-file gpg-id
          (dolist (key psmt-intended)
            (insert (psmt-get-fingerprint key) "\n")))
        ,body
        (mapc #'psmt-delete-key (append psmt-intended psmt-unintended)))))


(ert-deftest psmt-correct ()
  (psmt-gpg-fixture
   (let ((filename (file-name-concat pwd-dir "correct")))
     (psmt-create-file filename psmt-intended)
     (should (null
              (password-store-menu--verify-file
               (concat filename ".gpg") intended-keys))))))

(ert-deftest psmt-incorrect-only ()
  (psmt-gpg-fixture
   (let ((filename (file-name-concat pwd-dir "incorrect")))
     (psmt-create-file filename psmt-unintended)
     (should (equal
              (format "File has recipients not listed in .gpg-id: %s" unintended-keys)
              (password-store-menu--verify-file
               (concat filename ".gpg") intended-keys))))))

(ert-deftest psmt-toomany ()
  (psmt-gpg-fixture
   (let ((filename (file-name-concat pwd-dir "toomany"))
         (all-recipients (append psmt-intended psmt-unintended)))
     (psmt-create-file filename all-recipients)
     (should (equal
              (format "File has recipients not listed in .gpg-id: %s" unintended-keys)
              (password-store-menu--verify-file
               (concat filename ".gpg") intended-keys))))))

(ert-deftest psmt-missing ()
  (psmt-gpg-fixture
   (let ((filename (file-name-concat pwd-dir "missing"))
         (single-recipient (cdr psmt-intended)))
     (psmt-create-file filename single-recipient)
     (should (equal
              (format "File has missing recipients: %s"  (list (car intended-keys)))
              (password-store-menu--verify-file
               (concat filename ".gpg") intended-keys))))))

(ert-deftest psmt-unencrypted ()
  (psmt-gpg-fixture
   (let ((filename (file-name-concat pwd-dir "unencrypted.txt")))
     (with-temp-file filename
       (insert "test"))
     (should (equal
              "GPG failed - file not encrypted or corrupt"
              (password-store-menu--verify-file
               (concat filename ".gpg") intended-keys))))))


;; Local Variables:
;; read-symbol-shorthands: (("psmt-" . "password-store-menu-test-"))
;; End:
