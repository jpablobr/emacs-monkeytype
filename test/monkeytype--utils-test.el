;;; monkeytype--utils-test.el --- Monkeytype: tests  -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Pablo Barrantes
;; Author: Pablo Barrantes <xjpablobrx@gmail.com>
;;; Commentary:
;; Utility/Miscellaneous related tests.
;;; Code:

(require 'monkeytype)

(ert-deftest monkeytype-test-utils-idle-timer ()
  "Setup and cancellation of idle timer."
  (should (monkeytype--utils-idle-timer 5 'test-func))
  (should (equal (elt monkeytype--idle-timer 2) 5))
  (should (equal (elt monkeytype--idle-timer 5) 'test-func))
  (cancel-timer monkeytype--idle-timer))

(ert-deftest monkeytype-test-utils-file-path ()
  "Path to monkeytype files."
  (let ((rgx "^[~/]/.+/words/.+\\.txt\\'")
        (path (monkeytype--utils-file-path "words")))
    (should (string-match-p rgx path))))

(ert-deftest monkeytype-test-utils-text-file-name ()
  "Files name format."
  (let ((rgx "^[[:alnum:]-].+[-[:digit:]]"))
    (should (string-match-p rgx (monkeytype--utils-text-file-name)))))

(ert-deftest monkeytype-test-utils-save-run ()
  "Files for saving monkeytype user data."
  (let ((run (make-hash-table :test 'equal))
        (entries '((x . y))))
    (puthash 'entries entries run)
    (unless (file-exists-p "json") (make-directory "json"))
    (setq monkeytype--text-file-directory (expand-file-name "./"))
    (monkeytype--utils-save-run run)
    (should
     (file-exists-p
      (car (directory-files "./json" t "\\.json\\'" nil))))
    (if (file-exists-p "json") (delete-directory "json" t))))

(ert-deftest monkeytype-test-utils-elapsed-seconds ()
  "Formatting for seconds."
  (setq monkeytype--start-time (float-time))
  (should (floatp (monkeytype--utils-elapsed-seconds))))

(ert-deftest monkeytype-test-utils-check-same ()
  "Char equality check."
  (should (monkeytype--utils-check-same "f" "f"))
  (should (monkeytype--utils-check-same " " " "))
  (should (not (monkeytype--utils-check-same "f" "x"))))

(ert-deftest monkeytype-test-utils-seconds-to-minutes ()
  "Seconds to minutes."
  (should (equal (monkeytype--utils-seconds-to-minutes 120) 2.0))
  (should
   (equal (monkeytype--utils-seconds-to-minutes 20) 0.3333333333333333)))

(ert-deftest monkeytype-test-utils-index-words ()
  "alist of for words to index/position in text."
  (let ((words '(
                (4 . "")
                (3 . "")
                (2 . "bar")
                (1 . "foo"))))
    ;; monkeytype--words should be empty
    (should (null monkeytype--words))
    ;; alist should keep reference to removed special characters
    ;; for indexing char-to-word purposes.
    (setq monkeytype--source-text "foo bar^%")
    (monkeytype--utils-index-words)
    (should (equal words monkeytype--words))))

(ert-deftest monkeytype-test-utils-index-chars-to-words ()
  "Associate chars to words."
  (let ((association '((7 . "bar")
					             (6 . "bar")
					             (5 . "bar")
                       ;; Whitespace or special characters
                       ;; are not included.
					             (3 . "foo")
					             (2 . "foo")
					             (1 . "foo"))))
    ;; monkeytype--words should be empty
    (should (null monkeytype--words))
    (setq monkeytype--source-text "foo bar^%")
    ;; Process words/chars
    (monkeytype--utils-index-words)
    (monkeytype--utils-index-chars-to-words)
    (should (equal association monkeytype--chars-to-words))))

(ert-deftest monkeytype-test-utils-index-chars ()
  "Index chars for given run."
  (let ((run (make-hash-table :test 'equal))
        (entry1 (make-hash-table :test 'equal))
        (entry2 (make-hash-table :test 'equal))
        (entry3 (make-hash-table :test 'equal))
        (chars '((1 . "f")
                 (2 . "o")
                 (3 . "o"))))
    (puthash 'source-index 1 entry1)
    (puthash 'source-index 2 entry2)
    (puthash 'source-index 3 entry3)
    (puthash 'entries (list entry3 entry2 entry1) run)
    (should (null monkeytype--chars))
    (setq monkeytype--source-text "foo")
    (monkeytype--utils-index-chars run)
    (should (equal chars monkeytype--chars))
    (setq monkeytype--chars nil))

  (let ((run (make-hash-table :test 'equal))
        (entry1 (make-hash-table :test 'equal))
        (entry2 (make-hash-table :test 'equal))
        (entry3 (make-hash-table :test 'equal))
        (chars '((4 . "b")
                 (5 . "a")
                 (6 . "r"))))
    (puthash 'source-index 4 entry1)
    (puthash 'source-index 5 entry2)
    (puthash 'source-index 6 entry3)
    (puthash 'entries (list entry3 entry2 entry1) run)
    (should (null monkeytype--chars))
    ;; Make run a second one. Starting on index 3
    (setq monkeytype--previous-last-entry-index 3)
    (setq monkeytype--source-text "foobar")
    (monkeytype--utils-index-chars run)
    (should (equal chars monkeytype--chars))))

(ert-deftest monkeytype-test-utils-format-words ()
  "Words formatting."
  (setq monkeytype-randomize nil)
  (should
   (equal "foo bar" (monkeytype--utils-format-words '("Foo" "bar"))))
  (setq monkeytype-downcase nil)
  (should
   (equal "Foo bar" (monkeytype--utils-format-words '("Foo" "bar ")))))

(ert-deftest monkeytype-test-utils-format-text ()
  "Text formatting."
  (setq monkeytype-randomize nil)
  (should (equal "Foo bar" (monkeytype--utils-format-text "Foo bar ")))
  (setq monkeytype-delete-trailing-whitespace nil)
  (should (equal "Foo bar " (monkeytype--utils-format-text "Foo bar "))))
(provide 'monkeytype-tests)

;;; monkeytype--utils-test.el ends here
