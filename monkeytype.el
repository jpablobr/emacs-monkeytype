;;; monkeytype.el --- Mode for speed/touch typing -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Pablo Barrantes

;; Author: Pablo Barrantes <xjpablobrx@gmail.com>
;; Maintainer: Pablo Barrantes <xjpablobrx@gmail.com>
;; Version: 0.1.0
;; Keywords: games
;; URL: http://github.com/jpablobr/emacs-monkeytype
;; Package-Requires: ((emacs "24.3") (seq "2.19") (ht "2.2"))

;;; Commentary:

;; Emacs Monkeytype is a typing game/tutor inspired by
;; monkeytype.com but for Emacs.

;; Features:

;; - Type any text you want.
;; - Visual representation of typed text including errors and retries.
;; - UI customisation.
;; - Auto stops after 5 seconds of no input (monkeytype-resume] resumes).

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'ht)
(require 'async)

;;;; Customization

(defgroup monkeytype nil
  "Monkeytype."
  :group 'games
  :tag    "Monkeytype"
  :link   '(url-link :tag "GitHub" "https://github.com/jpablobr/emacs-monkeytype"))

(defgroup monkeytype-faces nil
  "Font-lock faces for `monkeytype'."
  :group 'monkeytype
  :group 'faces)

(defface monkeytype--buffer-face-mode-face
  '((t (:family "Menlo")))
  "Face for text area.")

(defface monkeytype--label-face
  '((t (:foreground "#969896")))
  "Face for labels.")

(defface monkeytype--correct-face
  '((t (:foreground "#98be65")))
  "Face for correctly typed char.")

(defface monkeytype--error-face
  '((t (:foreground "#ff6c6b")))
  "Face for wrongly typed char.")

(defface monkeytype--correction-error-face
  '((t (:inherit region :foreground "#d54e53")))
  "Face for wrongly typed correction.")

(defface monkeytype--correction-correct-face
  '((t (:inherit region :foreground "#b9ca4a")))
  "Face for correctly typed correction.")

(defface monkeytype--header-1-face
  '((t (:foreground "#c5c8c6" :height 240)))
  "Runs performance header 1")

(defface monkeytype--header-2-face
  '((t (:foreground "#B7950B" :height 200)))
  "Runs performance header 2")

(defface monkeytype--header-3-face
  '((t (:foreground "#969896" :height 180)))
  "Runs performance header 3")

;;;; Configurable settings:

(defcustom monkeytype--treat-newline-as-space t
  "Allow continuing to the next line by pressing space."
  :type 'boolean
  :group 'monkeytype-mode)

(defcustom monkeytype--insert-log nil
  "Show log in results section."
  :type 'boolean
  :group 'monkeytype-mode)

;;;; Setup:

(defvar monkeytype--finished nil)
(make-variable-buffer-local 'monkeytype--finished)
(defvar monkeytype--start-time nil)
(make-variable-buffer-local 'monkeytype--start-time)
(defvar monkeytype--source-text "")
(defvar monkeytype--entries-counter 0)
(make-variable-buffer-local 'monkeytype--entries-counter)
(defvar monkeytype--input-counter 0)
(make-variable-buffer-local 'monkeytype--input-counter)
(defvar monkeytype--error-counter 0)
(make-variable-buffer-local 'monkeytype--error-counter)
(defvar monkeytype--correction-counter 0)
(make-variable-buffer-local 'monkeytype--correction-counter)
(defvar monkeytype--error-list '())
(make-variable-buffer-local 'monkeytype--error-list)
(defvar monkeytype--source-text-length 0)
(make-variable-buffer-local 'monkeytype--source-text-length)
(defvar monkeytype--remaining-counter 0)
(make-variable-buffer-local 'monkeytype--remaining-counter)
(defvar monkeytype--progress nil)
(make-variable-buffer-local 'monkeytype--progress)
(defvar monkeytype--buffer-name "*Monkeytype*")
(make-variable-buffer-local 'monkeytype--buffer-name)
(defvar monkeytype--typing-buffer nil)
(defvar monkeytype--run-list '())
(defvar monkeytype--current-run-list '())
(defvar monkeytype--current-run-start-datetime nil)
(defvar monkeytype--change>ignored-change-counter 0)
(defvar monkeytype--mistyped-words-list '())
(defvar monkeytype--chars-to-words-list '())
(defvar monkeytype--words-list '())
(make-variable-buffer-local 'monkeytype--change>ignored-change-counter)

(defun monkeytype--run-with-local-idle-timer (secs repeat function &rest args)
  "Like `run-with-idle-timer', but always run in `current-buffer'.
Cancels itself, if this buffer is killed or after 5 SECS.
REPEAT FUNCTION ARGS."
  (let* ((fns (make-symbol "local-idle-timer"))
         (timer (apply 'run-with-idle-timer secs repeat fns args))
         (fn `(lambda (&rest args)
                (if (not (buffer-live-p ,(current-buffer)))
                    (cancel-timer ,timer)
                  (with-current-buffer ,(current-buffer)
                    (apply (function ,function) args))))))
    (fset fns fn)
    fn))

(defun monkeytype--setup (text)
  "Set up a new buffer for the typing exercise on TEXT."
  (with-temp-buffer
    (insert text)
    (delete-trailing-whitespace)
    (setq text (buffer-string)))

  (setq monkeytype--typing-buffer (generate-new-buffer monkeytype--buffer-name))
  (let* ((len (length text)))
    (set-buffer monkeytype--typing-buffer)
    (setq monkeytype--mistyped-words-list '())
    (setq monkeytype--chars-to-words-list '())
    (setq monkeytype--words-list '())
    (setq monkeytype--source-text text)
    (setq monkeytype--source-text-length (length text))
    (setq monkeytype--current-run-list '())
    (setq monkeytype--run-list '())
    (setq monkeytype--progress (make-string len 0))
    (setq monkeytype--remaining-counter (length text))
    (erase-buffer)
    (insert monkeytype--source-text)
    (set-buffer-modified-p nil)
    (switch-to-buffer monkeytype--typing-buffer)
    (goto-char 0)

    (face-remap-add-relative 'default 'monkeytype--buffer-face-mode-face)
    (monkeytype--add-hooks)
    (monkeytype-mode)

    (message "Monkeytype: Timer will start when you type the first character.")))

;;;; Change:

(defun monkeytype--change (start end change-length)
  "START END CHANGE-LENGTH."
  (let* ((source-start (1- start))
         (source-end (1- end))
         (entry (substring-no-properties (buffer-substring start end)))
         (source (substring monkeytype--source-text source-start source-end))
         (deleted-text (substring monkeytype--source-text source-start (+ source-start change-length)))
         (update (lambda ()
                   (monkeytype--change>handle-del source-start end deleted-text)
                   (monkeytype--change>diff source entry start end)
                   (when (monkeytype--change>add-to-entriesp entry change-length)
                     (monkeytype--change>add-to-entries source-start entry source)))))
    (funcall update)
    (goto-char end)
    (when (= monkeytype--remaining-counter 0) (monkeytype--handle-complete))))

(defun monkeytype--change>handle-del (source-start end deleted-text)
  "Keep track of statistics when deletion occurs between SOURCE-START and END DELETED-TEXT."
  (delete-region (1+ source-start) end)
  (let* ((entry-state (aref monkeytype--progress source-start)))
    (cond ((= entry-state 1)
           (cl-decf monkeytype--entries-counter)
           (cl-incf monkeytype--remaining-counter))
          ((= entry-state 2)
           (cl-decf monkeytype--entries-counter)
           (cl-incf monkeytype--remaining-counter)
           (cl-decf monkeytype--error-counter)
           (cl-incf monkeytype--correction-counter)))
    (store-substring monkeytype--progress source-start 0)
    (insert deleted-text)))

(defun monkeytype--change>diff (source entry start end)
  "Update stats and buffer contents with result of changed text.
SOURCE ENTRY START END."
  (when (/= start end)
    (let* ((correct (monkeytype--check-same source entry))
          (progress-index (1- start))
          (face-for-entry (monkeytype--final-text>typed-entry-face correct)))
      (if correct
        (store-substring monkeytype--progress progress-index 1)
        (progn
          (cl-incf monkeytype--error-counter)
          (store-substring monkeytype--progress progress-index 2)))
      (cl-incf monkeytype--entries-counter)
      (cl-decf monkeytype--remaining-counter)

      (if (fboundp 'add-face-text-property)
          (add-face-text-property start (1+ start) face-for-entry)
        (add-text-properties start (1+ start) `(face ,@face-for-entry))))))

(defun monkeytype--change>add-to-entriesp (entry change-length)
  "Add ENTRY CHANGE-LENGTH.

HACK: Properly fix BUG where \"f\" character produces a delete and re-enter
event. ATM this only ignores those events since at least the stats do not get
affected. Only set monkeytype--ignored-change-counter when the
`last-input-event' is a character(e.i., integerp not M-backspace)."
  (cond
   ((= 0 (length entry))
    (when (integerp last-input-event)
      (setq monkeytype--change>ignored-change-counter change-length))
    nil)
   ((> monkeytype--change>ignored-change-counter 0) ;; Number of changes to be ignored.
    (cl-decf monkeytype--change>ignored-change-counter)
    nil)
   (t t)))

(defun monkeytype--change>add-to-entries (source-start change-typed change-source)
  "SOURCE-START CHANGE-TYPED CHANGE-SOURCE."
  (cl-incf monkeytype--input-counter)
  (let ((entry (ht ('input-index monkeytype--input-counter)
                   ('typed-entry change-typed)
                   ('source-entry change-source)
                   ('source-index (1+ source-start))
                   ('error-count monkeytype--error-counter)
                   ('correction-count monkeytype--correction-counter)
                   ('state (aref monkeytype--progress source-start))
                   ('elapsed-seconds (monkeytype--elapsed-seconds))
                   ('formatted-seconds (format-seconds "%.2h:%z%.2m:%.2s" (monkeytype--elapsed-seconds))))))
    (add-to-list 'monkeytype--current-run-list entry)))

(defun monkeytype--first-change ()
  "Start the timer."
  (when (not monkeytype--start-time)
    (setq monkeytype--current-run-start-datetime (format-time-string "%a-%d-%b-%Y %H:%M:%S"))
    (setq monkeytype--start-time (float-time))
    (monkeytype--run-with-local-idle-timer 5 nil 'monkeytype-pause)))

(defun monkeytype--pause-run (&optional print-results)
  "Pause run and optionally PRINT-RESULTS."
  (setq monkeytype--start-time nil)
  (remove-hook 'after-change-functions 'monkeytype--change)
  (remove-hook 'first-change-hook 'monkeytype--first-change)
  (monkeytype--add-to-run-list)
  (when print-results
    (funcall print-results))
  (read-only-mode))

(defun monkeytype--handle-complete ()
  "Remove typing hooks from the buffer and print statistics."
  (setq monkeytype--finished t)
  (monkeytype--pause-run 'monkeytype--print-results)
  (monkeytype-mode))

(defun monkeytype--add-to-run-list ()
  "Add."
  (add-to-list
   'monkeytype--run-list
   (ht ('started-at monkeytype--current-run-start-datetime)
       ('finished-at (format-time-string "%a-%d-%b-%Y %H:%M:%S"))
       ('entries (vconcat monkeytype--current-run-list)))))

;;;; Utils:

(defun monkeytype--add-hooks ()
  "Add hooks."
  (make-local-variable 'after-change-functions)
  (make-local-variable 'first-change-hook)
  (add-hook 'after-change-functions 'monkeytype--change nil t)
  (add-hook 'first-change-hook 'monkeytype--first-change nil t))

(defun monkeytype--print-results ()
  "Print all results."
  (erase-buffer)

  (when (> (length monkeytype--run-list) 1)
    (insert (propertize (format "%s" "Overall Results:\n\n") 'face 'monkeytype--header-1-face))
    (insert (monkeytype--final-performance-results))
    (insert (propertize (format "%s" "\n\nBreakdown by Runs:\n\n") 'face 'monkeytype--header-1-face)))

  (dolist (run (reverse monkeytype--run-list))
    (insert (format "%s:\n" (ht-get  run 'started-at)))
    (insert (monkeytype--run-typed-text run))
    (insert (monkeytype--run-performance-results (ht-get  run 'entries)))
    (insert "\n\n")

    (when monkeytype--insert-log
      (async-start
       `(lambda () ,(monkeytype--run-log run) 1)
       (lambda (result)
         (message "Monkeytype: Log generated successfully. (%s)" result)))))
  (goto-char (point-min)))

(defun monkeytype--elapsed-seconds ()
  "Return float with the total time since start."
  (let ((end-time (float-time)))
    (if (not monkeytype--start-time)
        0 (- end-time monkeytype--start-time))))

(defun monkeytype--check-same (source typed)
  "Return non-nil if both POS (SOURCE and TYPED) are white space or the same."
  (if monkeytype--treat-newline-as-space
    (or (string= source typed)
        (and
         (= (char-syntax (aref source 0)) ?\s)
         (= (char-syntax (aref typed 0)) ?\s)))
    (string= source typed)))

(defun monkeytype--seconds-to-minutes (seconds)
  "Return minutes in float for SECONDS."
  (/ seconds 60.0))

;;;; Equations:

(defcustom monkeytype--word-divisor 5.0
  "5 is the most common number for these calculations.
Proper word count doesn't work as well since words have different number
of characters. This also makes calculations easier and more accurate."
  :type 'integer
  :group 'monkeytype-mode)

(defun monkeytype--words (chars)
  "Divide all CHARS by divisor."
  (/ chars monkeytype--word-divisor))

(defun monkeytype--gross-wpm (words minutes)
  "Divides WORDS by MINUTES."
  (/ words minutes))

(defun monkeytype--gross-cpm (chars minutes)
  "Divides CHARS by MINUTES."
  (/ chars minutes))

(defun monkeytype--net-wpm (words uncorrected-errors minutes)
  "Net WPM is the gross WPM minus the UNCORRECTED-ERRORS by MINUTES.
All WORDS count."
  (let ((net-wpm (- (monkeytype--gross-wpm words minutes)
                    (/ uncorrected-errors minutes))))
    (if (> 0 net-wpm) 0 net-wpm)))

(defun monkeytype--net-cpm (chars uncorrected-errors minutes)
  "Net CPM is the gross CPM minus the UNCORRECTED-ERRORS by MINUTES.
All CHARS count."
  (let ((net-cpm (- (monkeytype--gross-cpm chars minutes)
                    (/ uncorrected-errors minutes))))
    (if (> 0 net-cpm) 0 net-cpm)))

(defun monkeytype--accuracy (chars correct-chars corrections)
  "Accuracy is all CORRECT-CHARS minus CORRECTIONS divided by all CHARS."
  (when (> chars 0)
    (let* ((a-chars (- correct-chars corrections))
           (a-chars (if (> a-chars 0) a-chars 0))
           (accuracy (* (/ a-chars (float chars)) 100.00)))
      accuracy)))

;;;; Performance results

(defvar monkeytype--run-performance-results-format
  (let* ((top1
          (propertize "Net WPM: %d" 'face 'monkeytype--header-2-face))
         (top2
          (propertize "\nAccuracy: %.2f%%" 'face 'monkeytype--header-2-face))
         (top3
          (propertize "\nTotal time: %s" 'face 'monkeytype--header-2-face))
         (bottom
          (propertize "\nNet CPM: %d
Gross WPM: %d
Gross CPM: %d
Total chars: %d
Total words: %d
Corrections: %d
Total errors: %d" 'face 'monkeytype--header-3-face)))
    (concat top1 top2 top3 bottom)))

(defun monkeytype--run-performance-results (run)
  "Performance results for RUN."
  (let* ((last-entry (elt run 0))
         (elapsed-seconds (ht-get last-entry 'elapsed-seconds))
         (elapsed-minutes (monkeytype--seconds-to-minutes elapsed-seconds))
         (entries (ht-get last-entry 'input-index))
         (errors (ht-get last-entry 'error-count))
         (corrections (ht-get last-entry 'correction-count))
         (words (monkeytype--words entries)))
    (format monkeytype--run-performance-results-format
            (monkeytype--net-wpm words errors elapsed-minutes)
            (monkeytype--accuracy entries (- entries errors) corrections)
            (format-seconds "%.2h:%z%.2m:%.2s" elapsed-seconds)
            (monkeytype--net-cpm entries errors elapsed-minutes)
            (monkeytype--gross-wpm words elapsed-minutes)
            (monkeytype--gross-cpm entries elapsed-minutes)
            entries
            words
            corrections
            (+ errors corrections))))

(defun monkeytype--final-performance-results ()
  "Final Performance results for all run(s).
Total time is the sum of all the last entries' elapsed-seconds from all runs."
  (let* ((runs-last-entry (mapcar (lambda (x) (elt (ht-get x 'entries ) 0)) monkeytype--run-list))
         (last-entry (elt runs-last-entry 0))
         (total-elapsed-seconds (apply '+  (mapcar (lambda (x) (ht-get x 'elapsed-seconds)) runs-last-entry)))
         (elapsed-minutes (monkeytype--seconds-to-minutes total-elapsed-seconds))
         (entries (ht-get last-entry 'input-index))
         (errors (ht-get last-entry 'error-count))
         (corrections (ht-get last-entry 'correction-count))
         (words (monkeytype--words entries)))
    (format monkeytype--run-performance-results-format
            (monkeytype--net-wpm words errors elapsed-minutes)
            (monkeytype--accuracy entries (- entries errors) corrections)
            (format-seconds "%.2h:%z%.2m:%.2s" total-elapsed-seconds)
            (monkeytype--net-cpm entries errors elapsed-minutes)
            (monkeytype--gross-wpm words elapsed-minutes)
            (monkeytype--gross-cpm entries elapsed-minutes)
            entries
            words
            corrections
            (+ errors corrections))))

;;;; Words

(defun monkeytype--get-words ()
  "Index words."
  (let* ((words (split-string monkeytype--source-text "[ \n]")))
    (setq index 1)
    (dolist (word words)
      (add-to-list 'monkeytype--words-list `(,index . ,word))
      (setq index (+ index 1)))))

(defun monkeytype--get-chars-to-words ()
  "Associate by index cars to words."
  (setq word-index 1)
  (setq char-index 1)
  (let* ((chars (mapcar 'char-to-string monkeytype--source-text)))
    (dolist (char chars)
      (if (string-match "[ \n\t]" char)
          (progn
            (setq word-index (+ word-index 1))
            (setq char-index (+ char-index 1)))
        (progn
          (let* ((word  (assoc word-index monkeytype--words-list))
                 (word (cdr word)))
            (add-to-list 'monkeytype--chars-to-words-list `(,char-index . ,word))
            (setq char-index (+ char-index 1))))))))

;;;; typed text

(defun monkeytype--run-typed-text (run)
  "Final Text for RUN."

  (monkeytype--get-words)
  (monkeytype--get-chars-to-words)

  (let* ((entries (seq-group-by
                   (lambda (entry) (ht-get entry 'source-index))
                   (reverse (ht-get run 'entries))))
         (final-text (mapconcat
                      (lambda (entries-for-source)
                        (let* ((tries (cdr entries-for-source))
                               (correctionsp (> (length tries) 1))
                               (settled (if correctionsp (car (last tries)) (car tries)))
                               (source (ht-get settled 'source-entry))
                               (newline (if (string= "\n" source) source ""))
                               (settled-correctp (= (ht-get settled 'state) 1))
                               (propertized-settled (propertize
                                                     (format "%s" (ht-get settled 'typed-entry))
                                                     'face
                                                     (monkeytype--final-text>typed-entry-face settled-correctp)))
                               (corrections (if correctionsp (butlast tries) nil)))
                          (if correctionsp
                              (let* ((propertized-corrections
                                      (mapconcat (lambda (correction)
                                                   (let* ((correction-char (ht-get correction 'typed-entry))
                                                          (state (ht-get correction 'state))
                                                          (correction-face (monkeytype--final-text>typed-entry-face (= state 1) t)))
                                                     (propertize (format "%s" correction-char) 'face correction-face)))
                                                 corrections
                                                 "")))
                                (format "%s%s%s" propertized-settled propertized-corrections newline))
                            (progn
                              (unless (= (ht-get settled 'state) 1)
                                (unless (string-match "[ \n\t]" (ht-get settled 'source-entry))
                                  (let* ((char-index (ht-get settled 'source-index))
                                         (mistyped-word (cdr (assoc char-index monkeytype--chars-to-words-list))))
                                    (add-to-list 'monkeytype--mistyped-words-list mistyped-word)))))
                            (format "%s%s" propertized-settled newline))))
                      entries
                      "")))
    (format "\n%s\n\n" final-text)))

(defun monkeytype--final-text>typed-entry-face (correctp &optional correctionp)
  "Return the face for the CORRECTP and/or CORRECTIONP entry."
  (let* ((entry-face (if correctionp
                 (if correctp 'monkeytype--correction-correct-face 'monkeytype--correction-error-face)
               (if correctp 'monkeytype--correct-face 'monkeytype--error-face))))
    entry-face))

;;;; Log:

(defun monkeytype--run-log (run)
  "Log for the RUN."
  (insert "Log:")
  (insert (monkeytype--run-log>header))
  (dotimes (i (length (ht-get  run 'entries)))
    (let* ((entries  (reverse (ht-get  run 'entries)))
           (entry (elt entries i)))
      (insert (monkeytype--run-log>entry entry))))
  (insert "\n\n"))

(defun monkeytype--run-log>header ()
  "Log header."
  (let ((log-header
         '(" I/S Idx "
           " S/T Chr "
           " N/WPM   "
           " N/CPM   "
           " G/WPM   "
           " G/CPM   "
           " Acc %   "
           " Time    "
           " Mends   "
           " Errs    ")))
    (format "\n|%s|" (mapconcat 'identity log-header "|"))))

(defun monkeytype--run-log>entry (entry)
  "Format ENTRY."
  (let* ((source-index (ht-get entry 'source-index))
         (typed-entry (ht-get entry 'typed-entry))
         (source-entry (ht-get entry 'source-entry))
         (typed-entry (if (string= typed-entry "\n") "↵" typed-entry))
         (source-entry (if (string= source-entry "\n") "↵" source-entry))
         (error-count (ht-get entry 'error-count))
         (correction-count (ht-get entry 'correction-count))
         (input-index (ht-get entry 'input-index))
         (state (ht-get entry 'state))
         (elapsed-seconds (ht-get entry 'elapsed-seconds))
         (elapsed-minutes (monkeytype--seconds-to-minutes elapsed-seconds))
         (typed-entry-face (monkeytype--final-text>typed-entry-face (= state 1)))
         (propertized-typed-entry (propertize (format "%S" typed-entry) 'face typed-entry-face)))
    (format "\n|%9s|%9s|%9d|%9d|%9d|%9d|%9.2f|%9s|%9d|%9d|"
            (format "%s %s" input-index source-index)
            (format "%S %s" source-entry propertized-typed-entry)
            (monkeytype--net-wpm (monkeytype--words input-index) error-count elapsed-minutes)
            (monkeytype--net-cpm input-index error-count elapsed-minutes)
            (monkeytype--gross-wpm (monkeytype--words input-index) elapsed-minutes)
            (monkeytype--gross-cpm input-index elapsed-minutes)
            (monkeytype--accuracy input-index (- input-index error-count) correction-count)
            (format-seconds "%.2h:%z%.2m:%.2s" elapsed-seconds)
            correction-count
            (+ error-count correction-count))))

;;;; Autoloads:

;;;###autoload
(defun monkeytype-region (start end)
  "Type marked region form START to END."
  (interactive "r")
  (monkeytype--setup (buffer-substring-no-properties start end)))

;;;###autoload
(defun monkeytype-repeat ()
  "Repeat run."
  (interactive)
  (monkeytype--setup monkeytype--source-text))

;;;###autoload
(defun monkeytype-dummy-text ()
  "Dummy text."
  (interactive)
  (monkeytype--setup "\"I have had a dream past the wit of man to say what dream it was,\"\nsays Bottom."))

;;;###autoload
(defun monkeytype-fortune ()
  "Type fortune."
  (interactive)
  (fortune)
  (monkeytype-buffer))

;;;###autoload
(defun monkeytype-buffer ()
  "Type entire current buffet."
  (interactive)
  (monkeytype--setup (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun monkeytype-pause ()
  "Pause run."
  (interactive)
  (when monkeytype--start-time (monkeytype--pause-run))
  (when (not monkeytype--finished)
    (message "Monkeytype: Paused ([C-c C-c r] to resume.)")))

;;;###autoload
(defun monkeytype-stop ()
  "Finish run."
  (interactive)
  (monkeytype--handle-complete))

;;;###autoload
(defun monkeytype-resume ()
  "Resume run."
  (interactive)
  (when (not monkeytype--finished)
    (progn
      (setq monkeytype--current-run-list '())
      (switch-to-buffer monkeytype--typing-buffer)
      (set-buffer-modified-p nil)
      (monkeytype--add-hooks)
      (monkeytype-mode)
      (setq buffer-read-only nil)
      (message "Monkeytype: Timer will start when you type the first character."))))

;;;###autoload
(defun monkeytype-mistyped-words ()
  "Practice mistyped words."
  (interactive)
  (if (> (length monkeytype--mistyped-words-list) 0)
      (let* ((text (mapconcat (lambda (word) (replace-regexp-in-string " " "" word))
                              monkeytype--mistyped-words-list " ")))
        (monkeytype--setup text))
    (message "Monkeytype: No errors. ([C-c C-c t] to repeat.)")))

;;;###autoload
(define-minor-mode monkeytype-mode
  "Monkeytype mode is a minor mode for speed/touch typing"
  :lighter " Monkeytype"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c p") 'monkeytype-pause)
            (define-key map (kbd "C-c C-c r") 'monkeytype-resume)
            (define-key map (kbd "C-c C-c s") 'monkeytype-stop)
            (define-key map (kbd "C-c C-c t") 'monkeytype-repeat)
            (define-key map (kbd "C-c C-c f") 'monkeytype-fortune)
            (define-key map (kbd "C-c C-c m") 'monkeytype-mistyped-words)
            map))

(provide 'monkeytype)

;;; monkeytype.el ends here
