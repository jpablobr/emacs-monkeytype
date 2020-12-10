;;; monkeytype--calc-test.el --- Monkeytype: tests  -*- lexical-binding: t; -*-

(ert-deftest monkeytype-test-calc-gross-wpm ()
  "Gross WPM: words / minutes"
  ;; 1 word / 1 minute
  (should (= 1 (monkeytype--calc-gross-wpm 1 1)))
  ;; 5 word / 1 minute
  (should (= 5 (monkeytype--calc-gross-wpm 5 1)))
  ;; 5.5 words / 1 minute
  (should (= 5.5 (monkeytype--calc-gross-wpm 5.5 1)))
  ;; 5 words / 1.1 minute
  (should (= 4.545454545454545 (monkeytype--calc-gross-wpm 5 1.1)))
  ;; 1.0 words / 5.0 minutes
  (should (= 0.2 (monkeytype--calc-gross-wpm 1.0 5.0 ))))

(ert-deftest monkeytype-test-calc-gross-cpm ()
  "Gross CPM: chars / minutes"
  ;; 5 chars / 1 minute
  (should (= 5 (monkeytype--calc-gross-cpm 5 1))))

(ert-deftest monkeytype-test-calc-net-wpm ()
  "Gross WPM: gross-wpm - (errors / minutes)"
  ;; 5 words, 1 error, 1 minute
  (should (= 4 (monkeytype--calc-net-wpm 5 1 1)))
  ;; 5 words, 5 error, 1 minute
  (should (= 0 (monkeytype--calc-net-wpm 5 5 1)))
  ;; 5.5 words, 1 error, 1 minute
  (should (= 4.5 (monkeytype--calc-net-wpm 5.5 1 1)))
  ;; 5 words, 1 error, 1.1 minute
  (should (= 3.636363636363636 (monkeytype--calc-net-wpm 5 1 1.1)))
  ;; 5 words, 6 errors, 1 minute
  ;; More errors than words should return 0 instead of negative
  ;; This is given that a single word can have up to 5 errors.
  (should (= 0 (monkeytype--calc-net-wpm 5 6 1))))

(ert-deftest monkeytype-test-calc-net-cpm ()
  "Gross CPM: gross-wpm - (errors / minutes)"
  ;; 5 chars, 1 error, 1 minute
  (should (= 4 (monkeytype--calc-net-cpm 5 1 1)))
  ;; 5 chars, 5 error, 1 minute
  (should (= 0 (monkeytype--calc-net-cpm 5 5 1)))
  ;; 5.5 chars, 1 error, 1 minute
  (should (= 4.5 (monkeytype--calc-net-cpm 5.5 1 1)))
  ;; 5 chars, 1 error, 1.1 minute
  (should (= 3.636363636363636 (monkeytype--calc-net-cpm 5 1 1.1)))
  ;; 5 chars, 6 errors, 1 minute
  ;; This can happen on re-mistyping multiple times
  (should (= 0 (monkeytype--calc-net-wpm 5 6 1))))

(ert-deftest monkeytype-test-calc-accuracy ()
  "Accuracy: all-correct-chars - (corrections / all-chars)"
  ;; 5 chars, 5 correct-chars, 0 corrections
  (should (= 100.0 (monkeytype--calc-accuracy 5 5 0)))
  ;; 5 chars, 1 correct-chars, 0 corrections
  (should (= 20.0 (monkeytype--calc-accuracy 5 1 0)))
  ;; 5 chars, 0 correct-chars, 1 corrections
  (should (= 0.0 (monkeytype--calc-accuracy 5 0 1)))
  ;; 5 chars, 5 correct-chars, 5 corrections
  (should (= 0.0 (monkeytype--calc-accuracy 5 5 5)))
  ;; 5 chars, 5 correct-chars, 6 corrections
  (should (= 0.0 (monkeytype--calc-accuracy 5 5 6))))

(provide 'monkeytype-tests)

;;; monkeytype--calc-test.el ends here
