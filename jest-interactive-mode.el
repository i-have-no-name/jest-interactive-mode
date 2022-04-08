;;; jest-interactive-mode.el --- Jest interactive mode  -*- lexical-binding: t; -*-

;; URL: https://github.com/i-have-no-name/jest-interactive-mode
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; Put a description of the package here

;;; Code:

(require 'json)
(require 'filenotify)

(defun jest-interactive--replace-msg (str)
  (replace-regexp-in-string "\s+"
                            " "
                            (replace-regexp-in-string "\x1b\[[0-9;]*m"
                                                      "" str)))

(defun jest-interactive--get_message (msg)
  (let* ((lines (split-string msg "\n"))
         (expected (jest-interactive--replace-msg (nth 2 lines)))
         (received (jest-interactive--replace-msg (nth 3 lines))))
    (concat expected ", " received)))

(defun jest-interactive--expect_message (msg)
  (save-match-data (and (string-match "\(\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)\)"
                                      msg)
                        (let* ((line (match-string 2 msg))
                               (_ (goto-line (string-to-number line)))
                               (p1 (point))
                               (__ (end-of-line))
                               (p2 (point))
                               (ov (make-overlay p1 p2 nil t)))
                          (push ov overlays)
                          (overlay-put ov
                                       'after-string
                                       (propertize (concat " "
                                                           (jest-interactive--get_message msg))
                                                   'face
                                                   '(error)))
                          (overlay-put ov
                                       'before-string
                                       (propertize "x"
                                                   'display
                                                   '(left-fringe filled-rectangle error)))))))


(defun jest-interactive--success (ov)
  (overlay-put ov
               'before-string
               (propertize "x"
                           'display
                           '(left-fringe filled-rectangle success))))

(defun jest-interactive--failure (ov msg)
  (overlay-put ov
               'before-string
               (propertize "x"
                           'display
                           '(left-fringe filled-rectangle error)))
  (jest-interactive--expect_message msg))

(defun jest-interactive--create-empty-file-if-no-exists (filePath)
  (unless (file-exists-p filePath)
    (with-temp-buffer
      (write-file filePath)))
  (write-region "{\"testResults\": [{\"assertionResults\": []}] }"
                nil filePath))

(defun jest-interactive--modeline (num_failed_tests)
  (when (and num_failed_tests
             (boundp 'doom-modeline-mode))
    (setq global-mode-string jest-interactive--modeline-string)
    (if (eq num_failed_tests 0)
        (add-to-list 'global-mode-string
                     (propertize "jest"
                                 'face
                                 '(success 'bold))
                     'APPEND)
      (add-to-list 'global-mode-string
                   (propertize (format "jest(failed:%s)" num_failed_tests)
                               'face
                               '(error 'bold))
                   'APPEND))))

(defun jest-interactive--run ()
  (message "jest-interactive-mode processing")
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file jest-interactive--results-file-name))
         (num_failed_tests (gethash "numFailedTests" json))
         (results (gethash "testResults" json))
         (head (car results))
         (asserts (gethash "assertionResults" head)))
    (unless (null overlays)
      (dolist (ov overlays)
        (delete-overlay ov)))
    (dolist (assert asserts)
      (let* ((title (gethash "title" assert))
             (status (gethash "status" assert))
             (msgs (gethash "failureMessages" assert)))
        (save-excursion
          (goto-char 1)
          (let ((position (search-forward-regexp (format "test('%s'\\|it('%s'" title title))))
            (end-of-line)
            (let ((ov (make-overlay position
                                    (point)
                                    nil
                                    t)))
              (push ov overlays)
              (if (string= status "passed")
                  (jest-interactive--success ov)
                (jest-interactive--failure ov
                                           (car msgs))))))))
    (jest-interactive--modeline num_failed_tests)))

(defun jest-interactive--open ()
  (interactive)
  (message "jest-interactive-mode enabled")
  (set (make-local-variable 'jest-interactive--modeline-string)
       global-mode-string)
  (set (make-local-variable 'overlays)
       '())
  (set (make-local-variable 'jest-interactive--results-file-name)
       (concat (file-name-directory buffer-file-name)
               "__jest_interactive_mode_results__.json"))
  (jest-interactive--create-empty-file-if-no-exists
   jest-interactive--results-file-name)
  (set (make-local-variable 'fd)
       (file-notify-add-watch jest-interactive--results-file-name
                              '(change)
                              (lambda (event)
                                (jest-interactive--run))))
  (set (make-local-variable 'process)
       (make-comint-in-buffer "jest-interactive"
                              nil
                              "yarn"
                              nil
                              "jest"
                              (buffer-file-name)
                              "--json"
                              (format "--outputFile=%s" jest-interactive--results-file-name)
                              "--watch"))
  (jest-interactive--run))

(defun jest-interactive--close ()
  (delete-process process)
  (kill-buffer process)
  (file-notify-rm-watch fd)
  (delete-file jest-interactive--results-file-name)
  (dolist (ov overlays)
    (delete-overlay ov))
  (setq global-mode-string jest-interactive--modeline-string)
  (message "jest-interactive-mode disabled"))

(define-minor-mode jest-interactive-mode
  "jest interactive mode"
  :ligher "jest-interactive"
  (if jest-interactive-mode
      (jest-interactive--open)
    (jest-interactive--close)))


(provide 'jest-interactive-mode)

;;; jest-interactive-mode.el ends here
