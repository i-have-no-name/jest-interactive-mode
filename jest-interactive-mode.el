;;; jest-interactive-mode --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
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

;; Put a description of the package here

;;; Code:

(require 'json)
(require 'filenotify)

(defun jim/open()
  (interactive)
  (set (make-local-variable 'overlays)
       '())
  (set (make-local-variable 'jim/results-file-name) (concat (file-name-directory buffer-file-name) "__jest_interactive_mode_results__.json"))

  (defun jim/run()
    (defun get_message(msg)
(defun jest-interactive--replace-msg(str)
(replace-regexp-in-string "\s+" " " ( replace-regexp-in-string "\x1b\[[0-9;]*m" "" str )))
(let* ((lines (split-string msg "\n"))
(expected ( jest-interactive--replace-msg (nth 2 lines) ))
(received ( jest-interactive--replace-msg (nth 3 lines) )))
(concat expected ", " received)))
    (defun expect_message(msg)
(save-match-data (and (string-match "\(\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)\)" msg)
(let ((line (match-string 2 msg)))
(goto-line (string-to-number line))
(let ((p1 (point)))
(end-of-line)
(let ((p2 (point)))
(let ((ov (make-overlay p1 p2 nil t)))
(push ov overlays)
(overlay-put ov 'after-string (propertize (concat " " (get_message msg)) 'face
'(error)))
(overlay-put ov 'before-string (propertize "x" 'display '(left-fringe filled-rectangle error))))))))))
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (json (json-read-file jim/results-file-name))
           (results (gethash "testResults" json))
           (head (car results))
           (asserts (gethash "assertionResults" head)))
      (unless (null overlays)
        (dolist (tm overlays)
          (delete-overlay tm)))
      (dolist (assert asserts)
(let* ((title (gethash "title" assert))
(status (gethash "status" assert))
(msgs (gethash "failureMessages" assert)))
(defun success(ov)
(overlay-put ov 'before-string (propertize "x" 'display '(left-fringe filled-rectangle success))))
(defun failure(ov msg)
(overlay-put ov 'before-string (propertize "x" 'display '(left-fringe filled-rectangle error)))
;; (overlay-put ov 'after-string (propertize " failure" 'face '(error)))
            (expect_message msg))
(save-excursion (goto-char 1)
(let ((position (search-forward-regexp (format "test('%s'\\|it('%s'" title title))))
(end-of-line)
(let ((ov (make-overlay position (point) nil t)))
(push ov overlays)
(if (string= status "passed")
(success ov)
(failure ov (car msgs))))))))))
  (defun jim/create-empty-file-if-no-exists(filePath)
(if (file-exists-p filePath)
()
(with-temp-buffer (write-file filePath)))
(write-region "{\"testResults\": [{\"assertionResults\": []}] }" nil filePath))
  (jim/create-empty-file-if-no-exists jim/results-file-name)
  (set (make-local-variable 'fd)
       (file-notify-add-watch jim/results-file-name '(change)
                              (lambda (event)
                                (jim/run))))
  (set (make-local-variable 'process)
       (make-comint-in-buffer "jest-interactive" nil "yarn" nil "jest" (buffer-file-name) "--json"
                              (format "--outputFile=%s" jim/results-file-name)
                               "--watch"))
  (jim/run))

(defun jim/close()
  (delete-process process)
  (kill-buffer process)
  (file-notify-rm-watch fd)
  (delete-file jim/results-file-name)
  (dolist (tm overlays)
    (delete-overlay tm)))

(define-minor-mode jest-interactive-mode "jest interactive mode"
  :ligher "jest-interactive"
  (if jest-interactive-mode (jim/open)
    (jim/close)))


(provide 'jest-interactive-mode)

;;; jest-interactive-mode.el ends here
