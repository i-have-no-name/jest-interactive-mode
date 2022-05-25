;;; jest-interactive-mode.el --- Jest interactive mode  -*- lexical-binding: t; -*-

;; URL: https://github.com/i-have-no-name/jest-interactive-mode
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; Highlight jest results in window

;;; Code:

(require 'json)
(require 'filenotify)

(defun jest-interactive--resize-window (new-size &optional horizontal)
  "Sets the current window's with or height to `new-size'."
  (let ((wincfg (current-window-configuration))
        (nwins (length (window-list)))
        (count (if horizontal
                   (- new-size
                      (window-width))
                 (- new-size
                    (window-height)))))
    (catch 'done
      (save-window-excursion (while (not (zerop count))
                               (if (> count 0)
                                   (progn
                                     (enlarge-window 1 horizontal)
                                     (setq count (1- count)))
                                 (progn
                                   (shrink-window 1 horizontal)
                                   (setq count (1+ count))))
                               (if (= nwins (length (window-list)))
                                   (setq wincfg (current-window-configuration))
                                 (throw 'done t)))))
    (set-window-configuration wincfg)))

;; strip unnecessary whitespaces and ansi
(defun jest-interactive--normalize-error-message (str)
  (replace-regexp-in-string "\s+"
                            " "
                            (replace-regexp-in-string "\x1b\[[0-9;]*m"
                                                      "" str)))

(defun jest-interactive--get-error-message (msg)
  (let* ((lines (split-string msg "\n"))
         (expected (jest-interactive--normalize-error-message (nth 2 lines)))
         (received (jest-interactive--normalize-error-message (nth 3 lines))))
    (concat expected ", " received)))

(defun jest-interactive--set-expect-error (msg)
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
                                                           (jest-interactive--get-error-message msg))
                                                   'face
                                                   '(error)))
                          (overlay-put ov
                                       'before-string
                                       (propertize "x"
                                                   'display
                                                   '(left-fringe filled-rectangle error)))))))

(defun jest-interactive--set-success-suite (ov)
  (overlay-put ov
               'before-string
               (propertize "x"
                           'display
                           '(left-fringe filled-rectangle success))))

(defun jest-interactive--set-failure-suite (ov msg)
  (overlay-put ov
               'before-string
               (propertize "x"
                           'display
                           '(left-fringe filled-rectangle error)))
  (jest-interactive--set-expect-error msg))

(defun jest-interactive--create-empty-file-if-no-exists (filePath)
  (unless (file-exists-p filePath)
    (with-temp-buffer
      (write-file filePath)))
  (write-region "{\"testResults\": [{\"assertionResults\": []}] }"
                nil filePath))

(defun jest-interactive--line-jump (e)
  (interactive)
  (let ((line (get-text-property (point)
                                 `jest-interactive-error-line)))
    (pop-to-buffer jest-interactive-main-buffer)
    (goto-line line)))

;; TODO: change opened to function with visible state
(defun jest-interactive--display-error-window ()
  (setq jest-interactive-errors-buffer-opened
        t)
  (setq jest-interactive-errors-buffer (get-buffer-create "*jest-interactice-errors-buffer*"))
  (setq jest-interactive-main-buffer (current-buffer))
  (let* ((errors-lm-list errors-line-message-list)
         (max-line-number (car (car (last errors-lm-list))))
         (line-string "line %s: ")
         (line-longest-string (format line-string max-line-number)))
    (with-current-buffer jest-interactive-errors-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (jest-interactive--list-errors-mode)
        (dolist (line-message-list errors-lm-list)
          (let* ((line (car line-message-list))
                 (msg (car (cdr line-message-list)))
                 (line-format-string (format line-string line)))
            (insert-text-button (propertize (concat (s-pad-right (string-width line-longest-string)
                                                                 " "
                                                                 line-format-string)
                                                    msg)
                                            'face
                                            '(error :underline t))
                                'action
                                'jest-interactive--line-jump
                                'follow-link
                                "\C-m"
                                'jest-interactive-error-line
                                line)
            (let ((last_line (car (car (last errors-lm-list)))))
              (unless (eq last_line line)
                (insert "\n"))))))))
  ;; open window with errors only if it's not visible
  (when (and jest-interactive-errors-buffer-opened
             (not (get-buffer-window jest-interactive-errors-buffer)))
    (pop-to-buffer jest-interactive-errors-buffer
                   '(display-buffer-at-bottom . ()))
    (goto-line 1)
    (jest-interactive--resize-window 13)
    (setq mode-line-format nil)
    (hl-line-mode)
    (setq buffer-read-only t)))

(defun jest-interactive--modeline-propertize (text &rest rest)
  (propertize (concat " "
                      (apply #'propertize text rest))))

(defun jest-interactive--modeline (num_failed_tests)
  (when (and num_failed_tests
             (boundp 'doom-modeline-mode))
    (setq global-mode-string jest-interactive--modeline-string)
    (if (eq num_failed_tests 0)
        (add-to-list 'global-mode-string
                     (jest-interactive--modeline-propertize "jest"
                                                            'face
                                                            '(success 'bold))
                     'APPEND)
      (add-to-list 'global-mode-string
                   (jest-interactive--modeline-propertize (format "jest(failed:%s)" num_failed_tests)
                                                          'face
                                                          '(error 'bold)
                                                          'mouse-face
                                                          'mode-line-highlight
                                                          'local-map
                                                          (make-mode-line-mouse-map 'mouse-1 'jest-interactive-display-list-errors))
                   'APPEND))))

(defun jest-interactive--modeline-init ()
  (when (boundp 'doom-modeline-mode)
    (setq global-mode-string jest-interactive--modeline-string)
    (add-to-list 'global-mode-string
                 (jest-interactive--modeline-propertize "jest(initializing)"
                                                        'face
                                                        '(success 'bold))
                 'APPEND)))

(defun jest-interactive--run ()
  (message "jest-interactive-mode processing")
  (setq errors-line-message-list '())
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
          (let ((position (search-forward-regexp (format "\\(test(\\(\'\\|\"\\)%s\\)\\|\\(it(\\(\'\\|\"\\)%s\\)"
                                                         title title)
                                                 nil
                                                 t)))
            (when position
              (end-of-line)
              (let ((ov (make-overlay position
                                      (point)
                                      nil
                                      t)))
                (push ov overlays)
                (if (string= status "passed")
                    (jest-interactive--set-success-suite ov)
                  (progn
                    (let ((msg (thing-at-point 'line))
                          (line (line-number-at-pos)))
                      (add-to-list 'errors-line-message-list
                                   (list line
                                         (string-trim msg))
                                   t))
                    (jest-interactive--set-failure-suite ov
                                                         (car msgs))))))))))
    (jest-interactive--modeline num_failed_tests)
    (when (and asserts
               (boundp 'jest-interactive-errors-buffer)
               (get-buffer-window jest-interactive-errors-buffer
                                  'visible)
               (boundp 'jest-interactive-main-buffer))
      (jest-interactive--display-error-window))))

(defun jest-interactive--file-name ()
  (file-name-nondirectory (buffer-file-name)))

(defun jest-interactive--create-results-file-name ()
  (concat "."
          (jest-interactive--file-name)
          ".results.json"))

(defun jest-interactive--hide-list-errors ()
  (when (and jest-interactive-errors-buffer
             (get-buffer-window jest-interactive-errors-buffer))
    (delete-window (get-buffer-window jest-interactive-errors-buffer))
    (kill-buffer jest-interactive-errors-buffer))
  (setq jest-interactive-errors-buffer-opened
        nil)
  (setq jest-interactive-errors-buffer nil))

(defun jest-interactive--open ()
  (interactive)
  (message "jest-interactive-mode enabled")
  (setq jest-interactive--modeline-string global-mode-string)
  (setq jest-interactive-errors-buffer nil)
  (setq jest-interactive-errors-buffer-opened
        nil)
  (jest-interactive--modeline-init)
  (setq overlays '())
  (setq jest-interactive--results-file-name (concat (file-name-directory buffer-file-name)
                                                    (jest-interactive--create-results-file-name)))
  (jest-interactive--create-empty-file-if-no-exists
   jest-interactive--results-file-name)
  (setq fd (file-notify-add-watch jest-interactive--results-file-name
                                  '(change)
                                  (lambda (event)
                                    (jest-interactive--run))))
  (setq process (make-comint-in-buffer (concat (jest-interactive--file-name)
                                               ".jest-process")
                                       nil
                                       "yarn"
                                       nil
                                       "jest"
                                       (buffer-file-name)
                                       "--json"
                                       (format "--outputFile=%s" jest-interactive--results-file-name)
                                       "--runInBand"
                                       "--watch"))
  (jest-interactive--run))

(defun jest-interactive--close ()
  (file-notify-rm-watch fd)
  (jest-interactive--hide-list-errors)
  (delete-process process)
  (kill-buffer process)
  (delete-file jest-interactive--results-file-name)
  (dolist (ov overlays)
    (delete-overlay ov))
  (setq global-mode-string jest-interactive--modeline-string)
  (message "jest-interactive-mode disabled"))

(defun jest-interactive-display-list-errors ()
  (interactive)
  (jest-interactive--display-error-window))


(defvar jest-interactive--keymap ()
  "jest interactive keymap")

(setq jest-interactive--keymap (make-sparse-keymap))

(define-minor-mode jest-interactive-mode
  "jest interactive mode"
  :ligher "jest-interactive"
  :keymap jest-interactive--keymap
  (if jest-interactive-mode
      (jest-interactive--open)
    (jest-interactive--close)))

(defvar jest-interactive--list-errors-keymap ()
  "jest interactive list errors keymap")

(setq jest-interactive--list-errors-keymap (make-sparse-keymap))

(define-minor-mode jest-interactive--list-errors-mode
  "jest interactive list errors mode"
  :ligher "jest-interactive-list-errors"
  :keymap jest-interactive--list-errors-keymap
  (progn
    (if jest-interactive--list-errors-mode
        (jest-interactive--bind-keys)
      (jest-interactive--unbind-keys))))

(defun jest-interactive--close-list-errors ()
  (interactive)
  (jest-interactive--unbind-keys)
  (jest-interactive--hide-list-errors))


(defun jest-interactive--bind-keys ()
  (when evil-mode
    (evil-define-key 'normal
      jest-interactive--keymap
      [escape]
      'jest-interactive--close-list-errors)
    (evil-define-key 'normal
      jest-interactive--list-errors-keymap
      [escape]
      'jest-interactive--close-list-errors)
    (evil-define-key 'normal jest-interactive--list-errors-keymap
      "q" 'jest-interactive--close-list-errors)))

(defun jest-interactive--unbind-keys ()
  (when evil-mode
    (evil-define-key 'normal
      jest-interactive--keymap
      [escape]
      nil)
    (evil-define-key 'normal
      jest-interactive--list-errors-keymap
      [escape]
      nil)
    (evil-define-key 'normal jest-interactive--list-errors-keymap
      "q" nil)))

(provide 'jest-interactive-mode)

;;; jest-interactive-mode.el ends here
