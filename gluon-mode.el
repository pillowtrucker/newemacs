(defvar gluon-font-lock-keywords
  (let* (
         ;; Define patterns for basic syntax groups from Vim syntax file
         (x-conditional '("match" "if" "else" "with" "then"))
         (x-typedef '("type"))
         (x-keyword '("do" "let" "in" "and" "forall"))
         (x-boolean '("True" "False"))
         (x-todo '("TODO" "FIXME" "NOTE"))
         (x-types '("\\<[A-Z][A-Za-z_0-9]*\\>"))
         (x-macro '("\\w\\+!"))
         (x-number '("\\_<\\(?:\\d+\\.?\\d*\\|0x[[:xdigit:]]+\\|0b[01]+\\|0o[0-7]+\\)\\_>"))
         (x-operator '("[!#$%&*+\\-./<=>?@\\\\^|~:]+"))
         (x-separator '("\\(|\\)"))
         (x-escape '("\\\\[nrt0\\'\"x\\{2}\\]"))
         (x-string '("\"\\(?:[^\"\\]\\|\\\\.\\)*\""))
         (x-character '("'\\(?:[^\\']\\|\\\\.\\)*'"))
         ;; Build the list
         (patterns (append x-conditional x-typedef x-keyword x-boolean x-todo x-types x-macro x-number x-operator x-separator x-escape x-string x-character))
        )
    `(
      (,(regexp-opt x-conditional 'words) . font-lock-keyword-face)
      (,(regexp-opt x-typedef 'words) . font-lock-type-face)
      (,(regexp-opt x-keyword 'words) . font-lock-keyword-face)
      (,(regexp-opt x-boolean 'words) . font-lock-constant-face)
      (,(regexp-opt x-todo 'words) . font-lock-warning-face)
      (,x-types . font-lock-type-face)
      (,x-macro . font-lock-preprocessor-face)
      (,x-number . font-lock-constant-face)
      (,x-operator . font-lock-builtin-face)
      (,x-separator . font-lock-builtin-face)
      (,x-escape . font-lock-warning-face)
      (,x-string . font-lock-string-face)
      (,x-character . font-lock-string-face)
      ;; Add more patterns and faces here
      ))
  "Highlighting expressions for Gluon mode")

(defun gluon-calculate-indentation ()
  "Calculate the indentation for the current line in Gluon mode."
  (save-excursion
    (beginning-of-line)
    (if (bobp)  ; Check if it's the beginning of the buffer
        0       ; If so, no indentation
      (let ((cur-indent nil) (indent-increase t) (sw (if indent-tabs-mode tab-width standard-indent)))
        (skip-chars-backward "\n\r\t ")
        (when (not (bobp))  ; Check if it's not the beginning of the buffer
          (backward-char)  ; Move to the end of the previous line
          (if (looking-at "[})]") ; Check if the line starts with '}' or ')'
              (setq indent-increase nil))
          (forward-line 1)
          (let* ((line-end (line-end-position))
                 (line-start (line-beginning-position))
                 (line (buffer-substring-no-properties line-start line-end)))
            (if (string-match-p "\\(=\\|{\\|(\\|\\[\\|->\\|then\\|else\\|with\\)\\s-*$" line)
                (setq indent-increase t))))
        (forward-line -1)
        (setq cur-indent (current-indentation))
        (if indent-increase
            (+ cur-indent sw)
          (- cur-indent sw))))))

(defun gluon-indent-line ()
  "Indent current line in Gluon mode."
  (interactive)
  (let ((indent-level (gluon-calculate-indentation)))
    (if (< indent-level 0) (setq indent-level 0))
    (indent-line-to indent-level)))

(defun gluon-mode-setup-indentation ()
  "Set up indentation function for Gluon mode."
  (setq-local indent-line-function 'gluon-indent-line))

(define-derived-mode gluon-mode prog-mode "Gluon"
  "Major mode for editing Gluon language code."
  (setq font-lock-defaults '((gluon-font-lock-keywords)))
  (gluon-mode-setup-indentation)
  ;; Additional setup code can be added here
  )

;; Add to the list of automatically detected modes
(add-to-list 'auto-mode-alist '("\\.glu\\'" . gluon-mode))

(provide 'gluon-mode)
