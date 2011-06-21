(require 'font-lock)

(defvar thrift-mode-hook nil)

(defvar (setq thrift-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c\C-k" 'thrift-gen)
    map))
  "Keymap for THRIFT major mode")

(defconst thrift-font-lock-keywords
  (list
   '("#.*$" . font-lock-comment-face)  ;; perl style comments
   '("\\<\\(include\\|struct\\|union\\|namespace\\|exception\\|typedef\\|php_namespace\\|const\\|enum\\|service\\|extends\\|void\\|async\\|throws\\|optional\\|required\\)\\>" . font-lock-keyword-face)  ;; keywords
   '("\\<\\(b\\(?:inary\\|ool\\|yte\\)\\|double\\|i\\(?:16\\|32\\|64\\)\\|list\\|map\\|s\\(?:et\\|tring\\)\\)\\>" . font-lock-type-face)  ;; built-in types
   '("\\<\\(cpp\\|java\\|p\\(?:erl\\|hp\\|y\\)\\)" . font-lock-builtin-face)  ;; langs
   '("\\<\\([A-Z]\\w*\\)\\>" . font-lock-type-face)   ;; typenames (unions & structs)
   '("\\<\\([0-9]+\\)\\>" . font-lock-variable-name-face)   ;; ordinals
   '("\\<\\(\\w+\\)\\s-*(" (1 font-lock-function-name-face))  ;; functions
   )
  "Thrift Keywords")

(defun thrift-mode ()
  "Mode for editing Thrift files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table thrift-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(thrift-font-lock-keywords))
  (setq major-mode 'thrift-mode)
  (setq mode-name "Thrift")
  (run-hooks 'thrift-mode-hook)
  (set (make-local-variable 'indent-line-function) 'thrift-indent-line)
  (use-local-map thrift-mode-map)
  )

(defvar thrift-indent-level 4
  "Defines 4 spaces for thrift indentation.")

;; indentation
(defun thrift-indent-line ()
  "Indent current line as Thrift code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\(}\\|throws\\)")
          (if (looking-at "^[ \t]*}")
              (progn
                (save-excursion
                  (forward-line -1)
                  (setq cur-indent (- (current-indentation) thrift-indent-level)))
                (if (< cur-indent 0)
                    (setq cur-indent 0)))
            (progn
              (save-excursion
                (forward-line -1)
                (if (looking-at "^[ \t]*[\\.<>[:word:]]+[ \t]+[\\.<>[:word:]]+[ \t]*(")
                    (setq cur-indent (+ (current-indentation) thrift-indent-level))
                  (setq cur-indent (current-indentation))))))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*}")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^.*{[^}]*$")
                  (progn
                    (setq cur-indent (+ (current-indentation) thrift-indent-level))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              (if (looking-at "^[ \t]*throws")
                  (progn
                    (setq cur-indent (- (current-indentation) thrift-indent-level))
                    (if (< cur-indent 0)
                        (setq cur-indent 0))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              (if (looking-at "^[ \t]*[\\.<>[:word:]]+[ \t]+[\\.<>[:word:]]+[ \t]*([^)]*$")
                  (progn
                    (setq cur-indent (+ (current-indentation) thrift-indent-level))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              (if (looking-at "^[ \t]*\\/\\*")
                  (progn
                    (setq cur-indent (+ (current-indentation) 1))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              (if (looking-at "^[ \t]*\\*\\/")
                  (progn
                    (setq cur-indent (- (current-indentation) 1))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              ))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

;; C/C++ comments; also allowing underscore in words
(defvar thrift-mode-syntax-table
  (let ((thrift-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" thrift-mode-syntax-table)
    (modify-syntax-entry ?/ ". 124b" thrift-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" thrift-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" thrift-mode-syntax-table)
    thrift-mode-syntax-table)
  "Syntax table for thrift-mode")

;;
;; Imenu
;;
(defvar thrift-imenu-generic-expression
  '(("service"   "^service \\(.*\\) ?{" 1))
  "Imenu Regexps for thrift mode")

(add-hook 'thrift-mode-hook '(lambda ()
                               (setq imenu-generic-expression
                                     thrift-imenu-generic-expression)))

;;

(defun thrift-gen ()
  "Generate the relevant thrift code"
  (interactive)
  (message "hi")
  (let ((cmd (concat "thrift --gen py "
                         (expand-file-name buffer-file-name))))
    (message cmd)
    (shell-command cmd))
  (message (concat "Generating " buffer-file-name)))

(provide 'thrift-mode)