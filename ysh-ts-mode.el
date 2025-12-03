;;; ysh-ts-mode.el --- Tree-sitter support for YSH  -*- lexical-binding: t; -*-

(require 'treesit)

(setq ysh-ts-mode-font-lock-rules
  '(;; ysh font locking
    :language ysh
    :override t
    :feature command
    (
     ;; ((function_definition (function_name (variable_name))) @font-lock-function-name-face)
     (function_name (variable_name)) @font-lock-function-name-face
     (proc_definition (proc_name)) @font-lock-function-name-face
     )
    ;; (function_definition name: (identifier) @font-lock-function-name-face))

    :language ysh
    :override t
    :feature string
    ((string) @font-lock-string-face)
    
    :language ysh
    :override t
    :feature variable
     ((variable_name) @font-lock-variable-name-face)
     ;; ((variable_declaration variable: (variable_name) @font-lock-variable-name-face))

    :language ysh
    :override t
    :feature keyword
    (["proc" "func" "var" "const" "setvar" "setglobal" "if" "else" "while"
       "case" "for" "return"] @font-lock-keyword-face)

    :language ysh
    :override t
    :feature number
    ((number) @font-lock-number-face)

    :language ysh
    :override t
    :feature operator
    (["=" "+=" "-=" "===" "<" ">" "<=" ">=" "&&" "||" "|" "&" "/"] @font-lock-operator-face
     ["." "," ";" ":" "@" "$"] @font-lock-punctuation-face)
    ;; "!=" "==" is broken what else?
    ;; (["=" "+=" "-=" "==" "!=" "===" "<" ">" "<=" ">=" "&&" "||"] @font-lock-operator-face)

    :language ysh
    :override t
    :feature comment
    ((comment) @font-lock-comment-face)

    ))
  ;; "Tree-sitter font-lock settings for `ysh-ts-mode`.")

(defun ysh-ts-mode--anchor-string-user-controlled (node parent _ &rest _)
  "Return the anchor point for string content.
   Logic: Use the previous line's indentation, UNLESS it is shallower
   than the start of the string/command (the parent).
   In that case, clamp it to the parent's indentation."
  (save-excursion
    (let* ((parent-bol (progn 
                         (goto-char (treesit-node-start parent))
                         (line-beginning-position)))
           (parent-indent (current-indentation))
           
           ;; Move to previous line to measure it
           (prev-bol (progn 
                       (forward-line -1) 
                       (line-beginning-position)))
           (prev-indent (current-indentation)))
      
      ;; If previous line is shallower than parent, snap to parent.
      ;; Otherwise, trust the user's previous line.
      (if (< prev-indent parent-indent)
          parent-bol
        prev-bol))))

(setq ysh-ts-mode-indent-rules
      `((ysh

         ((match "'''" "string") ysh-ts-mode--anchor-string-user-controlled 0)
         ;; ((node-is "string") prev-line 0)
         ;; ((match "}" "proc_block" nil) parent-bol 0)
         ((match "}" "block" nil) parent-bol 0)
         
         ;; ((node-is "}") grandparent-bol 0)
         ((parent-is "proc_block") parent-bol 2)
         ((parent-is "block") parent-bol 2)
         ((parent-is "do_group") parent-bol 2)
         ((parent-is "if_statement") parent-bol 2)
         ((parent-is "for_statement") parent-bol 2)
         ((parent-is "while_statement") parent-bol 2)
         ((parent-is "case_statement") parent-bol 2)
         ((parent-is "function_definition") parent-bol 2)
         ((parent-is "proc_definition") parent-bol 2)
         ((parent-is "argument_list") parent-bol 2)
         ((parent-is "list") parent-bol 2)

         (no-node parent-bol 0))))
  ;; "Tree-sitter indentation rules for `ysh-ts-mode`.")

;; (defun ysh-ts-imenu-node-p (node)
;;   "Return t if NODE is a valid imenu node."
;;   (and (string-match-p "^h[0-6]$" (treesit-node-text node))
;;        (equal (treesit-node-type (treesit-node-parent node))
;;               "start_tag")))

(defun ysh-ts-imenu-node-p (node)
  "Return t if NODE is a valid imenu node for (proc_definition \"proc\") 
   or (function_name (variable_name))."
  (or
   (and (equal (treesit-node-type node) "proc_name")
        (equal (treesit-node-type (treesit-node-parent node)) "proc_definition"))
        ;; (equal (treesit-node-type (treesit-node-parent (treesit-node-parent node)))))
   (and (equal (treesit-node-type node) "variable_name")
        (equal (treesit-node-type (treesit-node-parent node)) "function_name")
        (equal (treesit-node-type (treesit-node-parent (treesit-node-parent node))) "function_definition"))))

(defun ysh-ts-imenu-name-function (node)
  "Return the name of the imenu entry for NODE, formatted as TYPE / NAME."
  (let ((name (treesit-node-text node))
        (node-type (treesit-node-type node)))
    (cond
     ((equal node-type "proc_name")
      (concat "proc / " name))
     ((equal node-type "variable_name")
      (concat "func / " name))
     (t
      name))))

(defun ysh-ts-setup ()
  "Setup for `ysh-ts-mode'."
  (interactive)

  ;; font lock crap
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     ysh-ts-mode-font-lock-rules))
  (setq-local font-lock-defaults nil)  
  (setq-local treesit-font-lock-feature-list
              '((comment string keyword)          ; Level 1 (Basic)
                (command variable number)         ; Level 2 (Common)
                (operator)                        ; Level 3 (More detail)
                ()))                              ; Level 4 (Max detail)
  (setq-local treesit-font-lock-level 4)

  ;; indent rules
  (setq-local treesit-simple-indent-rules ysh-ts-mode-indent-rules)

  ;; imenu
  (setq-local treesit-simple-imenu-settings
              `((nil ysh-ts-imenu-node-p nil ysh-ts-imenu-name-function)
                ;; ("proc" ysh-ts-imenu-node-p nil ysh-ts-imenu-name-function)
                ))
  
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode ysh-ts-mode prog-mode "YSH"
  "Major mode for editing ysh."
  :syntax-table nil
  (when (treesit-ready-p 'ysh)
    (treesit-parser-create 'ysh)
    (ysh-ts-setup)))

(provide 'ysh-ts-mode)
