;;; init.el --- Emacs init file current project  -*- lexical-binding: t; -*-
(mapc 'require '(cl-lib subr-x))

(setq *git-dir* (expand-file-name "Tools/git" (getenv "HOME")))
(setq warning-minimum-level :error
      find-program          (expand-file-name "usr/bin/find.exe" *git-dir*)
      grep-program          (expand-file-name "usr/bin/grep.exe" *git-dir*)
      unix_utils_dir_1      (expand-file-name "usr/local/wbin" *git-dir*)
      unix_utils_dir_2      (expand-file-name "bin" *git-dir*))

(setenv "PATH" (concat (expand-file-name unix_utils_dir_1) path-separator (getenv "PATH")))
(setenv "PATH" (concat (expand-file-name unix_utils_dir_2) path-separator (getenv "PATH")))

(add-to-list 'exec-path unix_utils_dir_1)
(add-to-list 'exec-path unix_utils_dir_2)

(setq grep-find-use-xargs 'exec)

(setq confirm-nonexistent-file-or-buffer nil
      ffap-machine-p-known               'reject)

(defun me/make-path (root-dir &rest path-elements)
  (cl-reduce '(lambda (x &optional y)
                (concat ( file-name-as-directory x) y))
             path-elements :initial-value root-dir))

(defun me/mkdir-p (dir-path)
  (unless (file-exists-p dir-path)
    (make-directory dir-path t)))

(defun me/emacs-dir-path (&rest path-elements)
  (apply 'me/make-path (cons user-emacs-directory path-elements)))

(defun me/switch-window-max ()
  "Switch to the other window and maximize it."
  (interactive)
  (other-window -1)
  (delete-other-windows)
  (goto-char (point-max)))

(defun me/switch-window-normal ()
  "Switch to the other window and maximize it."
  (interactive)
  (other-window -1)
  (delete-other-windows))

(global-set-key (kbd "C-c wm") #'me/switch-window-max)

(defun me/transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;;;; compression
(add-hook 'after-init-hook #'auto-compression-mode)

;;;; utf-8
(defun me/setup-utf8 ()
  (interactive)
  (prefer-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)
  (setq coding-system-for-write 'utf-8
        coding-system-for-read  'utf-8
        file-name-coding-system 'utf-8
        locale-coding-system    'utf-8)
  (set-language-environment     'utf-8)
  (set-default-coding-systems   'utf-8)
  (set-terminal-coding-system   'utf-8)
  (set-keyboard-coding-system   'utf-8)
  (set-selection-coding-system  'utf-8)
  (set-language-environment     'utf-8))
(add-hook 'after-init-hook #'me/setup-utf8)

;;;; utilities
(global-set-key (kbd "C-x M-o") 'join-line)
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)

(defun me/rename-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(defun me/burry-other-buffer ()
  "Close other buffer window."
  (interactive)
  (when (window-parent)
    (other-window -1)
    (bury-buffer)
    (other-window -1)))

(defun me/get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun me/comment-or-uncomment-line-or-region ()
  "Comment or uncomment the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun me/do-with-symbol-at-point-bounds (cb-fn)
  "Do something with the bounds of the symbol at point"
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (funcall cb-fn (car bounds) (cdr bounds)))))

(defun me/touch-file (path)
  "Create a file if it does not exists."
  (when (not (file-exists-p path))
    (with-temp-buffer (write-file path))))

(defun me/indent-region-or-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (let ((coords (if (region-active-p)
                    (list (region-beginning) (region-end))
                  (list (point-min) (point-max)))))
    (indent-region (car coords) (car (last coords)))
    (delete-trailing-whitespace (point-min) (point-max))
    (untabify (point-min) (point-max))))

;;;; eshell
(eval-after-load 'eshell
  (progn
    (setq eshell-highlight-prompt       nil
          eshell-history-size           8000
          eshell-path-env               (getenv "PATH")
          eshell-cmpl-cycle-completions nil)

    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)
    (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

    '(defun eshell/up (&optional level)
       "Change directory from one up to a level of folders."
       (let ((path-level (or level 1)))
         (cd (apply 'concat (make-list path-level "../")))))))

;;;; ansi
(autoload 'ansi-color-apply-on-region "ansi-color" "ansi colors" t nil)

(defun me/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook #'me/colorize-compilation-buffer)

;;;; auth-source
(autoload 'auth-source-user-and-password "auth-source" "credentials" t nil)

;;;; package
(eval-after-load 'package
  (progn
    (require 'package)
    (setq package-check-signature nil)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (package-initialize)
    (setq me-pkg-initialized nil)
    
    '(mapc (lambda (p)
             (unless (package-installed-p p)
               (progn
                 (unless me-pkg-initialized
                   (package-refresh-contents)
                   (setq me-pkg-initialized t))
                 (package-install p))))
           package-selected-packages)))

;;;; dired
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd ".") #'dired-up-directory))

(defun me/open-in-explorer ()
  (interactive)
  (async-shell-command (format "explorer .")))

(global-set-key (kbd "C-x D") 'me/open-in-explorer)

;;;; encryption
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;;;; hippie-expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(global-set-key (kbd "M-/") #'hippie-expand)

;;;; line-operations
(defun me/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun me/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun me/duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1))
                          (newline))))))
        (dotimes (i (abs (or n 1)))
          (insert text))))
    (if use-region nil
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-set-key (kbd "M-P")   #'me/move-line-up)
(global-set-key (kbd "M-N")   #'me/move-line-down)
(global-set-key (kbd "C-c d") #'me/duplicate-line-or-region)


;;;; transpose
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "C-t"))
(global-set-key (kbd "M-t c") #'transpose-chars)
(global-set-key (kbd "M-t l") #'transpose-lines)
(global-set-key (kbd "M-t s") #'transpose-sexps)
(global-set-key (kbd "M-t w") #'me/transpose-windows)

;;;; things-at-point
(defun me/cut-symbol-at-point ()
  "Cut the symbol at point."
  (interactive)
  (me/do-with-symbol-at-point-bounds 'kill-region))

(defun me/copy-symbol-at-point ()
  "Copy the symbol at point."
  (interactive)
  (me/do-with-symbol-at-point-bounds 'kill-ring-save))

(defun me/mark-symbol-at-point ()
  "Mark symbol at point."
  (interactive)
  (me/do-with-symbol-at-point-bounds #'(lambda (start end)
                                         (goto-char start)
                                         (set-mark-command nil)
                                         (goto-char end))))

(global-set-key (kbd "C-h C-w") #'me/cut-symbol-at-point)
(global-set-key (kbd "C-h M-w") #'me/copy-symbol-at-point)
(global-set-key (kbd "C-c m w") #'me/mark-symbol-at-point)

;;;; general
(global-set-key (kbd "C-c m l") (kbd "C-a C-@ C-e"))
(global-set-key (kbd "C-x C-r") #'query-replace)
(global-set-key (kbd "C-c /") #'me/comment-or-uncomment-line-or-region)

(global-set-key (kbd "C-c \\") #'me/indent-region-or-buffer)
(global-set-key (kbd "C-c ar") #'align-regexp)
(global-set-key (kbd "RET")    #'newline-and-indent)

;;;; window-management
(fset 'scroll-other-window-up 'scroll-other-window-down)

(global-set-key (kbd "C-M-y")     #'scroll-other-window-up)
(global-set-key (kbd "C-M-v")     #'scroll-other-window)
(global-set-key (kbd "C-h C-M-o") #'me/switch-window-max)

(eval-after-load 'windmove
  (progn
    (global-set-key (kbd "C-c wn") #'windmove-up)
    (global-set-key (kbd "C-c ws") #'windmove-down)
    (global-set-key (kbd "C-c we") #'windmove-right)
    '(global-set-key (kbd "C-c ww") #'windmove-left)))

;;;; buffers
(defun me/kill-buffer-no-confirm ()
  "Kill buffer without confirmation."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(defun me/kill-buffer-matching-mode (mode-name)
  "Kill buffer matching mode"
  (interactive "sEnter mode name: ")
  (dolist (active-buffer (buffer-list))
    (when (string= mode-name (format "%s" (buffer-local-value 'major-mode active-buffer)))
      (kill-buffer active-buffer))))

(defun me/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(global-set-key (kbd "C-x C-b") #'ibuffer-list-buffers)
(global-set-key (kbd "C-h [")   #'next-buffer)
(global-set-key (kbd "C-h ]")   #'previous-buffer)
(global-set-key (kbd "C-c r")   #'me/revert-buffer-no-confirm)
(global-set-key (kbd "C-x M-k") #'me/kill-buffer-no-confirm)
(global-set-key (kbd "C-x M-K") #'me/kill-buffer-matching-mode)

;;;; xml
(setq nxml-slash-auto-complete-flag t
      nxml-child-indent             2
      nxml-outline-child-indent     2)

;;;; xref
(with-eval-after-load 'xref
  (global-set-key (kbd "M-\"") #'xref-find-apropos))

;;;; project
(eval-after-load 'project
  (progn
    (require 'project)

    (defun me/project-dired ()
      (interactive)
      (dired (cdr (project-current))))

    (defun me/project-files ()
      (interactive)
      (let* ((d (cdr (project-current)))
             (command (format "%s %s %s -type f -print0"
                              find-program
                              (shell-quote-argument
                               (expand-file-name d))
                              (xref--find-ignores-arguments
                               (project-ignores (project-current t) d)
                               (expand-file-name d))))
             (files (split-string (shell-command-to-string command) "\0" t))
             (files-list
              (delete-dups
               (delq nil files)))
             (files-hash (make-hash-table :test 'equal)))
        (dolist (p files-list)
          (puthash (file-relative-name p d) p files-hash))

        (find-file
         (gethash (completing-read "Select file: "
                                   (hash-table-keys files-hash))
                  files-hash))))

    (defun me/project-dirs ()
      (interactive)
      (let* ((d (cdr (project-current)))
             (command (format "%s %s %s -type d -print0"
                              find-program
                              (shell-quote-argument
                               (expand-file-name d))
                              (xref--find-ignores-arguments
                               (project-ignores (project-current t) d)
                               (expand-file-name d))))
             (dirs (split-string (shell-command-to-string command) "\0" t))
             (dirs-list
              (delete-dups
               (delq nil
                     (mapcar #'file-name-directory
                             dirs))))
             (dirs-hash (make-hash-table :test 'equal)))
        (dolist (p dirs-list)
          (puthash (file-relative-name p d) p dirs-hash))

        (dired
         (gethash (completing-read "Select directory: "
                                   (hash-table-keys dirs-hash))
                  dirs-hash))))

    (global-set-key (kbd "C-c pf") 'me/project-files)
    (global-set-key (kbd "C-c pD") 'me/project-dirs)
    '(global-set-key (kbd "C-c pd") 'me/project-dired)))

;;;; imenu
(global-set-key (kbd "M-s i") #'imenu)


;;;; ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; org-mode
(with-eval-after-load 'org
  (setq org-use-speed-commands    t
        org-directory             "~/Documents/notes"
        org-hide-leading-stars    nil
        org-cycle-separator-lines 0)

  (setq org-archive-location      (me/make-path org-directory "archives.org")
        org-agenda-files          (directory-files org-directory t "\\.org$")
        org-export-html-postamble nil
        org-me-todo-file         (me/make-path org-directory "todo.org")
        org-me-notes-file        (me/make-path org-directory "tasks.org"))

  (setq org-link-mailto-program (quote (compose-mail "%a" "%s")))

  (setq org-clock-into-drawer                 t
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done               t
        org-log-into-drawer                   t
        org-clock-persist                     t)

  (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  (setq org-cycle-separator-lines 1)
  (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

  (setq org-capture-templates
        '(("m" "Meeting" entry (file+headline org-me-notes-file "Meeting")
           "* TODO Meeting %? \nSCHEDULED: %^t" :clock-in t :clock-resume t)
          ("t" "Task" entry (file+headline org-me-notes-file "Tasks")
           "** TODO %? \nSCHEDULED: %^t" :clock-in t :clock-resume t)))

  (setq org-log-done-with-time t)

  (setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)")
                            (sequence "|" "DONE(d)")
                            (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                            (sequence "|" "CANCELED(c)")))

  (setq org-tag-alist '(("@design"   . ?d)
                        ("@task"     . ?t)))

  (setq org-agenda-exporter-settings '((ps-number-of-columns 2)
                                       (ps-landscape-mode t)
                                       (org-agenda-add-entry-text-maxlines 5)
                                       (htmlize-output-type 'css)))

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (defun me/org-mode-hook ()
    (add-to-list 'org-structure-template-alist '("t" "#+TITLE: ?"))
    (define-key org-mode-map (kbd "C-c ot") #'org-todo)
    (define-key org-mode-map (kbd "C-c oh") #'org-insert-heading)
    (define-key org-mode-map (kbd "C-c os") #'org-insert-subheading)
    (define-key org-mode-map (kbd "C-c oa") #'org-agenda)
    (define-key org-mode-map (kbd "C-c oc") #'org-capture)
    (org-indent-mode 1))

  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
  (add-hook 'org-mode-hook #'me/org-mode-hook)

  '(add-hook 'org-after-todo-statistics-hook #'org-summary-todo))

;;;; movement
(global-set-key (kbd "M-p")   (kbd "C-u 15 C-p"))
(global-set-key (kbd "M-n")   (kbd "C-u 15 C-n"))

;;;; others
(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

(global-set-key (kbd "C-x C-m") #'execute-extended-command)
(global-set-key (kbd "M-s l")   #'goto-line)
(global-set-key (kbd "M-s e")   #'eshell)
(global-set-key (kbd "M-s s")   #'grep-find)
(global-set-key (kbd "M-s o")   #'occur)
(global-set-key (kbd "M-s r")   #'recentf-open-files)
(global-set-key (kbd "M-Z")     #'zap-up-to-char)
(global-set-key (kbd "M-z")     #'zap-to-char)

;;;; disabled-commands
(put 'erase-buffer     'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)

(fset 'yes-or-no-p 'y-or-n-p)

;;;; registers
(set-register ?i `(file . ,user-init-file))
(set-register ?e `(file . ,user-emacs-directory))

;;;; yasnippet
(setq yas-verbosity 1)
(require 'yasnippet)

(defun me/yas-snippet-reload ()
  (interactive)
  (yas-reload-all))
(add-hook 'after-init-hook 'me/yas-snippet-reload)

(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)

(defun me/yas-snippet-setup ()
  (interactive)
  (define-key yas-minor-mode-map (kbd "C-c ye") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "C-c yi") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil))

(add-hook 'yas-minor-mode-hook 'me/yas-snippet-setup)


;;;; outline
(with-eval-after-load 'outline
  (define-key outline-minor-mode-map
    (concat outline-minor-mode-prefix "") 'outline-hide-body))

(setq my-backup-dir (expand-file-name "backups" user-emacs-directory ))
(me/mkdir-p my-backup-dir)
(setq backup-directory-alist `(("." . ,my-backup-dir))
      delete-old-versions t
      kept-new-versions 12
      kept-old-versions 12
      version-control t)

;;;; xref
(with-eval-after-load 'xref
  (global-set-key (kbd "M-\"") 'xref-find-apropos))

;;;; google-c-style
(autoload 'google-set-c-style "google-c-style")
(autoload 'google-make-newline-indent "google-c-style")
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;;; eglot
(eval-after-load 'eglot
  (progn
    (require 'eglot)

    (defun eglot-rimero/eglot-setup ()
      (interactive)
      (define-key (current-local-map) (kbd "C-c l r") #'eglot-rename)
      (define-key (current-local-map) (kbd "C-c l h") #'eglot-help-at-point)
      (define-key (current-local-map) (kbd "C-c l f") #'eglot-format)
      (define-key (current-local-map) (kbd "C-c l d") #'flymake-show-diagnostics-buffer)
      (define-key (current-local-map) (kbd "C-c l a") #'eglot-code-actions)
      (eldoc-mode -1))

    (add-hook 'eglot--managed-mode-hook 'eglot-rimero/eglot-setup)
    '(add-hook 'js-mode-hook 'eglot-ensure)))


;;;; version control
(eval-after-load 'vc
  (progn
    (require 'vc)
    (defun me/vc-delete-marked-files ()
      (interactive)
      (let ( (vc-selected-files (vc-deduce-fileset t))
             (vc-selected-dir (vc-root-dir)) )
        (dolist (f (cadr vc-selected-files))
          (when (file-exists-p f)
            (if (file-directory-p f)
                (delete-directory f t)
              (delete-file f))))
        (me/kill-buffer-no-confirm)
        (vc-dir vc-selected-dir)))

    (defun me/vc-refresh ()
      (interactive)
      (let ( (vc-selected-dir (vc-root-dir)) )
        (me/kill-buffer-no-confirm)
        (vc-dir vc-selected-dir)
        (delete-other-windows)))

    (add-hook 'vc-dir-mode-hook 'me/vc-setup-keys)
    '(defun me/vc-setup-keys ()
       (interactive)
       (define-key vc-dir-mode-map (kbd "r") 'me/vc-delete-marked-files)
       (define-key vc-dir-mode-map (kbd "g") 'me/vc-refresh))))

;;;; swagger
(eval-after-load 'yaml-mode
  (progn
    (defun me/openapi-newline ()
      (interactive)
      (newline-and-indent))

    (defun me/openapi-create ()
      (interactive)
      (let* ((openapi-fn-by-type #s(hash-table size 8 test equal data ("Object"    me/openapi-obj-simple
                                                                       "String"    me/openapi-string
                                                                       "Boolean"   me/openapi-boolean
                                                                       "Array"     me/openapi-array
                                                                       "Integer"   me/openapi-int
                                                                       "Decimal"   me/openapi-bigdecimal
                                                                       "Date"      me/openapi-date
                                                                       "Date-Time" me/openapi-datetime)))
             (obj-type (completing-read "Type: " (hash-table-keys openapi-fn-by-type) nil t ))
             (obj-name (read-string "Name: ")))
        (funcall (gethash obj-type openapi-fn-by-type) obj-name) ))

    (defun me/openapi-obj-simple (var-name)
      (interactive "sEnter variable: ")
      (insert (format "%s:" var-name))
      (newline-and-indent)
      (insert (format "%s: %s" "properties" ""))
      (me/openapi-newline))

    (defun me/openapi-bigdecimal (var-name)
      (interactive "sEnter variable: ")
      (insert (format "%s:" var-name))
      (newline-and-indent)
      (insert (format "%s: %s" "type" "number"))
      (newline-and-indent)
      (insert (format "%s: %s" "format" "decimal"))
      (me/openapi-newline))

    (defun me/openapi-int (var-name)
      (interactive "sEnter variable: ")
      (insert (format "%s:" var-name))
      (newline-and-indent)
      (insert (format "%s: %s" "type" "integer"))
      (newline-and-indent)
      (insert (format "%s: %s" "format" "int32"))
      (me/openapi-newline))

    (defun me/openapi-array (var-name)
      (interactive "sEnter variable: ")
      (insert (format "%s:" var-name))
      (newline-and-indent)
      (insert (format "%s: %s" "type" "array"))
      (me/openapi-newline)
      (insert "items: ")
      (me/openapi-newline)
      (insert "type: "))

    (defun me/openapi-boolean (var-name)
      (interactive "sEnter variable: ")
      (insert (format "%s:" var-name))
      (newline-and-indent)
      (insert (format "%s: %s" "type" "boolean"))
      (me/openapi-newline))

    (defun me/openapi-string (var-name)
      (interactive "sEnter variable: ")
      (insert (format "%s:" var-name))
      (newline-and-indent)
      (insert (format "%s: %s" "type" "string"))
      (me/openapi-newline))

    (defun me/openapi-date (var-name)
      (interactive "sEnter variable: ")
      (insert (format "%s:" var-name))
      (newline-and-indent)
      (insert (format "%s: %s" "type" "string"))
      (newline-and-indent)
      (insert (format "%s: %s" "format" "date"))
      (me/openapi-newline))

    (defun me/openapi-datetime (var-name)
      (interactive "sEnter variable: ")
      (insert (format "%s:" var-name))
      (newline-and-indent)
      (insert (format "%s: %s" "type" "string"))
      (newline-and-indent)
      (insert (format "%s: %s" "format" "date-time"))
      (me/openapi-newline))

    (defun me/setup-yaml-mode ()
      (interactive)
      (define-key yaml-mode-map (kbd "C-c sc") 'me/openapi-create))

    '(add-hook 'yaml-mode-hook 'me/setup-yaml-mode)))

(defun me/init-hooks ()
  (interactive)
  (setq-default frame-title-format '("Text Editor " "- %b"))
  (require 'imenu))

(add-hook 'after-init-hook 'me/init-hooks)

;;;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-save-flag 1)
 '(c-basic-offset 2)
 '(groovy-indent-offset 2)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message "")
 '(js-indent-level 2)
 '(linum-format " %5i ")
 '(menu-bar-mode nil)
 '(package-check-signature nil)
 '(package-selected-packages (quote (yasnippet eglot yaml-mode typescript-mode markdown-mode plantuml-mode)))
 '(plantuml-java-args
   (quote
    ("-Djava.awt.headless=true" "-jar" "--illegal-access=deny")))
 '(recentf-mode t)
 '(safe-local-variable-values
   (quote
    ((eval progn
           (outline-minor-mode 1)
           (outline-hide-body)))))
 '(show-paren-mode t)
 '(tab-always-indent t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "nil" :family "Consolas"))))
 '(cursor ((t (:background "#d4ce7d" :foreground "magenta"))))
 '(dap-ui-pending-breakpoint-face ((t (:background "#magenta" :bold t :foreground "white"))))
 '(dap-ui-verified-breakpoint-face ((t (:background "#aa5aa1" :bold t :foreground "white"))))
 '(mode-line ((t (:background "grey30" :foreground "grey80" :box (:line-width -1 :style released-button))))))
