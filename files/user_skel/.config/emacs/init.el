;;; init.el --- Emacs init file current project  -*- lexical-binding: t; -*-

;;;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-save-flag 1)
 '(c-basic-offset 4) 
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(linum-format " %5i ")
 '(menu-bar-mode nil)
 '(package-check-signature nil)
 '(package-selected-packages '(rimero-theme xclip avy multiple-cursors))
 '(plantuml-java-args
   '("-Djava.awt.headless=true" "-jar" "--illegal-access=deny"))
 '(recentf-mode t)
 '(outline-minor-mode-prefix "h")
 '(safe-local-variable-values '((eval progn (outline-minor-mode 1) (outline-hide-body))))
 '(show-paren-mode t)
 '(tab-always-indent t)
 '(tab-width 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "DejaVu Sans Mono")))))

;;;; begin-configuration
(mapc 'require '(cl-lib subr-x))

(setq confirm-nonexistent-file-or-buffer nil
      ffap-machine-p-known               'reject)

(defun user/make-path (root-dir &rest path-elements)
  (cl-reduce '(lambda (x &optional y)
                (concat ( file-name-as-directory x) y))
             path-elements :initial-value root-dir))

(defun user/mkdir-p (dir-path)
  (unless (file-exists-p dir-path)
    (make-directory dir-path t)))

(defun user/emacs-dir-path (&rest path-elements)
  (apply 'user/make-path (cons user-emacs-directory path-elements)))

(defun user/switch-window-max ()
  "Switch to the other window and maximize it."
  (interactive)
  (other-window -1)
  (delete-other-windows)
  (goto-char (point-max)))

(defun user/switch-window-normal ()
  "Switch to the other window and maximize it."
  (interactive)
  (other-window -1)
  (delete-other-windows))

(global-set-key (kbd "C-c wm") #'user/switch-window-max)

(defun ers/transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows."))
  (let* ((windows (window-list))
         (w1 (car windows))
         (w2 (nth 1 windows))
         (w1b (window-buffer w1))
         (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

;;;; compression
(add-hook 'after-init-hook #'auto-compression-mode)

;;;; utf-8
(defun user/setup-utf8 ()
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
(add-hook 'after-init-hook #'user/setup-utf8)

;;;; utilities
(global-set-key (kbd "C-x M-o") 'join-line)
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)

(defun user/rename-buffer-file ()
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

(defun user/burry-other-buffer ()
  "Close other buffer window."
  (interactive)
  (when (window-parent)
    (other-window -1)
    (bury-buffer)
    (other-window -1)))

(defun user/get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun user/comment-or-uncomment-line-or-region ()
  "Comment or uncomment the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun user/do-with-symbol-at-point-bounds (cb-fn)
  "Do something with the bounds of the symbol at point"
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (funcall cb-fn (car bounds) (cdr bounds)))))

(defun user/touch-file (path)
  "Create a file if it does not exists."
  (when (not (file-exists-p path))
    (with-temp-buffer (write-file path))))

(defun user/indent-region-or-buffer ()
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

(defun user/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook #'user/colorize-compilation-buffer)

;;;; auth-source
(autoload 'auth-source-user-and-password "auth-source" "credentials" t nil)

;;;; package
(eval-after-load 'package
  (progn
    (require 'package)
    (setq package-check-signature nil)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (package-initialize)
    (setq user-pkg-initialized nil)

    '(mapc (lambda (p)
             (unless (package-installed-p p)
               (progn
                 (unless user-pkg-initialized
                   (package-refresh-contents)
                   (setq user-pkg-initialized t))
                 (package-install p))))
           package-selected-packages)))

;;;; dired
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd ".") #'dired-up-directory))

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
(defun user/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun user/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun user/duplicate-line-or-region (&optional n)
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

(global-set-key (kbd "M-P")   #'user/move-line-up)
(global-set-key (kbd "M-N")   #'user/move-line-down)
(global-set-key (kbd "C-c d") #'user/duplicate-line-or-region)


;;;; transpose
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "C-x C-t"))
(global-set-key (kbd "C-x C-t c") #'transpose-chars)
(global-set-key (kbd "C-x C-t l") #'transpose-lines)
(global-set-key (kbd "C-x C-t s") #'transpose-sexps)
(global-set-key (kbd "C-x C-t w") #'user/transpose-windows)

;;;; things-at-point
(defun user/cut-symbol-at-point ()
  "Cut the symbol at point."
  (interactive)
  (user/do-with-symbol-at-point-bounds 'kill-region))

(defun user/copy-symbol-at-point ()
  "Copy the symbol at point."
  (interactive)
  (user/do-with-symbol-at-point-bounds 'kill-ring-save))

(defun user/mark-symbol-at-point ()
  "Mark symbol at point."
  (interactive)
  (user/do-with-symbol-at-point-bounds #'(lambda (start end)
                                           (goto-char start)
                                           (set-mark-command nil)
                                           (goto-char end))))

(global-set-key (kbd "C-h C-w") #'user/cut-symbol-at-point)
(global-set-key (kbd "C-h M-w") #'user/copy-symbol-at-point)
(global-set-key (kbd "C-c m w") #'user/mark-symbol-at-point)

;;;; general
(global-set-key (kbd "C-c m l") (kbd "C-a C-@ C-e"))
(global-set-key (kbd "C-x C-r") #'query-replace)
(global-set-key (kbd "C-c /") #'user/comment-or-uncomment-line-or-region)

(global-set-key (kbd "C-c \\") #'user/indent-region-or-buffer)
(global-set-key (kbd "C-c ar") #'align-regexp)
(global-set-key (kbd "RET")    #'newline-and-indent)

;;;; window-management
(fset 'scroll-other-window-up 'scroll-other-window-down)

(global-set-key (kbd "C-M-y")     #'scroll-other-window-up)
(global-set-key (kbd "C-M-v")     #'scroll-other-window)
(global-set-key (kbd "C-h C-M-o") #'user/switch-window-max)

(eval-after-load 'windmove
  (progn
    (global-set-key (kbd "C-c wn")  #'windmove-up)
    (global-set-key (kbd "C-c ws")  #'windmove-down)
    (global-set-key (kbd "C-c we")  #'windmove-right)
    '(global-set-key (kbd "C-c ww") #'windmove-left)))

;;;; buffers
(defun user/kill-buffer-no-confirm ()
  "Kill buffer without confirmation."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(defun user/kill-buffer-matching-mode (mode-name)
  "Kill buffer matching mode"
  (interactive "sEnter mode name: ")
  (dolist (active-buffer (buffer-list))
    (when (string= mode-name (format "%s" (buffer-local-value 'major-mode active-buffer)))
      (kill-buffer active-buffer))))

(defun user/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(global-set-key (kbd "C-x C-b") #'ibuffer-list-buffers)
(global-set-key (kbd "C-h [")   #'next-buffer)
(global-set-key (kbd "C-h ]")   #'previous-buffer)
(global-set-key (kbd "C-h C-r") #'user/revert-buffer-no-confirm)
(global-set-key (kbd "C-x M-k") #'user/kill-buffer-no-confirm)
(global-set-key (kbd "C-x M-K") #'user/kill-buffer-matching-mode)

;;;; multiple-cursors
(eval-after-load 'multiple-cursors
  (progn
    (require 'multiple-cursors)
    (global-set-key (kbd "C-c me") 'mc/edit-lines)
    '(global-set-key (kbd "C-c ma") 'mc/mark-all-like-this)))

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

    (defun user/project-dired ()
      (interactive)
      (dired (cdr (project-current))))

    (defun user/project-files ()
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

    (defun user/project-dirs ()
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

    (global-set-key (kbd "C-c pf") 'user/project-files)
    (global-set-key (kbd "C-c pD") 'user/project-dirs)
    '(global-set-key (kbd "C-c pd") 'user/project-dired)))

;;;; imenu
(global-set-key (kbd "M-s i") #'imenu)


;;;; ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; org-mode
(with-eval-after-load 'org
  (setq org-use-speed-commands    t
        org-hide-leading-stars    nil
        org-cycle-separator-lines 0)

  (setq org-clock-into-drawer                 t
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done               t
        org-log-into-drawer                   t
        org-clock-persist                     t)

    (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  (setq org-cycle-separator-lines 1)
  (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

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

  (defun user/org-mode-hook ()
    (require 'org-tempo)
    (define-key org-mode-map (kbd "C-c ot") #'org-todo)
    (define-key org-mode-map (kbd "C-c oh") #'org-insert-heading)
    (define-key org-mode-map (kbd "C-c os") #'org-insert-subheading)
    (define-key org-mode-map (kbd "C-c oa") #'org-agenda)
    (define-key org-mode-map (kbd "C-c oc") #'org-capture)
    (org-indent-mode 1))

  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
  (add-hook 'org-mode-hook #'user/org-mode-hook)

  '(add-hook 'org-after-todo-statistics-hook #'org-summary-todo))

;;;; movement
(global-set-key (kbd "M-p")   (kbd "C-u 15 C-p"))
(global-set-key (kbd "M-n")   (kbd "C-u 15 C-n"))

;;;; others
(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

(global-set-key (kbd "C-x C-m") #'execute-extended-command)
(global-set-key (kbd "M-s j")   #'avy-goto-char)
(global-set-key (kbd "M-s l")   #'goto-line)
(global-set-key (kbd "M-s e")   #'eshell)
(global-set-key (kbd "M-s s")   #'grep-find)
(global-set-key (kbd "M-s o")   #'occur)
(global-set-key (kbd "M-s r")   #'recentf-open-files)
(global-set-key (kbd "M-Z")     #'zap-up-to-char)
(global-set-key (kbd "M-z")     #'zap-to-char)
(global-set-key (kbd "M-\"")    #'xref-find-apropos)
(global-set-key (kbd "M-s i")   #'imenu)

;;;; disabled-commands
(put 'erase-buffer     'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)

(fset 'yes-or-no-p 'y-or-n-p)

;;;; registers
(set-register ?i `(file . ,user-init-file))
(set-register ?e `(file . ,user-emacs-directory))

;;;; outline
(eval-after-load 'outline
  (progn
    (defun user/outline-setup ()
      (define-key outline-minor-mode-map
        (concat outline-minor-mode-prefix "") 'outline-hide-body))
    '(add-hook 'outline-minor-mode-hook 'user/outline-setup)))

(setq user-backup-dir (expand-file-name "backups" user-emacs-directory ))
(user/mkdir-p user-backup-dir)
(setq backup-directory-alist `(("." . ,user-backup-dir))
      delete-old-versions t
      kept-new-versions 12
      kept-old-versions 12
      version-control t)

;;;; xref
(with-eval-after-load 'xref
  (global-set-key (kbd "M-\"") 'xref-find-apropos))

;;;; clipboard
(eval-after-load 'xclip
  (progn
    (require 'xclip)
    '(add-hook 'after-init-hook 'xclip-mode)))

;;;; additional-settings
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(when (fboundp 'tool-bar-mode)
   (tool-bar-mode -1))

(load-theme 'rimero t)

;;; Local Variables:
;;; outline-regexp: ";;;; "
;;; eval:(progn (outline-minor-mode 1) (outline-hide-body))
;;; End:
