;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(customize-set-variable
 'package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.org/packages/")
   ("melpa-stable" . "http://stable.melpa.org/packages/")
   ;; ("marmalade" . "https://marmalade-repo.org/packages/") ; Errors with TLS connection
   ("orgmode" . "http://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(calendar-week-start-day 1)
 '(calendar-holidays nil) ; remove default holiday entries from calendar
 '(visible-bell t)
 '(visual-line-fringe-indicators
   '(left-curly-arrow right-curly arrow))
 '(make-backup-files nil)
 '(custom-file "~/.emacs.d/custom.el")
 ;; use-package customizations
 '(use-package-always-ensure t)
 '(use-package-always-pin "melpa-stable"))

(define-key global-map "\C-x\C-b" 'bs-show)
(define-key global-map "\C-x\C-p" 'list-processes)
(define-key global-map "\C-x\C-o" 'other-window)
(define-key global-map "\C-x\C-i" 'other-window)
(define-key global-map "\C-x\C-w" 'whitespace-mode)

;; enable colum-number-mode
(column-number-mode)

(use-package gnu-elpa-keyring-update
  :unless (<= 27 emacs-major-version)
  :pin "gnu")

(use-package el-get
  :pin "melpa"
  :config
  (add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")
  ;;TODO: https://github.com/dimitri/el-get/issues/2649
  (if (not (file-directory-p el-get-dir))
      (make-directory el-get-dir)))

(use-package use-package-el-get
  :config (use-package-el-get-setup))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables
   '("PATH" "MANPATH" "NVM_DIR"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package ido
  :custom
  (ido-enable-flex-matching t)
  (ido-auto-merge-work-directories-length -1)
  (ido-everywhere t)
  (ido-mode t))

(use-package ido-vertical-mode
  :custom
  (ido-vertical-show-count t)
  (ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode))

(use-package flx-ido
  :custom
  (ido-enable-flex-matching t)
  (ido-use-faces nil))

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode))

(use-package company
  :bind
  (:map company-mode-map
   ("TAB" . company-indent-or-complete-common))
  :demand t
  :hook
  (emacs-elisp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  :config
  (company-tng-configure-default))

(use-package org
  :init
  (defun org-archive-done-tasks ()
    "Archive all DONE and CANCELED tasks in the subtree of the current entry"
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "//DONE|CANCELED" 'agenda))
  :bind
  (("C-c a" . org-agenda)
   ("C-c k" . org-capture)
   :map org-mode-map
   ("C-c H" . org-archive-done-tasks)
   ("C-c C-e" . org-babel-execute-src-block))
  :hook
  (text-mode . visual-line-mode)
  :config
  (custom-set-variables
   '(org-agenda-span 'day)
   '(org-startup-indented t)
   '(org-startup-folded nil)
   '(org-directory "~/Workspace/org/")
   '(org-agenda-files (list org-directory))
   '(org-archive-location "archives.org::datetree/* %s")
   '(org-agenda-diary-file (concat org-directory "diary.org"))
   '(org-agenda-include-diary t)
   '(org-deadline-warning-days 7)
   '(org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
   '(org-agenda-skip-scheduled-if-deadline-is-shown t)
   '(org-agenda-skip-scheduled-if-done t)
   '(org-agenda-todo-ignore-scheduled 'future)
   '(org-agenda-tags-todo-honor-ignore-options t)
   ;; set SCHEDULED and DEADLINE leaders in agenda view
   '(org-agenda-deadline-leaders (quote ("Deadline!  " "In %d days: " "Late %d days: ")))
   '(org-agenda-scheduled-leaders (quote ("Scheduled! " "For %d days: ")))
   '(org-agenda-window-setup 'only-window)
   '(org-todo-keywords
     (quote ((sequence "TODO(t!)" "ONGOING(o!)" "WAITING(w@)" "|" "DONE(d@)" "CANCELED(c@)"))))
   '(org-todo-keyword-faces
     (quote (("WAITING" . "purple")
	     ("ONGOING" . "orange")
	     ("CANCELED" . "firebrick"))))
   ;; set ARCHIVE tag when todo state is set to CANCELED, and remove when reset to TODO
   `(org-todo-state-tags-triggers
     (quote
      ((todo . ((,org-archive-tag . nil)))
       ("CANCELED" . ((,org-archive-tag . t))))))
   '(org-tag-persistent-alist
     '(("TARGET" . ?t)))
   '(org-tags-exclude-from-inheritance (quote ("TARGET")))
   ;; REFILE BEHAVIOUR
   '(org-refile-targets
     '((nil . (:level . 1))
       (nil . (:tag . "TARGET"))))
   '(org-refile-use-outline-path t)
   '(org-goto-interface 'outline-path-completion)
   '(org-outline-path-complete-in-steps t)
   ;; prefer in-steps that ido for refile completion
   ;; '(org-completion-use-ido t)
   ;; '(org-reverse-note-order t)

   ;; LOGGING
   ;; todo state changes should be logged into drawer
   '(org-log-into-drawer t)
   ;; log when schedule or deadline changes
   '(org-log-redeadline (quote time))
   '(org-log-reschedule (quote time))
   ;;'(org-log-refile (quote time)) ; logs even when refiled during capture

   ;; PRIORITIES
   '(org-priority-start-cycle-with-default t)
   '(org-default-priority 70)
   '(org-lowest-priority 70)
   '(org-highest-priority 65)
   '(org-agenda-sorting-strategy
     '((agenda time-up deadline-down priority-down)
       (todo category-up priority-down)
       (tags category-up priority-down)
       (search category-up)))

   ;; DEPENDENCIES
   '(org-enforce-todo-dependencies t)
   ;;'(org-enforce-todo-checkbox-dependencies t)
   '(org-agenda-dim-blocked-tasks (quote invisible))

   ;; CUSTOM COMMANDS
   '(org-agenda-custom-commands
     '(("c" . "Custom commands")
       ("cb" "Backlog (tasks not scheduled)" todo "TODO"
	((org-agenda-skip-function
	  '(org-agenda-skip-entry-if 'scheduled))))
       ("cf" "Focussed tasks"
	((todo "ONGOING")
	 (todo "WAITING")))
       ("cc" . "Filter tasks by CATEGORY")
       ("cci" "INBOX tasks"
	((alltodo ""))
	((org-agenda-category-filter-preset '("+INBOX"))))
       ("ccf" "INFRA tasks"
	((alltodo ""))
	((org-agenda-category-filter-preset '("+INFRA"))))
       ("ccr" "ROLES tasks"
	((alltodo ""))
	((org-agenda-category-filter-preset '("+ROLES"))))))

   ;; CAPTURE TEMPLATES
   '(org-capture-templates
     (quote
      (("t" "Task")
       ("tt" "Task (Scheduled)" entry
	(file+headline "tasks.org" "INBOX")
	"* TODO %^{Title}\nSCHEDULED: %t\n%?")
       ("td" "Task (Scheduled, with Deadline)" entry
	(file+headline "tasks.org" "INBOX")
	"* TODO %^{Title}\nSCHEDULED: %^{Schedule}t DEADLINE: %^{Deadline}t\n%?")
       ("e" "Event")
       ("et" "Event (with single datetime)" entry
	(file+headline "events.org" "INBOX")
	"* %^{Title}\n%^T\n%?")
       ("er" "Event (with date range)" entry
	(file+headline "events.org" "INBOX")
	"* %^{Title}\n%^t--%^t\n%?")
       ("m" "Memo" entry
	(file+headline "memo.org" "INBOX")
	"* %?\n%T\n")
       ("x" "Example" entry
	(file+headline "example.org" "INBOX")
	"* %?\n%^t\n"))))))

(use-package ediff
  :bind
  (("C-c d f" . ediff-files)
   ("C-c d b" . ediff-buffers))
  :init
  (load-library "ediff")
  :custom
  ;; due to bug with mixup of horizontally/vertically
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  :pin "melpa"
  :bind
  (("C-x C-m" . magit-status)
   ("C-x M-f" . magit-log-buffer-file)
   ("C-x M-b" . magit-blame-addition))
  :custom
  (magit-repository-directories (quote ("~/Workspace/dev")))
  (magit-repository-directories-depth 1))

(use-package undo-tree
  :pin "gnu"
  :bind ("C-x u" . undo-tree-visualize)
  :config
  (global-undo-tree-mode))

(use-package paredit
  :hook
  (lisp-mode . paredit-mode)
  (scheme-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  :init (show-paren-mode))

(use-package projectile
  :init
  (projectile-mode)
  (defun pop-grep-buffer ()
    (pop-to-buffer next-error-last-buffer))
  :hook
  (projectile-grep-finished . pop-grep-buffer)
  :bind
  (:map projectile-mode-map
   ("C-c p" . projectile-command-map))
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package sunrise-commander
  :ensure nil
  :el-get t
  :bind (("C-x d" . sunrise)
	 ("C-x C-d" . sunrise-cd))
  :custom
  (sr-cursor-follows-mouse nil)
  (sr-listing-switches "-lA --group-directories-first")
  (sr-show-file-attributes t)
  (sr-show-hidden-files t)
  (sr-window-split-style (quote horizontal))
  (sr-start-hook nil)
  :config
  ;; run C(opy) and R(ename) operations in background
  ;; when using C-u prefix
  (require 'sunrise-x-loop))

(use-package clojure-mode
  :hook
  (clojure-mode . paredit-mode)
  :custom
  (clojure-indent-style :always-indent))

(use-package cider
  :after (paredit company)
  :bind
  (("C-c c c" . cider-connect)
   ("C-c c j" . cider-jack-in)
   :map cider-mode-map
   ("C-c c e" . cider-pprint-eval-last-sexp)
   ("C-c c b n" . cider-browse-ns-all)
   ("C-c c b s" . cider-browse-spec-all)
   ("C-c c f" . cider-completion-flush-caches)
   ("C-c c q" . cider-quit)
   :map cider-repl-mode-map
   ("C-c c q" . cider-quit))
  :hook
  (cider-mode . eldoc-mode)
  (cider-mode . company-mode)
  (cider-mode . cider-company-enable-fuzzy-completion)
  (cider-repl-mode . cider-company-enable-fuzzy-completion)
  (cider-repl-mode . paredit-mode)
  :custom
  (cider-lein-command "lein")
  (cider-prompt-for-symbol nil)
  (cider-repl-use-pretty-printing t)
  ;; font-lock results in terrible performance
  (cider-repl-use-clojure-font-lock nil))

(use-package sayid
  :config
  (eval-after-load 'clojure-mode '(sayid-setup-package)))

(use-package geiser
  :after (paredit company)
  :bind
  (("C-c g r" . run-geiser)
   ("C-c g c" . geiser-connect))
  :hook
  (geiser-mode . company-mode)
  (geiser-repl-mode . paredit-mode))

(use-package rainbow-delimiters :disabled t)
(use-package rainbow-blocks :disabled t)

(use-package haskell-mode)

(use-package scala-mode2 :disabled t)
(use-package sbt-mode :disabled t)

(use-package js2-refactor
  :after (js2-mode)
  :bind
  (:map js2-mode-map
   ("C-k" . js2r-kill))
  :hook
  (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package js2-mode
  :mode
  ("\\.js\\'" . js2-mode)
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :custom
  (js-indent-level 2)
  :config
  (define-key js2-mode-map (kbd "M-.") nil))

(use-package xref-js2
  :after (js2-mode)
  :init
  (defun init-js2-xref-backend ()
    "Initialises JS2 Xref backend via hook"
    (if (not (executable-find "ag"))
      (message "Necessary executable `ag' is not found on `exec-path'")
      (message "Registered JS2 Xref backend")
      (add-hook 'xref-backend-functions 'xref-js2-xref-backend)))
  :hook
  (js2-mode . init-js2-xref-backend))

(use-package company-tern
  :disabled t ; https://www.reddit.com/r/emacs/comments/g8i10n/companytern_on_melpa_pulled/
  :after (company js2-mode)
  :init
  (defun init-company-tern ()
    (if (not (executable-find "tern"))
      (message "Necessary executable `tern' is not found on `exec-path'")
      (message "Turning on company-mode using Tern")
      (tern-mode) (company-mode)))
  :hook
  (js2-mode . init-company-tern)
  :config
  (add-to-list 'company-backends 'company-tern)
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))

(use-package indium
  :after (js2-mode)
  :hook
  (js2-mode . indium-interaction-mode)
  (indium-repl-mode . indium-switch-to-repl-buffer)
  :bind
  (("C-c i l" . indium-launch)
   ("C-c i c" . indium-connect)
   ("C-c i s" . indium-scratch)
   (:map indium-interaction-mode-map)
   ("C-c i q" . indium-maybe-quit)
   ("C-c i k" . indium-eval-buffer)
   ("C-c i e" . indium-eval-region))
  :custom
  ;; this is default anyway
  (indium-chrome-use-temporary-profile t))

(use-package dockerfile-mode)

(use-package markdown-mode)

;;TODO: https://www.emacswiki.org/emacs/AUCTeX
(use-package tex
  :ensure auctex
  :pin "gnu"
  :hook
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . TeX-PDF-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t))

(use-package csv-mode
  :pin "gnu"
  :bind
  (:map csv-mode-map
   ("C-c a" . csv-align-fields)
   ("C-c u" . csv-unalign-fields))
  :custom
  (csv-align-style (quote auto)))

(use-package nhexl-mode
  :pin "gnu")

(use-package json-mode
  ;; causes errors with compiling json-reformat saying: "attempt to inline hash-table-keys before it was defined"
  :disabled t)

(use-package yaml-mode)

(use-package protobuf-mode
  ;; causes error "Symbol's function definition is void: set-difference"
  :disabled t)

(use-package monokai-theme)
(use-package solarized-theme
  :disabled t)
(use-package zenburn-theme
  :disabled t)

