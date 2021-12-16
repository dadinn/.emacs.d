
;; Bootstrapping straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(calendar-week-start-day 1)
 '(calendar-holidays nil) ; remove default holiday entries from calendar
 '(visible-bell t)
 '(visual-line-fringe-indicators
   '(left-curly-arrow right-curly arrow))
 '(require-final-newline t)
 '(make-backup-files nil)
 '(create-lockfiles nil)
 '(custom-file "~/.emacs.d/custom.el")
 ;; straight.el customizations
 '(straight-use-package-by-default t)
 '(straight-vc-git-default-protocol (quote ssh)))

(define-key global-map "\C-x\C-b" 'bs-show)
(define-key global-map "\C-x\C-p" 'list-processes)
(define-key global-map "\C-x\C-o" 'other-window)
(define-key global-map "\C-x\C-i" 'other-window)
(define-key global-map "\C-x\C-w" 'whitespace-mode)
(define-key global-map "\C-x\C-n"
 (if (version<= "26.0.50" emacs-version)
   'display-line-numbers-mode
   'linum-mode))

(column-number-mode)
(global-auto-revert-mode)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (defvar exec-path-from-shell-additional-variables
    (list "NVM_DIR" "JAVA_HOME"))
  :config
  (dolist (var exec-path-from-shell-additional-variables)
    (add-to-list (quote exec-path-from-shell-variables) var))
  (exec-path-from-shell-initialize))

(use-package selectrum
  :custom
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  :init
  (selectrum-mode))

(use-package prescient
  :custom
  (prescient-save-file
   (locate-user-emacs-file "prescient-save.el"))
  :config
  (add-to-list 'prescient-filter-method 'fuzzy)
  (prescient-persist-mode))

(use-package selectrum-prescient
  :after (prescient selectrum)
  :init
  (selectrum-prescient-mode))

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

(use-package company-prescient
  :after (prescient company)
  :init
  (company-prescient-mode)
  :custom
  (company-prescient-sort-length-enable nil))

(use-package org
  :init
  (defun org-archive-done-tasks ()
    "Archive all DONE and CANCELED tasks in the subtree of the current entry"
    (interactive)
    (org-map-entries
     (lambda ()
       (let ((prev-heading (outline-previous-heading)))
	 (org-archive-subtree)
	 (setq org-map-continue-from prev-heading)))
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
   '(org-outline-path-complete-in-steps nil)
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
  :bind
  (("C-x C-m" . magit-status)
   ("C-x M-f" . magit-log-buffer-file)
   ("C-x M-b" . magit-blame-addition))
  :custom
  (magit-repository-directories (quote ("~/Workspace/dev")))
  (magit-repository-directories-depth 1))

(use-package undo-tree
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
  :hook
  (projectile-grep-finished . (lambda () (pop-to-buffer next-error-last-buffer)))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (dolist (dir (list "node_modules" "target"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (projectile-mode))

(use-package sunrise-commander
  :ensure nil
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
  ("C-c c c" . cider-connect)
  ("C-c c j" . cider-jack-in)
  (:map cider-mode-map
   ("C-c b n" . cider-browse-ns-all)
   ("C-c b s" . cider-browse-spec-all)
   ("C-c c f" . cider-completion-flush-caches))
  :hook
  (cider-mode . eldoc-mode)
  (cider-mode . company-mode)
  (cider-mode . cider-company-enable-fuzzy-completion)
  (cider-repl-mode . cider-company-enable-fuzzy-completion)
  (cider-repl-mode . paredit-mode)
  :custom
  (cider-lein-command "lein")
  (cider-prompt-for-symbol nil)
  (cider-debug-prompt 'minibuffer)
  (cider-repl-use-pretty-printing t)
  ;; font-lock results in terrible performance
  (cider-repl-use-clojure-font-lock nil))

(use-package sayid
  :after (clojure-mode)
  :config
  (sayid-setup-package))

(use-package geiser
  :straight (:protocol https)
  :after (paredit company)
  :bind
  (("C-c g r" . run-geiser)
   ("C-c g c" . geiser-connect))
  :hook
  (geiser-mode . company-mode)
  (geiser-repl-mode . paredit-mode))

(use-package geiser-guile
  :straight
  (:protocol https
   :local-repo "geiser-guile")
  :after (geiser)
  :custom
  (geiser-guile-load-init-file t))

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
    (cond
     ((not (executable-find "ag"))
      (message "Necessary executable `ag' is not found on `exec-path'"))
     (t (message "Registered JS2 Xref backend")
      (add-hook 'xref-backend-functions 'xref-js2-xref-backend))))
  :hook
  (js2-mode . init-js2-xref-backend))

(use-package company-tern
  :after (company js2-mode)
  :init
  (defun enable-company-tern ()
    "Initialises Tern company backend"
    (cond
     ((not (executable-find "tern"))
      (message "Necessary executable `tern' is not found on `exec-path'"))
     (t (message "Turning on company-mode using Tern")
	(company-mode)
	(tern-mode))))
  :hook
  (js2-mode . enable-company-tern)
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
(use-package tex-site
  :straight auctex
  :hook
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . TeX-PDF-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t))

(use-package csv-mode
  :bind
  (:map csv-mode-map
   ("C-c a" . csv-align-fields)
   ("C-c u" . csv-unalign-fields))
  :custom
  (csv-align-style (quote auto)))

(use-package nhexl-mode)

(use-package json-mode
  ;; causes errors with compiling json-reformat saying: "attempt to inline hash-table-keys before it was defined"
  :disabled t)

(use-package yaml-mode)

(use-package protobuf-mode
  ;; causes error "Symbol's function definition is void: set-difference"
  :disabled t)

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package company-restclient
  :after (restclient company)
  :hook (restclient-mode . company-mode)
  :init
  (add-to-list 'company-backends 'company-restclient))

(use-package monokai-theme
  :config (load-theme (quote monokai) t))

(use-package solarized-theme
  :disabled t)
(use-package zenburn-theme
  :disabled t)

