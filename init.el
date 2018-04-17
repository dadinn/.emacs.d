(require 'package)
(customize-set-variable
  'package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
    ("melpa" . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/")
    ;; ("marmalade" . "https://marmalade-repo.org/packages/") ; Errors with TLS connection
    ("orgmode" . "http://orgmode.org/elpa/")
    ("SC" . "http://joseito.republika.pl/sunrise-commander/")))
(package-initialize)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(calendar-week-start-day 1)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly arrow))
 '(visible-bell t)
 ;; remove all the default holiday entries from calendar
 '(calendar-holidays nil)
 '(custom-file "~/.emacs.d/custom.el")
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))

(define-key global-map "\C-x\C-b" 'bs-show)
(define-key global-map "\C-x\C-o" 'other-window)
(define-key global-map "\C-x\C-i" 'other-window)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package ido
  :ensure t
  :config
  (custom-set-variables
   '(ido-enable-flex-matching t)
   '(ido-everywhere t)
   '(ido-mode t)))

(use-package ido-vertical-mode
  :ensure t
  :config
  (custom-set-variables
   '(ido-vertical-mode t)
   '(ido-vertical-show-count t)
   '(ido-vertical-pad-list nil)
   '(ido-vertical-define-keys 'C-n-and-C-p-only)))

(use-package flx-ido
  :ensure t
  :config
  (custom-set-variables
   '(ido-enable-flex-matching t)
   '(ido-use-faces nil)))

(use-package org
  :ensure t
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
   ("C-c C-h" . org-archive-done-tasks)
   ("C-c C-e" . org-babel-execute-src-block))
  :config
  (add-hook 'text-mode-hook 'visual-line-mode)
  (custom-set-variables
   '(org-agenda-span 'day)
   '(org-startup-indented t)
   '(org-directory "~/Workspace/org/")
   '(org-agenda-files (list org-directory))
   '(org-archive-location "archived/%s::")
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
   '(org-agenda-window-setup 'current-window)
   ;; add extra WAIT and CANCELED todo states and logging with notes
   '(org-todo-keywords (quote ((sequence "TODO(t!)" "EPIC(E!)" "WAIT(w@/!)" "|" "DONE(d@)" "CANCELED(c@/!)"))))
   '(org-tag-persistent-alist
     '(("TARGET" . ?t)
       (:startgroup . nil)
       ("NEXT" . ?n)
       ("SOMEDAY" . ?s)
       (:endgroup . nil)))
   '(org-tags-exclude-from-inheritance (quote ("TARGET")))
   ;; set ARCHIVE tag when todo state is set to CANCELED, and remove when reset to TODO
   '(org-todo-state-tags-triggers
     (cons 'quote (list (cons 'todo (list (cons org-archive-tag nil)))
			(cons "CANCELED" (list (cons org-archive-tag t)))
			(cons "EPIC" (list (cons "TARGET" t))))))
   ;; REFILE BEHAVIOUR
   '(org-refile-targets
     '((nil . (:level . 1))
       (nil . (:tag . "TARGET"))))
   '(org-refile-use-outline-path t)
   '(org-outline-path-complete-in-steps t)
   ;; prefer in-steps that ido for refile completion
   ;; '(org-completion-use-ido t)
   '(org-reverse-note-order t)

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

   ;; DEPENDENCIES
   '(org-enforce-todo-dependencies t)
   ;;'(org-enforce-todo-checkbox-dependencies t) ; TEST there is an unreported bug with checkbox dependencies
   '(org-agenda-dim-blocked-tasks t)

   ;; CUSTOM COMMANDS
   '(org-agenda-custom-commands
     '(("c" . "Custom commands")
       ("ca" . "Agenda commands")
       ("cad" "Daily agenda (ignore SOMEDAY)"
	((agenda "")
	 (tags-todo "NEXT")
	 (todo "WAIT")
	 (todo "EPIC"))
	((org-agenda-tag-filter-preset '("-SOMEDAY"))
	 (org-agenda-compact-block t)
	 (org-agenda-time-grid nil)))
       ("caw" "Weekly agenda (ignore SOMEDAY)"
	((agenda "")
	 (tags-todo "NEXT")
	 (todo "WAIT")
	 (todo "EPIC"))
	((org-agenda-tag-filter-preset '("-SOMEDAY"))
	 (org-agenda-span 'week)
	 (org-agenda-compact-block t)
	 (org-agenda-time-grid nil)))
       ("ct" . "Task commands")
       ("ctc" . "Filter tasks by CATEGORY")
       ("ctci" "INBOX tasks"
	((alltodo ""))
	((org-agenda-category-filter-preset '("+INBOX"))))
       ("ctcf" "INFRA tasks"
	((alltodo ""))
	((org-agenda-category-filter-preset '("+INFRA"))))
       ("ctcr" "ROLES tasks"
	((alltodo ""))
	((org-agenda-category-filter-preset '("+ROLES"))))
       ("ctw" "Tasks in WAIT state" todo "WAIT")
       ("cte" "Tasks in EPIC state" todo "EPIC")
       ("ctn" "NEXT tasks" tags-todo "NEXT")
       ("cts" "SOMEDAY tasks" tags-todo "SOMEDAY")))

   ;; CAPTURE TEMPLATES
   '(org-capture-templates
     (quote
      (("t" "Task")
       ("tt" "Task (Scheduled)" entry
	(file+headline "tasks.org" "INBOX")
	"* TODO %^{Title}\nSCHEDULED: %t\n%?")
       ("td" "Task (Scheduled, with Deadline)" entry
	(file+headline "tasks.org" "INBOX")
	"* TODO %^{Title}\nSCHEDULED: %t\nDEADLINE: %^{Deadline}t\n%?")
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
  :ensure t
  :bind
  (("C-c e f" . ediff-files)
   ("C-c e b" . ediff-buffers))
  :init
  (load-library "ediff")
  :config
  (custom-set-variables
   ;; due to bug with mixup of horizontally/vertically
   '(ediff-split-window-function 'split-window-horizontally)
   '(ediff-window-setup-function 'ediff-setup-windows-plain)))

(use-package magit
  :pin melpa-stable
  :ensure t
  :bind ("C-x C-m" . magit-status)
  :config
  (custom-set-variables
   '(magit-repository-directories (quote ("~/Workspace/dev")))
   '(magit-repository-directories-depth 1)))

(use-package undo-tree
  :pin gnu
  :ensure t
  :bind ("C-x u" . undo-tree-visualize)
  :config
  (custom-set-variables
   '(global-undo-tree-mode t)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package bm
  :ensure t
  :bind
  ((("C-c b n" . bm-next)
    ("C-c b p" . bm-previous)
    ("C-c b m" . bm-toggle)))
  :config
  (custom-set-variables
   '(bm-cycle-all-buffers t)
   '(bm-buffer-persistence t)
   '(bm-repository-file "bm.el")))

(use-package sunrise-commander
  :ensure t
  :bind (("C-x d" . sunrise)
	 ("C-x C-d" . sunrise-cd))
  :config
  (custom-set-variables
   '(sr-cursor-follows-mouse nil)
   '(sr-listing-switches "-lA --group-directories-first")
   '(sr-listing-switches "-la")
   '(sr-show-file-attributes t)
   '(sr-show-hidden-files t)
   '(sr-window-split-style (quote horizontal))
   '(sr-windows-default-ratio 80)))

(use-package clojure-mode
  :pin melpa-stable
  :ensure t
  :config
  (custom-set-variables
   '(clojure-defun-style-default-indent t)))

(use-package cider
  :pin melpa-stable
  :ensure t
  :bind
  (("C-c c i" . cider-jack-in)
   ("C-c c c" . cider-connect)
   ("C-c c q" . cider-quit))
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-mode-hook 'auto-complete-mode)
  (custom-set-variables
   '(cider-lein-command "lein")
   '(cider-prompt-for-symbol nil)
   '(cider-repl-use-pretty-printing t)
   ;; font-lock results in terrible performance
   '(cider-repl-use-clojure-font-lock nil)))

(use-package ac-cider
  :pin melpa-stable
  :ensure t)

(use-package cider-profile
  :ensure t)

(use-package sayid
  :pin melpa-stable
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'sayid-setup-package))

(use-package paredit
  :pin melpa-stable
  :ensure t
  :config
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode))

(use-package rainbow-delimiters :disabled t)
(use-package rainbow-blocks :disabled t)

(use-package haskell-mode
  :pin melpa-stable
  :ensure t)

(use-package scala-mode2 :disabled t)
(use-package sbt-mode :disabled t)

(use-package dockerfile-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package auctex
  :pin gnu
  :disabled t
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package json-mode
  :disabled t ; causes errors with compiling json-reformat saying: "attempt to inline hash-table-keys before it was defined"
  :ensure t)

(use-package protobuf-mode
  :pin melpa-stable
  :disabled t ; causes error "Symbol's function definition is void: set-difference"
  :ensure t)

(use-package monokai-theme
  :ensure t)
(use-package solarized-theme)
(use-package zenburn-theme)

