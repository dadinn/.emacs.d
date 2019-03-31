;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")
     ;; ("marmalade" . "https://marmalade-repo.org/packages/") ; Errors with TLS connection
     ("orgmode" . "http://orgmode.org/elpa/")))
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
(define-key global-map "\C-c\C-w" 'whitespace-mode)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (use-package-always-pin "melpa-stable"))

(use-package el-get
  :config
  (add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")
  ;;TODO: https://github.com/dimitri/el-get/issues/2649
  (if (not (file-directory-p el-get-dir))
      (make-directory el-get-dir)))

(use-package use-package-el-get
  :config (use-package-el-get-setup))

(use-package ido
  :custom
  (ido-enable-flex-matching t)
  (ido-auto-merge-work-directories-length -1)
  (ido-everywhere t)
  (ido-mode t))

(use-package ido-vertical-mode
  :custom
  (ido-vertical-mode t)
  (ido-vertical-show-count t)
  (ido-vertical-pad-list nil)
  (ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package flx-ido
  :custom
  (ido-enable-flex-matching t)
  (ido-use-faces nil))

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode))

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
   ("C-c e" . org-babel-execute-src-block))
  :hook
  (text-mode . visual-line-mode)
  :config
  (custom-set-variables
   '(org-agenda-span 'day)
   '(org-startup-indented t)
   '(org-directory "~/Workspace/org/")
   '(org-agenda-files (list org-directory))
   '(org-archive-location "archives/datetree.org::datetree/* %s")
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
       ("MAYBE" . ?m)
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
       ("cad" "Daily agenda (ignore MAYBE)"
	((agenda "")
	 (tags-todo "NEXT")
	 (todo "WAIT")
	 (todo "EPIC"))
	((org-agenda-tag-filter-preset '("-MAYBE"))
	 (org-agenda-compact-block t)
	 (org-agenda-time-grid nil)))
       ("caw" "Weekly agenda (ignore MAYBE)"
	((agenda "")
	 (tags-todo "NEXT")
	 (todo "WAIT")
	 (todo "EPIC"))
	((org-agenda-tag-filter-preset '("-MAYBE"))
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
       ("cts" "MAYBE tasks" tags-todo "MAYBE")))

   ;; CAPTURE TEMPLATES
   '(org-capture-templates
     (quote
      (("t" "Task")
       ("tt" "Task (Scheduled)" entry
	(file+headline "tasks.org" "INBOX")
	"* TODO %^{Title}\nSCHEDULED: %t\n%?")
       ("td" "Task (Scheduled, with Deadline)" entry
	(file+headline "tasks.org" "INBOX")
	"* TODO %^{Title}\nSCHEDULED: %^{Schedule}t\nDEADLINE: %^{Deadline}t\n%?")
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
  (("C-c e f" . ediff-files)
   ("C-c e b" . ediff-buffers))
  :init
  (load-library "ediff")
  :custom
  ;; due to bug with mixup of horizontally/vertically
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  :bind ("C-x C-m" . magit-status)
  :custom
  (magit-repository-directories (quote ("~/Workspace/dev")))
  (magit-repository-directories-depth 1))

(use-package undo-tree
  :pin "gnu"
  :bind ("C-x u" . undo-tree-visualize)
  :custom
  (global-undo-tree-mode t))

(use-package paredit
  :hook
  (lisp-mode . paredit-mode)
  (scheme-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  :init (show-paren-mode))

(use-package projectile
  :bind
  (:map projectile-mode-map
   ("C-c p" . projectile-command-map))
  :custom
  (projectile-grep-finished-hook
   '((lambda ()
       (pop-to-buffer next-error-last-buffer))))
  :init
  (projectile-mode))

(use-package bm
  :bind
  (("C-c b t" . bm-toggle)
   ("C-c b n" . bm-next)
   ("C-c b p" . bm-previous)
   ("C-c b s" . bm-show-all)
   ("C-c b x" . bm-remove-all-all-buffers)
   :map bm-show-mode-map
   ("n" . bm-show-next)
   ("p" . bm-show-prev))
  :custom
  (bm-annotate-on-create t)
  (bm-show-buffer-name "*Bookmarks*")
  (bm-cycle-all-buffers t)
  (bm-buffer-persistence t)
  (bm-repository-file "bookmarks.el"))

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
  (require 'sunrise-x-popviewer)
  (sr-popviewer-mode))

(use-package clojure-mode
  :hook
  (clojure-mode . paredit-mode)
  :custom
  (clojure-indent-style :always-indent))

(use-package cider
  :bind
  (("C-c c c" . cider-connect)
   ("C-c c j" . cider-jack-in)
   :map cider-mode-map
   ("C-c c q" . cider-quit))
  :hook
  (cider-mode . eldoc-mode)
  (cider-mode . auto-complete-mode)
  (cider-repl-mode . paredit-mode)
  :custom
  (cider-lein-command "lein")
  (cider-prompt-for-symbol nil)
  (cider-repl-use-pretty-printing t)
  ;; font-lock results in terrible performance
  (cider-repl-use-clojure-font-lock nil))

(use-package ac-cider)

(use-package sayid
  :config
  (eval-after-load 'clojure-mode '(sayid-setup-package)))

(use-package rainbow-delimiters :disabled t)
(use-package rainbow-blocks :disabled t)

(use-package haskell-mode)

(use-package scala-mode2 :disabled t)
(use-package sbt-mode :disabled t)

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

