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
 '(custom-file "~/.emacs.d/custom.el")
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))

(define-key global-map "\C-x\C-b" 'bs-show)

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

(use-package org
  :ensure t
  :bind
  (("C-c k" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c C-c e" . org-babel-execute-src-block))
  :config
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (custom-set-variables
   '(org-agenda-span 'day)
   '(calendar-week-start-day 1)
   '(org-startup-indented t)
   '(org-directory "~/Documents/org")
   '(org-archive-location "archived/%s_archived::")
   '(org-agenda-diary-file "diary.org")
   '(org-agenda-include-diary t)
   '(org-agenda-files (list org-directory))
   '(org-agenda-skip-unavailable-files t)
   '(org-agenda-skip-deadline-prewarning-if-scheduled nil)
   '(org-agenda-skip-scheduled-if-deadline-is-shown t)
   '(org-agenda-skip-scheduled-if-done t)
   '(org-agenda-tags-todo-honor-ignore-options t)
   '(org-agenda-todo-ignore-scheduled 'future)
   '(org-agenda-window-setup 'current-window)
   '(org-enforce-todo-dependencies t)
   '(org-default-priority 66)
   '(org-highest-priority 65)
   '(org-lowest-priority 70)
   '(org-completion-use-ido t)
   '(org-refile-use-outline-path t)
   '(org-outline-path-complete-in-steps nil)
   '(org-priority-start-cycle-with-default nil)
   '(org-todo-keywords (quote ((sequence "TODO" "|" "DONE" "CANCELED"))))
   '(org-todo-state-tags-triggers ("CANCELED" ("ARCHIVE" . t)))
   '(org-capture-templates
     (quote
      (("t" "Task" entry
	(file+headline "tasks.org" "INBOX")
	"* TODO %?
SCHEDULED: %t")
       ("m" "Memo" entry
	(file+headline "memo.org" "INBOX")
	"* %?%T")
       ("e" "Event" entry
	(file+headline "events.org" "INBOX")
	"* %?
%^T")))))
  (defun org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE|CANCELLED" 'agenda)))

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
   '(global-undo-tree-mode)))

(use-package sunrise-commander
  :ensure t
  :bind (("C-x d" . sunrise)
	 ("C-x C-d" . sunrise-cd))
  :config
  (custom-set-variables
   '(sr-cursor-follows-mouse nil)
   '(sr-listing-switches "-lA --group-directories-first")
   '(sr-show-file-attributes t)
   '(sr-show-hidden-files t)
   '(sr-window-split-style (quote horizontal))
   '(sr-windows-default-ratio 80)))

(use-package clojure-mode
  :pin melpa-stable
  :ensure t
  :config
  (customize-set-variable
   'clojure-defun-style-default-indent t))

(use-package cider
  :pin melpa-stable
  :ensure t
  :bind
  ("C-c C-i" . cider-jack-in)
  ("C-c C-q" . cider-quit)
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-mode-hook 'auto-complete-mode)
  (custom-set-variables
   '(cider-lein-command "/opt/bin/lein")
   '(cider-prompt-for-symbol nil)))

(use-package ac-cider
  :pin melpa-stable
  :ensure t)

(use-package paredit
  :pin melpa-stable
  :ensure t
  :config
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode))

(use-package rainbow-delimiters)
(use-package rainbow-blocks)

(use-package haskell-mode
  :pin melpa-stable
  :ensure t)

(use-package scala-mode2)
(use-package sbt-mode)

(use-package dockerfile-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package auctex
  :pin gnu
  :disabled t
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

