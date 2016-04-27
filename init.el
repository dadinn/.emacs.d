(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("sunrise-cmd" . "http://joseito.republika.pl/sunrise-commander/") t)
(package-initialize)

(setq custom-file "custom.el")

(defun min-emacs-version (&rest min-ver-list)
  (let ((verno (nth 2 (split-string (emacs-version) " "))))
    (let ((verno-list (mapcar 'string-to-number (split-string verno "\\."))))
      (version-list-<= min-ver-list verno-list))))

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
   ("C-c a" . org-agenda))
  :config
  (progn
    (add-hook 'text-mode-hook 'auto-fill-mode)
    (custom-set-variables
     '(org-agenda-span 'day)
     '(calendar-week-start-day 1)
     '(org-startup-indented t)
     '(org-directory "~/Documents/org")
     '(org-archive-location "archived/%s_archived::")
     '(org-agenda-files (quote ("tasks.org" "events.org")))
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
     '(org-lowest-priority 67)
     '(org-completion-use-ido t)
     '(org-refile-use-outline-path t)
     '(org-outline-path-complete-in-steps nil)
     '(org-priority-start-cycle-with-default nil)
     '(org-todo-keywords (quote ((sequence "TODO" "|" "DONE" "CANCELED"))))
     '(org-agenda-diary-file "diary.org")
     '(org-capture-templates
       (quote
	(("t" "Task" entry
	  (file+headline "tasks.org" "INBOX")
	  "* TODO %?
SCHEDULE: %t")
	 ("m" "Memo" entry
	  (file+headline "memo.org" "INBOX")
	  "* %?%T")
	 ("e" "Event" entry
	  (file+headline "events.org" "INBOX")
	  "* %?%^T")))))
    (defun org-archive-done-tasks ()
      (interactive)
      (org-map-entries
       (lambda ()
	 (org-archive-subtree)
	 (setq org-map-continue-from (outline-previous-heading)))
       "/DONE|CANCELLED" 'agenda))))

(use-package magit
  :ensure t
  :bind ("C-c C-s" . magit-status)
  :config
  (custom-set-variables
   '(magit-repository-directories (quote ("~/Workspace/dev")))
   '(magit-repository-directories-depth 1)))

(use-package undo-tree
  :ensure t
  :bind ("C-x u" . undo-tree-visualize)
  :config
  (custom-set-variables
   '(global-undo-tree-mode)))

(use-package sunrise-commander
  :ensure sunrise-commander
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
  :ensure t
  :config
  (customize-set-variable 'clojure-defun-style-default-indent t))

(use-package cider
  :ensure t
  :bind
  ("C-c C-i" . cider-jack-in)
  ("C-c C-q" . cider-quit)
  :config
  (progn
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (custom-set-variables
      '(cider-lein-command "/opt/bin/lein")
      '(cider-prompt-for-symbol nil))))

(use-package ac-cider
  :ensure t)

(use-package paredit
  :ensure t
  :config
  (progn
    (defun enable-paredit-mode ()
      (paredit-mode t))
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)))

(use-package haskell-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package rainbow-delimiters)
(use-package rainbow-blocks)

(use-package scala-mode2)
(use-package sbt-mode)
(use-package haskell-mode)
(use-package markdown-mode)
(use-package json-mode)
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(use-package solarized-theme)

(define-key global-map "\C-x\C-b" 'bs-show)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-week-start-day 1)
 '(cider-lein-command "/opt/bin/lein")
 '(cider-prompt-for-symbol nil)
 '(clojure-defun-style-default-indent t)
 '(custom-safe-themes
   (quote
    ("795d8a0785b16437ee67da091c2c684e4149a12991199c7f5ae4b91637ea0c9c" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" default)))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode t nil (ido))
 '(inhibit-startup-screen t)
 '(magit-repo-dirs (quote ("~/Documents/dev")))
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(org-agenda-diary-file (quote diary-file) nil nil "This is an interesting option")
 '(org-agenda-files (quote ("~/Documents/org/tasks" "~/Documents/org/events")))
 '(org-agenda-skip-deadline-prewarning-if-scheduled nil)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-span (quote day))
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-agenda-window-setup 'current-window)
 '(org-archive-location "archived/%s_archived::")
 '(org-capture-templates
   (quote
    (("t" "Task" entry
      (file+headline "tasks/default.org" "INBOX")
      "* TODO %?")
     ("b" "BUY" entry
      (file+headline "tasks/buy.org" "INBOX")
      "* TODO %?")
     ("e" "Event" entry
      (file+headline "events/default.org" "INBOX")
      "* %?"))))
 '(org-completion-use-ido t)
 '(org-default-priority 66)
 '(org-directory "~/Documents/org")
 '(org-enforce-todo-dependencies t)
 '(org-highest-priority 65)
 '(org-lowest-priority 67)
 '(org-outline-path-complete-in-steps nil)
 '(org-priority-start-cycle-with-default nil)
 '(org-refile-use-outline-path t)
 '(org-startup-indented t)
 '(org-todo-keywords (quote ((sequence "TODO" "|" "DONE" "CANCELED"))))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
