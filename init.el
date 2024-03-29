
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

(when (version<= "27.0.0" emacs-version)
  (setq package-enable-at-startup nil))

(custom-set-variables
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(use-dialog-box nil)
 '(calendar-week-start-day 1)
 '(calendar-holidays nil) ; remove default holiday entries from calendar
 '(visible-bell t)
 '(show-paren-mode t)
 '(column-number-mode t)
 '(visual-line-fringe-indicators
   '(left-curly-arrow right-curly arrow))
 '(bs-default-sort-name "by mode")
 '(indent-tabs-mode nil)
 '(require-final-newline t)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(make-backup-files nil)
 '(create-lockfiles nil)
 '(save-place-mode t)
 '(custom-file (locate-user-emacs-file "custom.el"))
 ;; straight.el customizations
 '(straight-use-package-by-default t)
 '(straight-vc-git-default-protocol (quote https)))

;; install use-package
(straight-use-package 'use-package)

(bind-key "C-x C-b" 'bs-show)
(bind-key "C-x C-p" 'list-processes)
(bind-key "C-x C-o" 'other-window)
(bind-key "C-x C-i" 'other-window)
(bind-key "C-x C-w" 'whitespace-mode)
(bind-key "C-x C-n"
 (if (version<= "26.0.50" emacs-version)
   'display-line-numbers-mode
   'linum-mode))

(defun executables-missing (&rest commands)
  "Check if any of COMMANDS are missing."
  (let ((result nil))
    (dolist (command commands result)
      (unless (executable-find command)
        (message "Necessary executable '%s' is not found on exec-path" command)
        (setq result t)))
    result))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (dolist (var (list "NVM_DIR" "JAVA_HOME"))
    (add-to-list (quote exec-path-from-shell-variables) var))
  (exec-path-from-shell-initialize))

(use-package god-mode
  :init
  (defvar god-modeline-active-face
    (list :background "firebrick" :box nil)
    "Face for modeline of active buffer while in god-mode.")
  (defvar god-modeline-inactive-face
    (list :background "darkred" :box nil)
    "Face for modeline of inactive buffer while in god-mode.")
  (defun god-modeline-enable ()
    (message "GOD mode on!")
    (face-remap-add-relative
     (quote mode-line)
     god-modeline-active-face)
    (face-remap-add-relative
     (quote mode-line-inactive)
     god-modeline-inactive-face))
  (defun god-modeline-disable ()
    (message "GOD mode off!")
    (face-remap-remove-relative
     (cons
      (quote mode-line)
      god-modeline-active-face))
    (face-remap-remove-relative
     (cons
      (quote mode-line-inactive)
      god-modeline-inactive-face)))
  (defvar god-tab-command-alist nil
    "Associatiation list for major-mode specific command to execute for TAB key while in god-mode")
  (defun god-tab-command ()
    (interactive)
    (let* ((mode
            (apply (function derived-mode-p)
             (mapcar (function car) god-tab-command-alist)))
           (command
            (and mode (alist-get mode god-tab-command-alist))))
      (if command (or (funcall command) (indent-for-tab-command))
        (indent-for-tab-command))))
  :bind
  ("C-x C-0" . delete-window)
  ("C-x C-1" . delete-other-windows)
  ("C-x C-2" . split-window-below)
  ("C-x C-3" . split-window-right)
  ("C-x C-z") ; un-bind suspend-frame
  ;; re-bind suspend-frame such that
  ;; it's harder to press accidentally
  ("<escape>" . suspend-frame)
  ("C-z" . god-mode-all)
  (:map god-local-mode-map
   ("TAB" . god-tab-command)
   ("[" . backward-paragraph)
   ("]" . forward-paragraph)
   ("C-S-Z" . repeat))
  :hook
  (after-init . god-mode-all)
  (god-mode-enabled . god-modeline-enable)
  (god-mode-disabled . god-modeline-disable)
  :custom
  (god-mode-enable-function-key-translation nil))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  :bind
  ("C-S-V" . vertico-repeat))

(use-package prescient
  :custom
  (prescient-save-file
   (locate-user-emacs-file "prescient-save.el"))
  :config
  (add-to-list 'prescient-filter-method 'fuzzy)
  (prescient-persist-mode))

(use-package vertico-prescient
  :after (vertico prescient)
  :config
  (vertico-prescient-mode))

(use-package hydra
  :init
  (defhydra scale-window-hydra nil
    "scale current window"
    ("%" enlarge-window "enlarge horizontally")
    ("^" shrink-window "shrink horizontally")
    ("{" shrink-window-horizontally "shrink vertically")
    ("}" enlarge-window-horizontally "enlarge vertically"))
  (defhydra navigate-sexp-hydra nil
    "Navigate s-expressions"
    ("f" forward-sexp "forward")
    ("b" backward-sexp "backward")
    ("d" down-list "down")
    ("u" backward-up-list "up"))
  :bind
  ("C-x C-%" . scale-window-hydra/enlarge-window)
  ("C-x C-^" . scale-window-hydra/shrink-window)
  ("C-x C-{" . scale-window-hydra/shrink-window-horizontally)
  ("C-x C-}" . scale-window-hydra/enlarge-window-horizontally)
  ("C-M-f" . navigate-sexp-hydra/forward-sexp)
  ("C-M-b" . navigate-sexp-hydra/backward-sexp)
  ("C-M-d" . navigate-sexp-hydra/down-list)
  ("C-M-u" . navigate-sexp-hydra/backward-up-list)
  ("C-<" . hydra-pause-resume)
  (:map hydra-base-map
   ("<" . hydra-pause-resume)))

(use-package company
  :bind
  (:map company-mode-map
   ("TAB" . company-complete-common))
  (:map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  :demand t
  :hook
  (emacs-elisp-mode . company-mode)
  :custom
  (company-idle-delay 1)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  :hook
  (emacs-lisp-mode . company-mode)
  :config
  (company-tng-configure-default))

(use-package company-prescient
  :after (prescient company)
  :init
  (company-prescient-mode)
  :custom
  (company-prescient-sort-length-enable nil))

(use-package projectile
  :hook
  (projectile-grep-finished . (lambda () (pop-to-buffer next-error-last-buffer)))
  :bind
  ("C-c p" . projectile-command-map)
  :demand t
  :custom
  (projectile-use-git-grep t)
  :config
  (dolist (dir (list "node_modules" "target"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (projectile-mode))

(use-package project)

(use-package magit
  :after (project)
  :bind
  ("C-x C-m" . magit-status)
  ("C-x M-f" . magit-log-buffer-file)
  ("C-x M-b" . magit-blame-addition)
  :custom
  (magit-repository-directories (quote ("~/Workspace/dev")))
  (magit-repository-directories-depth 1)
  (magit-diff-refine-hunk 'all))

(use-package ediff
  :straight (:type built-in)
  :bind
  ("C-x M-d C-f" . ediff-files)
  ("C-x M-d C-b" . ediff-buffers)
  :init
  (load-library "ediff")
  :custom
  ;; due to bug with mixup of horizontally/vertically
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package undo-tree
  :bind ("C-x C-u" . undo-tree-visualize)
  :config
  (global-undo-tree-mode))

(use-package paredit
  :after (hydra)
  :init
  (defhydra paredit-hydra nil
    "Edit s-expressions"
    ("f" forward-sexp "forward")
    ("b" backward-sexp "backward")
    ("d" down-list "down")
    ("u" backward-up-list "up")
    ("k" kill-sexp "kill")
    ("y" yank "yank")
    ("Y" paredit-yank-pop "pop")
    ("S" paredit-split-sexp "split")
    ("J" paredit-join-sexps "join")
    ("P" paredit-splice-sexp "splice")
    ("W" paredit-wrap-sexp "wrap")
    ("]" paredit-forward-slurp-sexp "slurp forward")
    ("[" paredit-forward-barf-sexp "barf forward")
    ("{" paredit-backward-slurp-sexp "slurp backward")
    ("}" paredit-backward-barf-sexp "barf backward")
    ("RET" paredit-newline "newline")
    ("^" delete-indentation "delete indentation"))
  :bind
  (:map paredit-mode-map
   ("C-M-f" . paredit-hydra/forward-sexp)
   ("C-M-b" . paredit-hydra/backward-sexp)
   ("C-M-d" . paredit-hydra/down-list)
   ("C-M-u" . paredit-hydra/backward-up-list)
   ("C-M-k" . paredit-hydra/kill-sexp)
   ("C-M-y" . paredit-hydra/yank)
   ("C-M-Y" . paredit-hydra/paredit-yank-pop)
   ;; overrides paredit-backward-down
   ("C-M-S-S" . paredit-hydra/paredit-split-sexp)
   ("C-M-S-J" . paredit-hydra/paredit-join-sexps)
   ("C-M-S-P" . paredit-hydra/paredit-splice-sexp)
   ("C-M-S-W" . paredit-hydra/paredit-wrap-sexp)
   ("C-M-]" . paredit-hydra/paredit-forward-slurp-sexp)
   ("C-M-[" . paredit-hydra/paredit-forward-barf-sexp)
   ("C-M-{" . paredit-hydra/paredit-backward-slurp-sexp)
   ("C-M-}" . paredit-hydra/paredit-backward-barf-sexp)
   ("C-M-RET" . paredit-hydra/paredit-newline)
   ("C-M-^" . paredit-hydra/delete-indentation))
  :hook
  (lisp-mode . paredit-mode)
  (scheme-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  :config
  ;; interferes with god-mode paragraph navigation
  (unbind-key "[" paredit-mode-map)
  (unbind-key "]" paredit-mode-map)
  ;; intereferes with xref-find-references binding
  (unbind-key "M-?" paredit-mode-map))

(use-package rainbow-delimiters :disabled t)
(use-package rainbow-blocks :disabled t)

(use-package clojure-mode
  :hook
  (clojure-mode . paredit-mode)
  :custom
  (clojure-indent-style :always-indent))

(use-package cider
  :after (clojure-mode paredit company)
  :bind
  (:map clojure-mode-map
   ("C-c C-c" . cider-connect)
   ("C-c C-j" . cider-jack-in))
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
  :after (paredit company)
  :hook
  (geiser-mode . company-mode)
  (geiser-repl-mode . paredit-mode))

(use-package geiser-guile
  :straight
  (:local-repo "geiser-guile")
  :after (geiser)
  :custom
  (geiser-guile-load-init-file t))

(use-package haskell-mode)
(use-package kotlin-mode)
(use-package scala-mode)
(use-package sbt-mode)

(use-package js2-mode
  :mode
  ("\\.js\\'" . js2-mode)
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :custom
  (js-indent-level 2)
  :config
  (unbind-key "M-." js2-mode-map))

(use-package js2-refactor
  :after (js2-mode)
  :bind
  (:map js2-mode-map
   ("C-k" . js2r-kill))
  :hook
  (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package xref-js2
  :unless (executables-missing "ag")
  :after (js2-mode)
  :hook
  (xref-backend-functions
   xref-js2-xref-backend))

(use-package company-tern
  :unless (executables-missing "tern")
  :after (company js2-mode)
  :hook
  (js2-mode . company-mode)
  (js2-mode . tern-mode)
  :config
  (add-to-list 'company-backends 'company-tern)
  (unbind-key "M-." tern-mode-keymap)
  (unbind-key "M-," tern-mode-keymap))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :custom
  (typescript-indent-level 2))

(use-package indium
  :after (js2-mode)
  :hook
  (js2-mode . indium-interaction-mode)
  (indium-repl-mode . indium-switch-to-repl-buffer)
  :bind
  ("C-c i l" . indium-launch)
  ("C-c i c" . indium-connect)
  ("C-c i s" . indium-scratch)
  (:map indium-interaction-mode-map
   ("C-c C-q" . indium-maybe-quit)
   ("C-c C-k" . indium-eval-buffer)
   ("C-c C-e" . indium-eval-region))
  :custom
  ;; this is default anyway
  (indium-chrome-use-temporary-profile t))

(use-package jenkinsfile-mode)

(use-package dockerfile-mode)

(use-package systemd)

(use-package markdown-mode
  :unless (executables-missing "markdown")
  :hook
  (markdown-mode . markdown-live-preview-mode)
  (markdown-mode . visual-line-mode)
  :custom
  (markdown-live-preview-delete-export 'delete-on-export)
  :config
  (defalias 'markdown-add-xhtml-header-and-footer
    (lambda (title)
      "Wrap HTML5 header and footer with given TITLE around current buffer."
      (goto-char (point-min))
      (insert "<!DOCTYPE html5>\n"
	      "<html>\n"
	      "<head>\n<title>")
      (insert title)
      (insert "</title>\n")
      (insert "<meta charset=\"utf-8\" />\n")
      (when (> (length markdown-css-paths) 0)
	(insert (mapconcat 'markdown-stylesheet-link-string markdown-css-paths "\n")))
      (insert "\n</head>\n\n"
	      "<body>\n\n")
      (goto-char (point-max))
      (insert "\n"
	      "</body>\n"
	      "</html>\n"))))

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
  :custom
  (csv-align-style (quote auto)))

(use-package nhexl-mode
  :bind ("C-x M-x" . nhexl-mode))

(use-package vlf-setup
  :straight vlfi
  :custom
  (vlf-application 'ask))

(use-package nxml-mode
  :after (hydra)
  :straight (:type built-in)
  :init
  (defvar nxml-extensions
    (list "xml" "xsd" "sch" "rng" "xslt" "svg" "rss"))
  (add-to-list 'auto-mode-alist
   (cons
    ;; Generate efficient regex to match all extensions
    (concat "\\." (regexp-opt nxml-extensions t) "\\'")
    'nxml-mode))
  (defhydra navigate-nxml-hydra nil
    "Navigate XML elements"
    ("f" nxml-forward-element "forward")
    ("b" nxml-backward-element "backward")
    ("d" nxml-down-element "down")
    ("u" nxml-backward-up-element "up"))
  :bind
  (:map nxml-mode-map
   ("C-M-S-f" . navigate-nxml-hydra/nxml-forward-element)
   ("C-M-S-b" . navigate-nxml-hydra/nxml-backward-element)
   ("C-M-S-d" . navigate-nxml-hydra/nxml-down-element)
   ("C-M-S-u" . navigate-nxml-hydra/nxml-backward-up-element))
  :magic-fallback
  ("<\\?xml " . nxml-mode))

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

(use-package sunrise-commander
  :after (dired)
  :bind
  ("C-x d" . sunrise)
  ("C-x C-d" . sunrise-cd)
  (:map sr-mode-map
   ("C-n" . dired-next-line)
   ("C-p" . dired-previous-line))
  (:map
   dired-mode-map
   ("J" . dired-up-directory))
  :custom
  (sr-cursor-follows-mouse nil)
  (sr-listing-switches
   (let ((common "-lvhAD"))
     (if (string-equal system-type "gnu/linux")
         (string-join
          (list common
           "--time-style=long-iso"
           "--group-directories-first")
          " ")
       common)))
  (sr-show-file-attributes t)
  (sr-show-hidden-files t)
  (sr-confirm-kill-viewer nil)
  (sr-window-split-style (quote horizontal))
  (sr-start-hook nil)
  :config
  ;; unbind scroll-down/up keys, as -up binding interferes with move-end-of-line in global-map,
  ;; and C-v and M-v is good enough for scrolling anyway
  (unbind-key "C-e" sr-mode-map)
  (unbind-key "C-y" sr-mode-map))

(use-package sunrise-x-loop
  :straight sunrise-commander
  :config
  (unbind-key "C-n" sr-tabs-mode-map)
  (unbind-key "C-p" sr-tabs-mode-map))

(use-package sunrise-x-popviewer
  :straight sunrise-commander
  :config
  (sr-popviewer-mode)
  (unbind-key "v" sr-mode-map)
  (unbind-key "o" sr-mode-map)
  (unbind-key "C-c TAB" sr-mode-map)
  (unbind-key "<C-tab>" sr-mode-map))

(use-package modus-themes
  :straight
  (:protocol https)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-mode-line
   (quote (borderless accented)))
  (modus-themes-region
   (quote (bg-only)))
  (modus-themes-paren-match
   (quote (bold intense)))
  (modus-themes-org-blocks
   (quote gray-background))
  :config
  (load-theme (quote modus-vivendi) t))

(use-package all-the-icons
  :if (display-graphic-p)
  :custom
  ;; https://github.com/domtronn/all-the-icons.el/issues/28
  (inhibit-compacting-font-caches t)
  :config
  (let ((families ;; install fonts automatically if missing
         (list "all-the-icons" "FontAwesome" "file-icons"
               "github-octicons" "Weather Icons")))
    (unless (cl-every (lambda (f) (find-font (font-spec :family f))) families)
      (all-the-icons-install-fonts t))))

(use-package doom-modeline
  :after (all-the-icons)
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-modal nil)
  (doom-modeline-height 20)
  (doom-modeline-bar-width 1)
  :config
  ;; https://github.com/seagle0128/doom-modeline/issues/115
  (set-face-background 'doom-modeline-bar-inactive (face-background 'mode-line-inactive))
  (set-face-background 'doom-modeline-bar (face-background 'mode-line)))

(use-package org
  :after (hydra)
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
  (defhydra navigate-org-hydra nil
    "Navigate Org headlines"
    ("f" org-forward-heading-same-level "forward")
    ("b" org-backward-heading-same-level "backward")
    ("d" org-down-element "down")
    ("u" org-up-element "up")
    ("TAB" org-cycle "cycle"))
  :bind
  ("C-c a" . org-agenda)
  ("C-c k" . org-capture)
  (:map org-mode-map
   ("C-c M-h" . org-archive-done-tasks)
   ("C-c C-e" . org-babel-execute-src-block)
   ("C-M-f" . navigate-org-hydra/org-forward-element)
   ("C-M-b" . navigate-org-hydra/org-backward-element)
   ("C-M-d" . navigate-org-hydra/org-down-element)
   ("C-M-u" . navigate-org-hydra/org-up-element)
   ("C-M-TAB" . navigate-org-hydra/org-cycle))
  :hook
  (outline-mode . visual-line-mode)
  :config
  (add-to-list
   (quote god-tab-command-alist)
   (quote (org-mode . org-cycle)))
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
     (quote ((sequence "TODO(t!)" "WAITING(w@)" "ONGOING(o!)" "|" "DONE(d@)" "CANCELED(c@)"))))
   '(org-todo-keyword-faces
     (quote (("WAITING" . "purple")
             ("ONGOING" . "orange")
             ("CANCELED" . "dark gray"))))
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
     '((agenda time-up deadline-up scheduled-up)
       (todo category-up priority-down)
       (tags category-up priority-down)
       (search category-up)))

   ;; DEPENDENCIES
   '(org-enforce-todo-dependencies t)
   ;;'(org-enforce-todo-checkbox-dependencies t)
   '(org-agenda-dim-blocked-tasks t)

   ;; CUSTOM COMMANDS
   '(org-agenda-custom-commands
     '(("c" . "Custom commands")
       ("b" "Backlog (tasks not scheduled)" todo "TODO"
        ((org-agenda-skip-function
          '(org-agenda-skip-entry-if 'scheduled))))
       ("f" "Focussed tasks"
        ((todo "ONGOING"
          ((org-agenda-sorting-strategy '(priority-down))))
         (todo "WAITING"
          ((org-agenda-sorting-strategy '(category-up))))))
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
       ("tt" "Task (Unscheduled)" entry
        (file+headline "tasks.org" "INBOX")
        "* TODO %?")
       ("ts" "Task (Scheduled from today)" entry
        (file+headline "tasks.org" "INBOX")
        "* TODO %?\nSCHEDULED: %t\n")
       ("td" "Task (Scheduled, with Deadline)" entry
        (file+headline "tasks.org" "INBOX")
        "* TODO %?\nSCHEDULED: %^{Schedule}t DEADLINE: %^{Deadline}t\n")
       ("e" "Event")
       ("et" "Event (with single datetime)" entry
        (file+headline "events.org" "INBOX")
        "* %?\n%^T\n")
       ("er" "Event (with date range)" entry
        (file+headline "events.org" "INBOX")
        "* %?\n%^{Start}t--%^{End}t\n")
       ("m" "Memo" entry
        (file+headline "memo.org" "INBOX")
        "* %?\n%T\n")
       ("x" "Example" entry
        (file+headline "example.org" "INBOX")
        "* %?\n%^t\n"))))))

(use-package ox-confluence
  :straight org-contrib
  :bind
  (:map org-mode-map
   ("C-c x J" . org-confluence-export-as-confluence)))

(server-start)
