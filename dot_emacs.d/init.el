;; Emacs configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;; Add the NonGNU ELPA package archive
(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives  '("MELPA"  . "https://melpa.org/packages/"))
(unless package-archive-contents  (package-refresh-contents))

;; Install and require use-package
;; THIS ONLY WORKS IN EMACS 29
(require 'use-package)
(setq use-package-always-ensure t)

(dolist (mode
         '(menu-bar-mode         ;; Disable the menu bar
           tool-bar-mode         ;; Disable the tool bar
           scroll-bar-mode       ;; Disable the scroll bars
           blink-cursor-mode))   ;; Disable the blinking cursor
  (funcall mode -1))

;; Disable splash screen
(setq inhibit-startup-screen t)

;; For navigating wrapped lines
(global-visual-line-mode t)

;; Enable and use relative line numbering
(display-line-numbers-mode)
(setq display-line-numbers 'relative)

;; Automatically pair parentheses
(electric-pair-mode t)

;; Use 2 spaces only for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; Smooth scrolling - That is, scroll only one line
(setq scroll-step            1
  scroll-conservatively  10000)

;; Fonts
(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrains Mono-12"))
;; Use a variable pitch, keeping fixed pitch where it's sensible
(use-package mixed-pitch ;; For auto detecting when to use variable pitch
  :defer t
  :hook (text-mode . mixed-pitch-mode)
  :config
  (when (member "Source Serif Pro" (font-family-list))
    (set-face-attribute 'variable-pitch nil :family "Source Serif Pro")))
;; Pretty simbols
(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("delta" . ?Δ)
                                       ("gamma" . ?Γ)
                                       ("phi" . ?φ)
                                       ("psi" . ?ψ)))

;; Install icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Doom Themes
(use-package doom-themes
  :ensure t
  :config
  ;; Load the desired theme
  (load-theme 'doom-tokyo-night t)  ; Replace 'doom-one with your preferred theme
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15))

;; A startup screen extracted from Spacemacs
(use-package dashboard
  :config
  (setq dashboard-projects-backend 'project-el
        dashboard-banner-logo-title nil
        dashboard-center-content t
        dashboard-set-footer nil
        dashboard-page-separator "\n\n\n"
        dashboard-items '((projects . 15)
                          (recents  . 15)
                          (bookmarks . 5)))
  (dashboard-setup-startup-hook))

;; Eglot - LSP Support
(use-package eglot
  :ensure t
  :hook (prog-mode . eglot-ensure)
  :init (defalias 'start-lsp-server #'eglot))

;; Flymake - Inline static analysis
(use-package flymake
  :ensure t
  :hook (prog-mode . flymake-mode)
  :config
  (setq help-at-pt-display-when-idle t))

;; Message navigation bindings
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error))

;; Projectile - Project Interaction Library for Emacs
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien)
  (setq projectile-project-search-path '("~/GitHub/" "~/Documents/GitHub/" "~/.local/share"))
  (setq projectile-enable-caching t))

;; Helm - Framework for incremental completions and narrowing selections
(use-package helm
  :ensure t
  :demand t
  :bind (("M-x" . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-c j" . helm-occur)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         ("C-j" . helm-next-line)       ; Bind C-j to move down
         ("C-k" . helm-previous-line)   ; Bind C-k to move up
         ("<escape>" . helm-keyboard-quit))   ; Bind Esc to quit
  :config
  (helm-mode 1))

;; Helm general settings
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t
      helm-locate-fuzzy-match     t
      helm-semantic-fuzzy-match   t
      helm-imenu-fuzzy-match      t
      helm-completion-in-region-fuzzy-match t
      helm-candidate-number-list  50
      helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-ff-file-name-history-use-recentf t)

;; Helm-ag for better searching
(use-package helm-ag
  :ensure t
  :bind (("C-c i" . helm-imenu))
  :config (setq helm-ag-base-command "rg --no-heading"))

;; Helm-projectile
(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on))

;; Prespective - for a "tmux session per project"-like experience
(use-package perspective
  :ensure t
  :config
  (persp-mode))

;; Magit - Git client
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :config (setq magit-diff-refine-hunk t))

;; Diff-hl - Indication of local VCS changes
(use-package diff-hl
  :ensure t
  :hook (prog-mode . diff-hl-mode)
  :config (diff-hl-flydiff-mode t))

;; Display available keybindings in popup
(use-package which-key
  :config
  (which-key-mode 1))

;; Keychorde -- Needed for escaping evil with jj and jk
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

;; Evil - Vim Emulation
(use-package evil
  :ensure t
  :config
  (evil-mode t)
  ;;; Define costum functions to peform personalised actions
  ;;;; Keep selection after indent
  (defun jf/evil-shift-right ()
  (interactive)
    (evil-shift-right evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))
  ;;;; Keep selection after unindent
  (defun jf/evil-shift-left ()
    (interactive)
    (evil-shift-left evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))
  ;;; Define shortcuts
  ;;;; Define shortcuts for indent and unindent
  (define-key evil-visual-state-map (kbd "TAB") 'jf/evil-shift-right)
  (define-key evil-visual-state-map (kbd "<BACKTAB>") 'jf/evil-shift-left)
  ;;;; Unbind C-h in global map
  (global-unset-key (kbd "C-h"))
  ;;;; Escape insert with jj and jk
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  ;;;; Window navigation using Ctrl + h/j/k/l
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  ;;;; Make Exceptions for NeoTree
  (evil-define-key 'normal neotree-mode-map
    "j" 'neotree-next-line
    "k" 'neotree-previous-line
    "l" 'neotree-enter
    "h" 'neotree-collapse-all
    "m" 'neotree-rename-node
    "d" 'neotree-delete-node
    "c" 'neotree-copy-node
    "a" 'neotree-create-node
    "S-h" 'neotree-hidden-file-toggle
    "h" 'neotree-collapse-all)
  :hook (prog-mode . evil-local-mode))

;; NeoTree - The fle tree
(use-package neotree
  :ensure t
  :config
  ;; Set up your preferred neo-tree settings here
  (general-define-key
    :keymaps 'neotree-mode-map
    "j" 'neotree-next-line
    "k" 'neotree-previous-line
    "l" 'neotree-enter
    "h" 'neotree-hide)
  )

;; Keybindings
(use-package general
  :ensure t
  :config
  ;; Define 'SPC' as the leader key
  (general-create-definer my-leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  
  ;; Leader keybinding for toggling neo-tree
  (my-leader-def
    "o" '(neotree-toggle :which-key "toggle neo-tree"))

  ;; Define leader keybindings
  (my-leader-def
    "f"  '(:ignore t :which-key "find")
    "ff" '(helm-find-files :which-key "find file")
    "fg" '(helm-do-grep-ag :which-key "grep")
    "fb" '(helm-buffers-list :which-key "buffer")
    "fl" '(helm-occur :which-key "line")
    "fo" '(helm-imenu :which-key "outline")
    "fr" '(helm-recentf :which-key "recent file")
    "fi" '(helm-imenu :which-key "imenu")
    "fs" '(helm-do-grep-ag :which-key "search with ag")))

;; Additional language support
(use-package go-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package sly
  :ensure t)

;; AUCTeX - LaTeX support
(use-package auctex
  :ensure t
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . reftex-mode))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil))

;; Markdown mode
(use-package markdown-mode
  :ensure t)

;; Org mode - Outline-based notes management and organizer
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)))

(use-package org-contrib
  :ensure t)

;; EditorConfig support
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode t))

;; EAT - In-Emacs Terminal Emulation
(use-package eat
  :ensure t
  :config
  (setq eat-kill-buffer-on-exit t
        eat-enable-mouse t))

;; Miscellaneous options
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)

;; Store automatic customization options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
