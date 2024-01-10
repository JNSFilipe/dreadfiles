;;; -*- lexical-binding: t -*-

(defun tangle-init ()
  "If the current buffer is init.org the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(add-hook
 'after-init-hook
 (lambda ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file))
     (when custom-file
       (load-file custom-file))
     (server-start))))

(require 'use-package)
(setq use-package-always-ensure t)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 5)
        ("MELPA Stable" . 0)))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq auto-revert-interval 1            ; Refresh buffers fast
      default-input-method "TeX"        ; Use TeX when toggling input method
      echo-keystrokes 0.1               ; Show keystrokes asap
      enable-recursive-minibuffers t    ; Allow recursive minibuffers
      frame-inhibit-implied-resize 1    ; Don't resize frame implicitly
      inhibit-startup-screen t          ; No splash screen please
      initial-scratch-message nil       ; Clean scratch buffer
      recentf-max-saved-items 10000     ; Show more recent files
      ring-bell-function 'ignore        ; Quiet
      scroll-margin 1                   ; Space between cursor and top/bottom
      sentence-end-double-space nil     ; No double space
      custom-file                       ; Customizations in a separate file
      (concat user-emacs-directory "custom.el"))
;; Some mac-bindings interfere with Emacs bindings.
(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

(setq-default tab-width 2                       ; Smaller tabs
              fill-column 79                    ; Maximum line width
              truncate-lines t                  ; Don't fold lines
              indent-tabs-mode nil              ; Use spaces instead of tabs
              split-width-threshold 160         ; Split verticly by default
              split-height-threshold nil        ; Split verticly by default
              frame-resize-pixelwise t          ; Fine-grained frame resize
              auto-fill-function 'do-auto-fill) ; Auto-fill-mode everywhere

(let ((default-directory (concat user-emacs-directory "site-lisp/")))
  (when (file-exists-p default-directory)
    (setq load-path
          (append
           (let ((load-path (copy-sequence load-path)))
             (normal-top-level-add-subdirs-to-load-path)) load-path))))

(fset 'yes-or-no-p 'y-or-n-p)

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

(put 'narrow-to-region 'disabled nil)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

;;; Vim Bindings
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;;; Vim Bindings Everywhere else
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(add-to-list 'default-frame-alist '(internal-border-width . 24))

;; N Λ N O theme
(use-package nano-theme
  :init
  (setq nano-light-background "#fafafa"
        nano-light-highlight "#f5f7f8"))

(defun load-nano-theme (variant)
  (let ((theme (intern (concat "nano-" (symbol-name variant)))))
    (load-theme theme t)))

(load-nano-theme (if (boundp 'ns-system-appearance) ns-system-appearance 'light))

(when (boundp 'ns-system-appearance-change-functions)
  (add-hook 'ns-system-appearance-change-functions 'load-nano-theme))

(defun cycle-themes ()
  "Returns a function that lets you cycle your themes."
  (let ((themes '(nano-light nano-dark)))
    (lambda ()
      (interactive)
      ;; Rotates the thme cycle and changes the current theme.
      (let ((rotated (nconc (cdr themes) (list (car themes)))))
        (load-theme (car (setq themes rotated)) t))
      (message (concat "Switched to " (symbol-name (car themes)))))))

;; N Λ N O modeline
(use-package nano-modeline
  :after meow
  :init
  ;; Disable the default modeline
  (setq-default mode-line-format nil)
  :config
  (defun meow-nano-modeline-indicator ()
    "Create the status indicator for the modeline."
    (pcase (meow--current-state)
      ('normal (propertize " N " 'face (nano-modeline-face 'status-RO)))
      ('motion (propertize " M " 'face (nano-modeline-face 'status-RO)))
      ('insert (propertize " I " 'face (nano-modeline-face 'status-RW)))
      ('keypad (propertize " K " 'face (nano-modeline-face 'status-**)))
      ('beacon (propertize " B " 'face (nano-modeline-face 'status-**)))))

  (defun my-default-nano-modeline (&optional default)
    "My nano modeline configuration."
    (funcall nano-modeline-position
             `((nano-modeline-buffer-status)
               (meow-nano-modeline-indicator) " "
               (nano-modeline-buffer-name) " "
               (nano-modeline-git-info))
             `((nano-modeline-cursor-position)
               (nano-modeline-window-dedicated))
             default))
  (my-default-nano-modeline 1))

(when (member "Source Code Pro" (font-family-list))
  (set-face-attribute 'default nil :font "Source Code Pro-15"))

;; Use a variable pitch, keeping fixed pitch where it's sensible
(use-package mixed-pitch
  :defer t
  :hook (text-mode . mixed-pitch-mode)
  :config
  (when (member "Source Serif Pro" (font-family-list))
    (set-face-attribute 'variable-pitch nil :family "Source Serif Pro")))

(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("delta" . ?Δ)
                                       ("gamma" . ?Γ)
                                       ("phi" . ?φ)
                                       ("psi" . ?ψ)))

;; Minor mode for a nice writing environment
(use-package olivetti
  :defer t
  :bind (:map custom-bindings-map ("C-c o" . olivetti-mode))
  :config
  (setq-default olivetti-body-width (+ fill-column 3)))

(use-package adaptive-wrap
  :defer t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

;; Dim color of text in surrounding sections
(use-package focus
  :defer t
  :bind (:map custom-bindings-map ("C-c f" . focus-mode)))

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

(when (memq window-system '(mac ns))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        ns-pop-up-frames nil
        native-comp-async-report-warnings-errors nil))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package ls-lisp
  :ensure nil
  :if (memq window-system '(mac ns))
  :config
  (setq ls-lisp-use-insert-directory-program nil))

(use-package reveal-in-osx-finder
  :if (memq window-system '(mac ns)))

(dolist (mode
         '(abbrev-mode                  ; E.g. sopl -> System.out.println
           column-number-mode           ; Show column number in mode line
           delete-selection-mode        ; Replace selected text
           dirtrack-mode                ; directory tracking in *shell*
           global-so-long-mode          ; Mitigate performance for long lines
           recentf-mode                 ; Recently opened files
           show-paren-mode))            ; Highlight matching parentheses
  (funcall mode 1))

;; A Git porcelain inside Emacs.
(use-package magit
  :bind (:map custom-bindings-map ("C-c m" . magit-status)))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :config
  (global-diff-hl-mode 1))

(use-package project
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m)))

;; EditorConfig Emacs Plugin
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; VERTical Interactive COmpletion
(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (setq vertico-count 25))

;; Using posframe to show Vertico
(use-package vertico-posframe
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-width 100
        vertico-posframe-height vertico-count))

;; Save minibuffer history
(use-package savehist
  :init
  (savehist-mode 1))

;; Enrich existing commands with completion annotations
(use-package marginalia
  :init
  (marginalia-mode 1))

;; Modular text completion framework
(use-package corfu
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 2
        corfu-popupinfo-delay 0.5))

;; Emacs completion style that matches multiple regexps in any order
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic partial-completion)
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-component-separator "[ |]"))

;; Consulting completing-read
(use-package consult
  :bind (:map custom-bindings-map
              ("C-x b" . consult-buffer)
              ("C-c r" . consult-ripgrep)))

;; Emacs support library for PDF files
(use-package pdf-tools
  :defer t
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("c" . (lambda ()
                       (interactive)
                       (if header-line-format
                           (setq header-line-format nil)
                         (nano-modeline-pdf-mode))))
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

(defun cycle-languages ()
  "Changes the ispell dictionary to the first element in
ISPELL-LANGUAGES, and returns an interactive function that cycles
the languages in ISPELL-LANGUAGES when invoked."
  (let ((ispell-languages (list "american" "norsk")))
    (lambda ()
      (interactive)
      ;; Rotates the languages cycle and changes the ispell dictionary.
      (let ((rotated (nconc (cdr ispell-languages) (list (car ispell-languages)))))
        (ispell-change-dictionary (car (setq ispell-languages rotated)))))))

(use-package flyspell
  :defer t
  :if (executable-find "aspell")
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (local-set-key
                             (kbd "C-c l")
                             (cycle-languages)))))
  :config
  (ispell-change-dictionary "american" t))

;; display the definition of word at point
(use-package define-word
  :defer t
  :bind (:map custom-bindings-map ("C-c D" . define-word-at-point)))

(use-package lorem-ipsum)

;; Outline-based notes management and organizer
(use-package org
  :defer t
  :config
  (setq org-adapt-indentation t
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-src-fontify-natively t
        org-edit-src-content-indentation 0))

;; Convert font-lock faces to other formats
(use-package engrave-faces
  :defer t)

;; LaTeX Back-End for Org Export Engine
(use-package ox-latex
  :ensure nil
  :after org
  :config
  (setq org-export-allow-bind-keywords t
        org-latex-src-block-backend 'engraved
        org-latex-pdf-process
        '("latexmk -pdflatex='xelatex -shell-escape -interaction nonstopmode' -pdf -f %f"))

  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

  (add-to-list 'org-latex-classes
               '("ifimaster"
                 "\\documentclass{ifimaster}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
\\usepackage{babel,csquotes,ifimasterforside,url,varioref}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("easychair" "\\documentclass{easychair}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Working with Code Blocks in Org
(use-package ob
  :ensure nil
  :after org
  :config
  (setq org-export-use-babel nil
        org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (clojure . t))))

;; Babel Functions for Python
(use-package ob-python
  :ensure nil
  :after (ob python)
  :config
  (setq org-babel-python-command python-shell-interpreter))

;; Template expansion for Org structures
(use-package org-tempo
  :ensure nil
  :after org)

;; Modern looks for Org
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-block-fringe nil))

;; Org mode to send and reply to email in HTML
(use-package org-msg
  :after (org mu4e)
  :config
  (add-to-list 'mu4e-compose-pre-hook 'org-msg-mode)
  (setq org-msg-enforce-css (concat user-emacs-directory "email-style.css")
        org-msg-options "html-postamble:nil toc:nil num:nil author:nil email:nil"
        org-msg-default-alternatives '((new           . (text html))
                                       (reply-to-html . (text html))
                                       (reply-to-text . (text)))
        org-msg-signature "

#+begin_signature
#+begin_export html

- Lars
#+end_export
#+end_signature\n"))

;; Export Github Flavored Markdown from Org
(use-package ox-gfm
  :after (org))

;; Emacs Major mode for Markdown-formatted files
(use-package markdown-mode
  :defer t)

;; direnv integration
(use-package direnv
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode 1))

(use-package mu4e
  :ensure nil
  :defer t
  :if (and (file-exists-p "~/Maildir")
           (executable-find "mbsync")
           (executable-find "msmtp")
           (executable-find "mu"))
  :bind (:map custom-bindings-map ("C-x m" . mu4e))
  :config
  (setq
   mail-user-agent 'mu4e-user-agent
   user-full-name "Lars Tveito"            ; Your full name
   user-mail-address "larstvei@ifi.uio.no" ; And email-address

   sendmail-program (executable-find "msmtp")
   send-mail-function 'smtpmail-send-it

   message-sendmail-f-is-evil t
   message-sendmail-extra-arguments '("--read-envelope-from")
   message-send-mail-function 'message-send-mail-with-sendmail
   message-kill-buffer-on-exit t

   mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
   mu4e-change-filenames-when-moving t
   mu4e-maildir-shortcuts '(("/Inbox" . ?i) ("/Sent Items" . ?s))

   mu4e-sent-folder "/Sent Items"
   mu4e-trash-folder "/Deleted Items"
   mu4e-trash-folder "/Drafts"

   mu4e-use-fancy-chars t))

;; Interaction mode for ChatGPT
(use-package gptel
  :defer t
  :hook ((gptel-mode . (lambda () (visual-line-mode 1)))
         (gptel-mode . (lambda () (auto-fill-mode 0))))
  :init
  (setq gptel-default-mode 'org-mode
        gptel-model "gpt-4"
        gptel-api-key (auth-source-pick-first-password
                       :host "api.openai.com")))

;; Multiple cursors for Emacs
(use-package multiple-cursors
  :defer t
  :bind (:map custom-bindings-map
              ("C-c e" . mc/edit-lines)
              ("C-c a" . mc/mark-all-like-this)
              ("C-c n" . mc/mark-next-like-this)))

;; Increase selected region by semantic units
(use-package expand-region
  :defer t
  :vc (:url "git@github.com:magnars/expand-region.el.git" :rev :newest)
  :bind (:map custom-bindings-map ("C-=" . er/expand-region)))

;; Try out Emacs packages
(use-package try
  :defer t)

(defun cycle-spacing-delete-newlines ()
  "Removes whitespace before and after the point."
  (interactive)
  (if (version< emacs-version "24.4")
      (just-one-space -1)
    (cycle-spacing -1)))

(defun jump-to-symbol-internal (&optional backwardp)
  "Jumps to the next symbol near the point if such a symbol
exists. If BACKWARDP is non-nil it jumps backward."
  (let* ((point (point))
         (bounds (find-tag-default-bounds))
         (beg (car bounds)) (end (cdr bounds))
         (str (isearch-symbol-regexp (find-tag-default)))
         (search (if backwardp 'search-backward-regexp
                   'search-forward-regexp)))
    (goto-char (if backwardp beg end))
    (funcall search str nil t)
    (cond ((<= beg (point) end) (goto-char point))
          (backwardp (forward-char (- point beg)))
          (t  (backward-char (- end point))))))

(defun jump-to-previous-like-this ()
  "Jumps to the previous occurrence of the symbol at point."
  (interactive)
  (jump-to-symbol-internal t))

(defun jump-to-next-like-this ()
  "Jumps to the next occurrence of the symbol at point."
  (interactive)
  (jump-to-symbol-internal))

(defun kill-this-buffer-unless-scratch ()
  "Works like `kill-this-buffer' unless the current buffer is the
*scratch* buffer. In witch case the buffer content is deleted and
the buffer is buried."
  (interactive)
  (if (not (string= (buffer-name) "*scratch*"))
      (kill-this-buffer)
    (delete-region (point-min) (point-max))
    (switch-to-buffer (other-buffer))
    (bury-buffer "*scratch*")))

(defun duplicate-thing (comment)
  "Duplicates the current line, or the region if active. If an argument is
given, the duplicated region will be commented out."
  (interactive "P")
  (save-excursion
    (let ((start (if (region-active-p) (region-beginning) (line-beginning-position)))
          (end   (if (region-active-p) (region-end) (line-end-position)))
          (fill-column most-positive-fixnum))
      (goto-char end)
      (unless (region-active-p)
        (newline))
      (insert (buffer-substring start end))
      (when comment (comment-region start end)))))

(defun tidy ()
  "Ident, untabify and unwhitespacify current buffer, or region if active."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (indent-region beg end)
    (whitespace-cleanup)
    (untabify beg (if (< end (point-max)) end (point-max)))))

(defun org-sync-pdf ()
  (interactive)
  (let ((headline (nth 4 (org-heading-components)))
        (pdf (concat (file-name-base (buffer-name)) ".pdf")))
    (when (file-exists-p pdf)
      (find-file-other-window pdf)
      (pdf-links-action-perform
       (cl-find headline (pdf-info-outline pdf)
                :key (lambda (alist) (cdr (assoc 'title alist)))
                :test 'string-equal)))))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))

(defadvice eval-last-sexp (around replace-sexp (arg) activate)
  "Replace sexp when called with a prefix argument."
  (if arg
      (let ((pos (point)))
        ad-do-it
        (goto-char pos)
        (backward-kill-sexp)
        (forward-sexp))
    ad-do-it))

(defadvice load-theme
    (before disable-before-load (theme &optional no-confirm no-enable) activate)
  (mapc 'disable-theme custom-enabled-themes))

(let* ((default (face-attribute 'default :height))
       (size default))

  (defun global-scale-default ()
    (interactive)
    (global-scale-internal (setq size default)))

  (defun global-scale-up ()
    (interactive)
    (global-scale-internal (setq size (+ size 20))))

  (defun global-scale-down ()
    (interactive)
    (global-scale-internal (setq size (- size 20))))

  (defun global-scale-internal (arg)
    (set-face-attribute 'default (selected-frame) :height arg)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "C-=") 'global-scale-up)
       (define-key map (kbd "C-+") 'global-scale-up)
       (define-key map (kbd "C--") 'global-scale-down)
       (define-key map (kbd "C-0") 'global-scale-default) map))))

(use-package eglot
  :defer t
  :hook (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
  :config
  (setq eglot-events-buffer-size 0)
  (add-to-list 'eglot-server-programs
               '(web-mode . ("svelteserver" "--stdio"))))

(use-package comint
  :ensure nil
  :bind (:map comint-mode-map ("C-l" . comint-clear-buffer))
  :hook (comint-mode . (lambda () (auto-fill-mode -1)))
  :config (add-hook 'compilation-filter-hook 'comint-truncate-buffer))

;; A terminal via libvterm
(use-package vterm
  :defer t
  :preface
  (let ((last-vterm ""))
    (defun toggle-vterm ()
      (interactive)
      (cond ((string-match-p "^\\vterm<[1-9][0-9]*>$" (buffer-name))
             (goto-non-vterm-buffer))
            ((get-buffer last-vterm) (switch-to-buffer last-vterm))
            (t (vterm (setq last-vterm "vterm<1>")))))

    (defun goto-non-vterm-buffer ()
      (let* ((r "^\\vterm<[1-9][0-9]*>$")
             (vterm-buffer-p (lambda (b) (string-match-p r (buffer-name b))))
             (non-vterms (cl-remove-if vterm-buffer-p (buffer-list))))
        (when non-vterms
          (switch-to-buffer (car non-vterms)))))

    (defun switch-vterm (n)
      (let ((buffer-name (format "vterm<%d>" n)))
        (setq last-vterm buffer-name)
        (cond ((get-buffer buffer-name)
               (switch-to-buffer buffer-name))
              (t (vterm buffer-name)
                 (rename-buffer buffer-name))))))

  :bind (:map custom-bindings-map
              ("C-z" . toggle-vterm)
              ("M-1" . (lambda () (interactive) (switch-vterm 1)))
              ("M-2" . (lambda () (interactive) (switch-vterm 2)))
              ("M-3" . (lambda () (interactive) (switch-vterm 3)))
              ("M-4" . (lambda () (interactive) (switch-vterm 4)))
              ("M-5" . (lambda () (interactive) (switch-vterm 5)))
              ("M-6" . (lambda () (interactive) (switch-vterm 6)))
              ("M-7" . (lambda () (interactive) (switch-vterm 7)))
              ("M-8" . (lambda () (interactive) (switch-vterm 8)))
              ("M-9" . (lambda () (interactive) (switch-vterm 9))))

  :config
  ;; Don't query about killing vterm buffers, just kill it
  (defadvice vterm (after kill-with-no-query nil activate)
    (set-process-query-on-exit-flag (get-buffer-process ad-return-value) nil)))

;; minor mode for editing parentheses
(use-package paredit
  :defer t
  :bind (:map paredit-mode-map ("RET" . nil))
  :hook ((cider-repl-mode
          clojure-mode
          ielm-mode
          racket-mode
          racket-repl-mode
          slime-repl-mode
          lisp-mode
          emacs-lisp-mode
          lisp-interaction-mode
          scheme-mode)
         . paredit-mode))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; Clojure Interactive Development Environment
(use-package cider
  :defer t
  :bind (:map cider-repl-mode-map ("C-l" . cider-repl-clear-buffer)))

;; Commands for refactoring Clojure code
(use-package clj-refactor
  :defer t)

;; Major mode for Racket language
(use-package racket-mode
  :defer t)

;; Superior Lisp Interaction Mode for Emacs
(use-package slime
  :disabled
  :defer t
  :bind (:map slime-repl-mode-map ("C-l" . slime-repl-clear-buffer))
  :hook (common-lisp-mode . activate-slime-helper)
  :config
  (when (file-exists-p "~/.quicklisp/slime-helper.el")
    (load (expand-file-name "~/.quicklisp/slime-helper.el")))

  (setq inferior-lisp-program "sbcl")

  (setq lisp-loop-forms-indentation   6
        lisp-simple-loop-indentation  2
        lisp-loop-keyword-indentation 6))

(setq python-shell-interpreter "python3.10")
(add-hook 'python-mode-hook
          (lambda () (setq forward-sexp-function nil)))

(defun c-setup ()
  (local-set-key (kbd "C-c C-c") 'compile))

(add-hook 'c-mode-hook 'c-setup)

(define-abbrev-table 'java-mode-abbrev-table
  '(("psv" "public static void main(String[] args) {" nil 0)
    ("sopl" "System.out.println" nil 0)
    ("sop" "System.out.printf" nil 0)))

(add-hook 'java-mode-hook 'eglot-ensure)

(defun asm-setup ()
  (setq comment-start "#")
  (local-set-key (kbd "C-c C-c") 'compile))

(add-hook 'asm-mode-hook 'asm-setup)

;; Integrated environment for *TeX*
(use-package tex
  :ensure auctex)

;; Erlang major mode
(use-package erlang
  :defer t)

;; Major mode for editing .nix files
(use-package nix-mode
  :defer t
  :hook (nix-mode . eglot-ensure))

;; A Haskell editing mode
(use-package haskell-mode
  :defer t
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . turn-on-haskell-doc-mode)
         (haskell-mode . turn-on-haskell-indent)))

;; Emacs mode for the programming language Maude
(use-package maude-mode
  :defer t
  :hook (maude-mode . (lambda () (setq-local comment-start "---")))
  :config
  (add-to-list 'maude-command-options "-no-wrap"))

(defun minizinc-setup-compile-command ()
  (let ((command (concat "minizinc " (buffer-file-name) " "))
        (f (concat (file-name-base (buffer-file-name)) ".dzn")))
    (local-set-key (kbd "C-c C-c") 'recompile)
    (setq-local compile-command (concat command (if (file-exists-p f) f "")))))

;; Major mode for MiniZinc code
(use-package minizinc-mode
  :disabled
  :defer t
  :mode "\\.mzn\\'"
  :hook (minizinc-mode . minizinc-setup-compile-command))

;; A generic Emacs interface for proof assistants
(use-package proof-general
  :disabled
  :defer t)

;; A collection of extensions PG's Coq mode
(use-package company-coq
  :disabled
  :defer t
  :hook (coq-mode . company-coq-mode))

;; Rust development environment
(use-package rustic
  :defer t
  :config
  (setq rustic-lsp-client 'eglot))

;; Major mode for the Go programming language
(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure))

;; a major-mode for editing Lua scripts
(use-package lua-mode
  :defer t)

;; Major mode for editing JavaScript
(use-package js
  :ensure nil
  :defer t
  :mode "\\.jsx?\\'"
  :hook (js-ts-mode . eglot-ensure))

;; tree sitter support for TypeScript
(use-package typescript-ts-mode
  :ensure nil
  :defer t
  :mode "\\.tsx?\\'"
  :hook (tsx-ts-mode . eglot-ensure))

(use-package web-mode
  :defer t
  :mode "\\.svelte\\'"
  :hook (web-mode . eglot-ensure)
  :config
  (add-to-list 'web-mode-engines-alist '("svelte" . "\\.svelte\\'")))

;; z3/SMTLIBv2 interactive development
(use-package z3-mode
  :disabled
  :defer t)

;; Display available keybindings in popup
(use-package which-key
  :config
  (which-key-mode 1))

(use-package emacs
  :bind (:map custom-bindings-map
              ("M-u" . upcase-dwim)
              ("M-c" . capitalize-dwim)
              ("M-l" . downcase-dwim)
              ("M-]" . other-frame)
              ("C-j" . newline-and-indent)
              ("C-c s" . ispell-word)
              ("C-c v" . visible-mode)))

(use-package emacs
  :bind (("M-p" . jump-to-previous-like-this)
         ("M-n" . jump-to-next-like-this)
         :map custom-bindings-map
         ("M-,"     . jump-to-previous-like-this)
         ("M-."     . jump-to-next-like-this)
         ("C-x k"   . kill-this-buffer-unless-scratch)
         ("C-c C-0" . global-scale-default)
         ("C-c C-=" . global-scale-up)
         ("C-c C-+" . global-scale-up)
         ("C-c C--" . global-scale-down)
         ("C-c j"   . cycle-spacing-delete-newlines)
         ("C-c d"   . duplicate-thing)
         ("<C-tab>" . tidy))
  :config
  (define-key custom-bindings-map (kbd "C-c .") (cycle-themes)))

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  :init-value t
  :keymap custom-bindings-map)
