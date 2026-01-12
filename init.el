;;; init.el  --- Unreal Emacs entry point.    -*- lexical-binding: t; -*-

;; Copyright 2021  Oleksandr Manenko

;; Permission  is  hereby  granted,  free of  charge,  to  any  person
;; obtaining  a copy  of  this software  and associated  documentation
;; files   (the  "Software"),   to  deal   in  the   Software  without
;; restriction, including without limitation  the rights to use, copy,
;; modify, merge, publish, distribute,  sublicense, and/or sell copies
;; of the  Software, and  to permit  persons to  whom the  Software is
;; furnished to do so, subject to the following conditions:

;; The  above copyright  notice and  this permission  notice shall  be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE  IS PROVIDED  "AS IS", WITHOUT  WARRANTY OF  ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY,   FITNESS    FOR   A   PARTICULAR    PURPOSE   AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT,  TORT OR OTHERWISE, ARISING FROM, OUT  OF OR IN
;; CONNECTION WITH  THE SOFTWARE OR THE  USE OR OTHER DEALINGS  IN THE
;; SOFTWARE.

;; Author: Oleksandr Manenko
;; Version: 1.3
;; URL: https://gitlab.com/unrealemacs/emacsconfig

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; This is for testing.  Do not  uncomment.  It allows loading of this
;; file as alternative `init.el' when  you have an existing init file.
;; To do so: $ emacs -q --load init.el
;;(setq user-init-file (or load-file-name (buffer-file-name)))
;;(setq user-emacs-directory (file-name-directory user-init-file))
; (package-initialize)

(defconst om-min-emacs-version "26.1")
(when (version< emacs-version om-min-emacs-version)
  (error "This Emacs version is unsupported, please upgrade to at least Emacs %s"
	 om-min-emacs-version))

(defun om-config-executable-find (command)
  "Search for COMMAND and return the absolute file name.

Return  COMMAND if  COMMAND is  not found  anywhere in  the value
returned by the function `exec-path'."
  (or (executable-find command)
      command))

;;; Configuration variables
(setq-default indent-tabs-mode nil)
(defconst om-clang-location
  (pcase system-type
    ('darwin     (om-config-executable-find "clang"))
    ('gnu/linux  (om-config-executable-find "clang"))
    ('windows-nt "C:\\Program Files\\LLVM\\bin\\clang++.exe"))
  "The absolute path to the clang executable.")

(defconst om-clang-format-location
    (pcase system-type
      ('darwin     (om-config-executable-find "clang-format"))
      ('gnu/linux  (om-config-executable-find "clang-format"))
      ('windows-nt "C:\\Program Files\\LLVM\\bin\\clang-format.exe"))
    "The absolute path to the clang-format executable.")

(defconst om-ag-location
    (pcase system-type
      ('darwin     (om-config-executable-find "ag"))
      ('gnu/linux  (om-config-executable-find "ag"))
      ('windows-nt "C:\\bin\\ag.exe"))
  "The absolute path to the ag executable.

See https://github.com/ggreer/the_silver_searcher#installing on how
to install this tool on your system.")

(defconst om-rg-location
    (pcase system-type
      ('darwin     (om-config-executable-find "rg"))
      ('gnu/linux  (om-config-executable-find "rg"))
      ('windows-nt "C:\\bin\\rg.exe"))
  "The absolute path to the rg executable.

See https://github.com/BurntSushi/ripgrep#installation on how
to install this tool on your system.")

(defconst om-activate-c++-mode-for-h-files t
  "Whether Emacs should treat `*.h' files as C++ headers.
Emacs  associates  `*.h' files  with  C  headers by  default  and
activates `c-mode' for them.  This  is an issue for Unreal Engine
based  projects  because Epic  Games  uses  `*.h' for  C++  code.
Setting this variable to a logical true changes the default Emacs
behavior.

If you work with C projects in Emacs, set this setting to nil and
use one of the alternative solutions instead:

  - Create `.dir-locals.el'  file in  the project  root directory
    with the following contents:
        ((c-mode . ((mode . c++))))
  - Put the following line at the top of the each `*.h' file:
        // -*-c++-*-.")

(defconst om-distraction-free-ui t
  "Whether Emacs should hide toolbar, menubar, and friends.")

;(defconst om-frame-font
;  (pcase system-type
;    ('darwin     "-*-Menlo-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;    ('gnu/linux "-ADBO-Hasklug Nerd Font Mono-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1") ;"-ADBO-Hasklug Nerd Font Mono-regular-normal-normal-*-13-*-*-*-m-0-fontset-auto1")
;    ('windows-nt "-outline-Consolas-normal-normal-normal-mono-32-*-*-*-c-*-iso8859-1"))
;  "Frame font.")
;(add-to-list 'default-frame-alist
;             '(font . "Hasklug Nerd Font 12"))
(add-to-list 'default-frame-alist
             '(font . "CaskaydiaCove Nerd Font 12"))
;;;; Keys
(defconst om-kbd-clang-format-buffer      (kbd "C-c f"))
(defconst om-kbd-ibuffer                  (kbd "C-x C-b"))
(defconst om-kbd-keymap-prefix-lsp        (kbd "C-c l"))
(defconst om-kbd-keymap-prefix-projectile (kbd "C-c p"))
(defconst om-kbd-keymap-prefix-ue         (kbd "C-c u"))
(defconst om-kbd-magit-status             (kbd "C-x g"))
(defconst om-kbd-search-backward          (kbd "C-M-r"))
(defconst om-kbd-search-backward-regexp   (kbd "C-r"))
(defconst om-kbd-search-forward           (kbd "C-M-s"))
(defconst om-kbd-search-forward-regexp    (kbd "C-s"))
(defconst om-kbd-smex                     (kbd "M-x"))
(defconst om-kbd-yasnippet-complete       (kbd "C-c y"))

;;; Package management

;(require 'ido)
(require 'package)
(require 'recentf)
(require 'saveplace)
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))
;;;; Configure straight.el as a package manager
;; https://github.com/raxod502/straight.el
(defvar straight-use-package-by-default t)
(defvar bootstrap-version               nil)
(let ((bootstrap-file    (expand-file-name
			  "straight/repos/straight.el/bootstrap.el"
			  user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install https://github.com/jwiegley/use-package
(straight-use-package 'use-package)


;; Keep ~/.emacs.d clean
;; https://github.com/emacscollective/no-littering

(setq gc-cons-threshold                  (* 12 1024 1024 1024)
      large-file-warning-threshold       (* 100 1024 1024)
      max-lisp-eval-depth                100000
      inhibit-startup-message            t
      load-prefer-newer                  t
      show-trailing-whitespace           t
      ring-bell-function                 'ignore
      auto-save-default                  100
      auto-save-list-file-prefix         "autosave"
      create-lockfiles                   t
      save-place-forget-unreadable-files nil)
(setq read-process-output-max (* 100 1024 1024))

   (setq no-littering-etc-directory
         (expand-file-name "etc/" user-emacs-directory))
   (setq no-littering-var-directory
         (expand-file-name "var/" user-emacs-directory))
(use-package no-littering
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq recentf-max-menu-items 5))

;; Enable blinking cursor.
(blink-cursor-mode +1)
;; Show column number in the mode line.
(column-number-mode +1)
;; Do  not  highlight   current  line.  Set  this  to   +1  to  enable
;; highlighting.
(global-hl-line-mode -1)
;; Highlight matching parenthesis.
(show-paren-mode +1)
;; Display right margin indicator.
(global-display-fill-column-indicator-mode +1)
;; When you visit  a file, point goes  to the last place  where it was
;; when you previously visited the same file.
(save-place-mode +1)

;; Recent Files
(recentf-mode   +1)
;; Change all yes/no  questions to y/n type so that  you don't need to
;; type "yes".
(fset 'yes-or-no-p 'y-or-n-p)

;;; Global keybindings

(global-set-key om-kbd-search-forward-regexp  #'isearch-forward-regexp)
(global-set-key om-kbd-search-backward-regexp #'isearch-backward-regexp)
(global-set-key om-kbd-search-forward         #'isearch-forward)
(global-set-key om-kbd-search-backward        #'isearch-backward)
(global-set-key om-kbd-ibuffer                #'ibuffer)
(global-set-key om-kbd-smex                   #'smex)
(global-set-key om-kbd-magit-status           #'magit-status)
(global-set-key (kbd "RET")                   #'newline-and-indent)

;; Move point from window to window using Shift and the arrow keys.
;; https://www.emacswiki.org/emacs/WindMove
(windmove-default-keybindings)

;; On a text terminal, the `C-z' command suspends Emacs. Turn this off.
;; No, fuck you, this is the easiest way to kill an unresponsive emacs.
;; (global-unset-key (kbd "C-z"))

;;; UI

(when om-distraction-free-ui
  (menu-bar-mode   -1)
  (tool-bar-mode   -1)
  (scroll-bar-mode -1))

;;; GUI



;; Configure system clipboard interop.
  (setq select-enable-clipboard             t
	select-enable-primary               t
	save-interprogram-paste-before-kill t
	mouse-yank-at-point                 t)

;;; GNU/Linux

(when (eq system-type 'gnu/linux)
  ;; Add GNU/Linux specific stuff here
  )

;;; macOS

(when (eq system-type 'darwin)
  ;; Ensure environment variables inside Emacs look the same as in the
  ;; user's shell.
  (use-package exec-path-from-shell
    :config
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

;;; Windows

(when (eq system-type 'windows-nt)
  ;; Add Windows specific stuff here
  )


(use-package inheritenv
  :demand t
  )

(use-package envrc
  :init (envrc-global-mode)
  :requires (inheritenv)
  :after (inheritenv)
  :hook (after-init . envrc-global-mode)
  :demand t
  )
(with-eval-after-load 'envrc
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))

;;; Convenient packages

;; https://company-mode.github.io/
(use-package company
  :config (setq company-clang-executable      om-clang-location
		company-minimum-prefix-length 3)
  :hook   (prog-mode . company-mode))

;; https://joaotavora.github.io/yasnippet/
(use-package yasnippet
  :config
  (setq yas-indent-line 'fixed)
  (yas-reload-all)
  :hook   (prog-mode . yas-minor-mode))

;; https://magit.vc/
(use-package magit)

; https://github.com/ludwigpacifici/modern-cpp-font-lock
(use-package modern-cpp-font-lock
  :config (modern-c++-font-lock-global-mode +1))

;; https://clang.llvm.org/docs/ClangFormat.html
(use-package clang-format
  :config (setq clang-format-executable om-clang-format-location))

;; https://github.com/Lindydancer/highlight-doxygen
(use-package highlight-doxygen)

;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :requires (envrc)
  :after (envrc)
  :init   (global-flycheck-mode +1)
;(setq flycheck-executable-find
;        (lambda (cmd) (inheritenv (executable-find cmd))))
)
;; https://emacs-lsp.github.io/lsp-mode/

(use-package lsp-mode
  :straight (:repo "emacs-lsp/lsp-mode"
                   :host github
                   :type git)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook     ((c-mode . lsp-deferred)
             (c-ts-mode . lsp-deferred)
             (c++-ts-mode . lsp-deferred)
             (c-or-c++-ts-mode . lsp-deferred)
             (c-or-c++-mode . lsp-deferred)
	     (c++-mode . lsp-deferred)
	     (haskell-mode . lsp-deferred)
             (lua-mode . lsp-deferred)
             (js-mode . lsp-deferred)
             (js-ts-mode . lsp-deferred)
             (typescript-mode . lsp-deferred)
             (typescript-ts-mode . lsp-deferred)
	     (haskell-literate-mode . lsp-deferred)
             (gluon-mode . lsp-mode)
             (racket-mode . lsp-deferred)
             (ansible-mode . lsp-deferred)
;             (rustic-mode . lsp-deferred)
;             (tcl-mode . lsp-deferred)
;	     (lsp-mode . lsp-enable-which-key-integration)
             (slint-mode . lsp-deferred)
             (lsp-mode . lsp-ui-mode))
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-eldoc-render-all t)
  (setq lsp-idle-delay 0.3)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (setq lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-display-reborrow-hints t)
  (setq lsp-keymap-prefix om-kbd-keymap-prefix-lsp)            
  :commands (lsp lsp-deferred))


;; https://github.com/emacs-lsp/lsp-ui

(use-package lsp-ui
  :straight (:repo "emacs-lsp/lsp-ui"
                   :host github
                   :type git)
  :config
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-update-mode "line")
  (setq lsp-ui-doc-enable t)
  (setq lsp-lens-enable t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-completion-show-detail t)
  (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  
  :commands lsp-ui-mode)

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x a 1"   . treemacs-delete-other-windows)
        ("C-x a t"   . treemacs)
        ("C-x a d"   . treemacs-select-directory)
        ("C-x a B"   . treemacs-bookmark)
        ("C-x a C-t" . treemacs-find-file)
        ("C-x a M-t" . treemacs-find-tag)))

;;(use-package treemacs-evil
;;  :after (treemacs evil)
;;  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(treemacs-start-on-boot)

;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
    :straight (:repo "emacs-lsp/lsp-treemacs"
                   :host github
                   :type git)
  :commands (lsp-treemacs-errors-list
	     lsp-treemacs-symbols)
  :config   (lsp-treemacs-sync-mode +1))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :init   (projectile-mode +1)
  :config (define-key
	    projectile-mode-map
	    om-kbd-keymap-prefix-projectile
	    'projectile-command-map))

;; https://github.com/Wilfred/ag.el
(use-package ag
  :config (setq ag-executable om-ag-location))

;; https://github.com/dajva/rg.el
(use-package rg
  :config (setq rg-executable om-rg-location))

;;; C++ config

(when om-activate-c++-mode-for-h-files
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode)))
(add-hook
 'c++-mode-hook
 (lambda ()
   (setq c-default-style "bsd"
	 c-basic-offset  2
	 tab-width       2)
   (c-set-offset 'innamespace 0)
   (highlight-doxygen-mode +1)
   ;; https://www.emacswiki.org/emacs/ElectricPair
;;   (electric-pair-mode +1)
   ;; https://wikemacs.org/wiki/Subword-mode
   (subword-mode +1)
   (display-line-numbers-mode +1)
   ;; Set the right margin according to my default clang-format
   (setq-local fill-column 80)
   (local-set-key om-kbd-clang-format-buffer #'clang-format-buffer)
   (local-set-key om-kbd-yasnippet-complete  #'company-yasnippet)))

(add-hook
 'c++-ts-mode-hook
 (lambda ()
   (setq c-default-style "bsd"
	 c-basic-offset  2
	 tab-width       2)
   (c-set-offset 'innamespace 0)
   (highlight-doxygen-mode +1)
   ;; https://www.emacswiki.org/emacs/ElectricPair
;;   (electric-pair-mode +1)
   ;; https://wikemacs.org/wiki/Subword-mode
   (subword-mode +1)
   (display-line-numbers-mode +1)
   ;; Set the right margin according to my default clang-format
   (setq-local fill-column 80)
   (local-set-key om-kbd-clang-format-buffer #'clang-format-buffer)
   (local-set-key om-kbd-yasnippet-complete  #'company-yasnippet)))


;;; Misc

;; Run Emacs server if not already  running.  This is to make Emacs to
;; open new files in the same frame (graphical window).
(require 'server)
(when (not (server-running-p))
  (server-start))

;; color theme
(use-package color-theme-sanityinc-tomorrow)
(use-package gruvbox-theme)

;; xclip
;; (use-package xclip)
;; (xclip-mode +1)
;; credit: yorickvP on Github
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe
                                      :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

(use-package rust-mode
  :ensure t
  :init
  (setq treesit-extra-load-path '("~/.emacs.d/tree-sitter-module/dist/"))
  (setq rust-mode-treesitter-derive t))

;; hello darkness my old friend
(use-package rustic
  :requires (envrc)
  :after (envrc rust-mode)
      :straight (:repo "emacs-rustic/rustic"
                   :host github
                   :type git)
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
;;  :hook ((rustic-mode . electric-pair-mode))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;misc
(setq auto-mode-alist (cons '("\\.ipp$" . c++-ts-mode) auto-mode-alist))

(setq compilation-scroll-output t)


(use-package lua-mode)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(tcl-mode . "tcl"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "/home/wrath/lc/tcl/lsp/lsp.tcl")
                    :activation-fn (lsp-activate-on "tcl")
                    :server-id 'lsptcl)))

(use-package popup)

(winner-mode 1)

;(define-key global-map (kbd "C-x C-c") 'save-buffers-kill-emacs)

;; github etc
(use-package forge)
(use-package emacsql)
;(use-package code-review)
;(add-hook 'code-review-mode-hook #'emojify-mode)
;(setq code-review-fill-column 120)
;(setq code-review-auth-login-marker 'forge)


(use-package w3m
  :config
  (define-key w3m-mode-map [left] 'backward-char)
  (define-key w3m-mode-map [right] 'forward-char)
  (define-key w3m-mode-map [up] 'previous-line)
  (define-key w3m-mode-map [down] 'next-line)
  )


;; cmake
(use-package cmake-mode)
(use-package cmake-font-lock)

;; for assigning blame
(use-package git-timemachine)

;; lol bolt
(use-package rmsbolt)
(add-to-list 'load-path "~/.emacs.d/")

; slime + clasp
(use-package slime)
(setq inferior-lisp-program "clasp")

;; org-mode stuff
(use-package org-edna
  :init (org-edna-mode +1)
  )
(use-package org-journal)
(use-package org-contrib)
(use-package org-ql)
(use-package org-transclusion
  :after org
  :bind
  (("<f12>" . org-transclusion-add)
   ("C-c r T" . org-transclusion-mode)
   )
  )

(use-package helm-org-ql)
;(use-package hyperbole
;    :straight (:repo "rswgnu/hyperbole"
;                   :host github
;                   :type git)
;  )
;(hyperbole-mode +1)
;(define-key hyperbole-mode-map (kbd "<mouse-3>")  'action-key)
(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/org-roam"))
  (setq find-file-visit-truename t)
  (org-roam-db-autosync-mode)
  (require 'org-roam-export)
  (require 'org-protocol)
  (require 'org-roam-protocol)
  
  :bind
  (("C-c r f" . org-roam-node-find)
   ("C-c r i" . org-roam-node-insert)
   ("C-c r r" . org-roam-node-random)
   ("C-c r l" . org-roam-buffer-toggle)
   ("C-c r g" . org-roam-graph)
   ("C-c r c" . org-roam-capture)
   
   :map org-mode-map
         (("C-M-i" . completion-at-point)
          ("C-c r i" . org-roam-node-insert)
          ("C-c r o" . org-id-get-create)
          ("C-c r t" . org-roam-tag-add)
          ("C-c r a" . org-roam-alias-add)
          ("[mouse-1]" . org-roam-visit-thing)
          ("C-c r l" . org-roam-buffer-toggle)))
  )
(use-package org-node
  :after org
  :config
  (org-node-cache-mode)
  (setq org-node-backlink-aggressive t)
  (org-node-backlink-mode)
  :bind
  (("M-s M-f" . org-node-find)
   :map org-mode-map (("M-s M-i" . org-node-insert-link))
   )
  )
(use-package org-node-fakeroam
  :after org-node
  :init
  (setq org-node-extra-id-dirs '("~/org-roam/"))
  (setq org-node-creation-fn #'org-node-fakeroam-new-via-roam-capture)
  (setq org-node-slug-fn #'org-node-fakeroam-slugify-via-roam)
  (setq org-node-datestamp-format "%Y%m%d%H%M%S-")
  (org-node-fakeroam-fast-render-mode)
  (setq org-node-fakeroam-fast-render-persist t)

  )
(use-package org-roam-bibtex)
(use-package org-roam-ql)
;(use-package org-roam-ui)
(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))
;; emms
(use-package emms)
;; w3m
(use-package w3m)
;; I like helm
(use-package epg)
(use-package epa)
(require 'helm-init)
;(require 'emms-config.el)

;;translate
;(use-package go-translate)
;(setq gt-translate-list '(("ja" "en") ("fr" "en") ("en" "de") ("de" "en") ("pl" "en")))

;(setq gt-default-translator
;      (gt-translator
;       :taker   (gt-taker :text 'buffer :pick 'paragraph)
;       :engines (list (gt-bing-engine) (gt-google-engine))
;       :render (gt-buffer-render)))


;; dashboard
(use-package page-break-lines)
(use-package all-the-icons)
(use-package nerd-icons)
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  )
(dashboard-setup-startup-hook)

;; docsets
(use-package helm-dash)
(use-package devdocs
  :bind
  ("C-c C-d" . devdocs-lookup)
  )

;; haskell
;(use-package haskell-literate-mode)
(use-package haskell-mode
  :hook ((haskell-mode . turn-on-haskell-indentation))
  )

(use-package lsp-haskell)
;; more lsp stuff
(use-package helm-lsp
  :config
  (keymap-set lsp-command-map "<remap> <lsp-execute-code-action>" 'helm-lsp-code-actions)
  :commands (helm-lsp-workspace-symbol helm-lsp-code-actions)
  )
(keymap-set lsp-command-map "<remap> <lsp-execute-code-action>" 'helm-lsp-code-actions)

(use-package dape
  :straight (:repo "/svaante/dape"
                   :host github
                   :type git)
  ;; To use window configuration like gud (gdb-mi)
   :init
   (setq dape-buffer-window-arrangment 'gud)
  :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangment 'right)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
   (add-hook 'dape-on-stopped-hooks 'dape-info)
   (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
   (setq dape-key-prefix "\C-x\C-a")

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks
  ;;           (defun dape--save-on-start ()
  ;;             (save-some-buffers t t)))

  ;; Projectile users
   (setq dape-cwd-fn 'projectile-project-root)
  )

(require 'gluon-mode)
(setq auto-mode-alist (cons '("\\.glu$" . gluon-mode) auto-mode-alist))
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
    '(gluon-mode . "gluon")))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "gluon_language-server")
                  :activation-fn (lsp-activate-on "gluon")
                  :server-id 'gluon))


(use-package minions
  :config (minions-mode 1))
(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))
(add-to-list 'auto-mode-alist '("\\.*rc$" . shell-script-mode))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (define-key yaml-mode-map "\C-m" 'newline-and-indent)
  )

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)
(add-hook 'nix-mode-hook
          (lambda () (add-hook 'before-save-hook 'nix-format-before-save nil 'local)))
(add-hook 'c++-mode-hook
                    (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil 'local)))
(add-hook 'c++-ts-mode-hook
                    (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil 'local)))
;(setq lsp-nix-nil-server-path "/home/wrath/.cargo/bin/nil")
(with-eval-after-load 'lsp-mode
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                     :major-modes '(nix-mode)
                     :priority -10
                     :server-id 'nixd)))

(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode")
    :hook (gdscript-mode . lsp-deferred)
    )
;(setq gdscript-use-tab-indents -1) ;; If true, use tabs for indents. Default: t
;(setq gdscript-indent-offset 2) ;; Controls the width of tab-based indents ; both options incompatible with stupid gdformat
(setq gdscript-gdformat-save-and-format -1) ;; Save all buffers and format them with gdformat anytime Godot executable is run.
;(setq gdscript-godot-executable "~/godot-mine/result/bin/godot4")
;(add-hook 'gdscript-mode-hook
;          (lambda () (add-hook 'before-save-hook 'gdscript-format-buffer nil 'local)))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package racket-mode
  :init
  (require 'racket-xp)
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  )
(use-package slint-mode)

;; Enable use of tree-sitter modes by default when available
;(setq major-mode-remap-defaults t)

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
;(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))                                                                      
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
;(use-package treesit-auto
;  :custom
;  (treesit-auto-install 'prompt)
;  :config
;  (treesit-auto-add-to-auto-mode-alist 'all)
;  (global-treesit-auto-mode))
;; for eat terminal backend:
(use-package eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el"))))

;; for vterm terminal backend:
;(use-package vterm :straight t)

;; install claude-code.el:
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map) ;; or your preferred key
  :config
  (claude-code-mode))
;(setq vterm-max-scrollback 100000)
;(setq claude-code-terminal-backend 'vterm)

;(use-package claude-code-ide
;  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el"))
;(setq claude-code-ide-window-side 'bottom
;      claude-code-ide-window-height 30)
;

(use-package typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))
(setenv "CODESTRAL_API_KEY" (car (read-lines "~/CODESTRAL_API_KEY")))
(use-package minuet
    :straight (:repo "milanglacier/minuet-ai.el"
                   :host github
                   :type git)
    :bind
    (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
     ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
     ("C-c m" . #'minuet-configure-provider)
     :map minuet-active-mode-map
     ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
     ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
     ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
     ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
     ;; Accept the first line of completion, or N lines with a numeric-prefix:
     ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
     ("M-a" . #'minuet-accept-suggestion-line)
     ("M-e" . #'minuet-dismiss-suggestion))

    :init
    ;; if you want to enable auto suggestion.
    ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
;    (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

    :config
    ;; You can use M-x minuet-configure-provider to interactively configure provider and model
;    (setq minuet-provider 'openai-fim-compatible)

    (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))

    ;; For Evil users: When defining `minuet-ative-mode-map` in insert
    ;; or normal states, the following one-liner is required.

    ;; (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)

    ;; This is *not* necessary when defining `minuet-active-mode-map`.

    ;; To minimize frequent overhead, it is recommended to avoid adding
    ;; `evil-normalize-keymaps` to `minuet-active-mode-hook`. Instead,
    ;; bind keybindings directly within `minuet-active-mode-map` using
    ;; standard Emacs key sequences, such as `M-xxx`. This approach should
    ;; not conflict with Evil's keybindings, as Evil primarily avoids
    ;; using `M-xxx` bindings.
(use-package ansible)
;(use-package ansible-vault :ensure t
;    :straight (:repo "freehck/ansible-vault-mode"
;                   :host github
;                   :type git)
;  )
(use-package ansible-doc)
(add-hook 'ansible-hook 'ansible-auto-decrypt-encrypt)
(add-hook 'yaml-mode-hook '(lambda () (ansible-mode 1)))
;(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
