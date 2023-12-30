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
;; URL: https://gitlab.com/unrealemacs/emacsconfig

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; This is for testing.  Do not  uncomment.  It allows loading of this
;; file as alternative `init.el' when  you have an existing init file.
;; To do so: $ emacs -q --load init.el
;;(setq user-init-file (or load-file-name (buffer-file-name)))
;;(setq user-emacs-directory (file-name-directory user-init-file))

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

(defconst om-frame-font
  (pcase system-type
    ('darwin     "-*-Menlo-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
    ('gnu/linux  "-ADBO-Hasklug Nerd Font Mono-regular-normal-normal-*-13-*-*-*-m-0-fontset-auto1")
    ('windows-nt "-outline-Consolas-normal-normal-normal-mono-32-*-*-*-c-*-iso8859-1"))
  "Frame font.")

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

(require 'ido)
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

(setq gc-cons-threshold                  (* 3 1024 1024 1024)
      large-file-warning-threshold       (* 100 1024 1024)
      inhibit-startup-message            t
      load-prefer-newer                  t
      show-trailing-whitespace           t
      ring-bell-function                 'ignore
      auto-save-default                  100
      auto-save-list-file-prefix         "autosave"
      create-lockfiles                   t
      save-place-forget-unreadable-files nil)
(setq read-process-output-max (* 20 1024 1024))
(setq ido-use-filename-at-point              nil
      ido-use-virtual-buffers                t
      ido-enable-flex-matching               t
      ido-auto-merge-work-directories-length -1)
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
;; Interactively DO things
(ido-mode       -1)
(ido-everywhere -1)
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


(when (display-graphic-p)
  (when om-frame-font
    (set-frame-font om-frame-font)))

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

;;; Convenient packages

;; https://www.emacswiki.org/emacs/Smex
(use-package smex
  :config  (smex-initialize))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :config (which-key-mode +1))

;; https://github.com/DarwinAwardWinner/ido-completing-read-plus
(use-package ido-completing-read+
  :config (ido-ubiquitous-mode -1))

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

;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(use-package modern-cpp-font-lock
  :config (modern-c++-font-lock-global-mode +1))

;; https://clang.llvm.org/docs/ClangFormat.html
(use-package clang-format
  :config (setq clang-format-executable om-clang-format-location))

;; https://github.com/Lindydancer/highlight-doxygen
(use-package highlight-doxygen)

;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :init   (global-flycheck-mode +1))

;; https://emacs-lsp.github.io/lsp-mode/

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook     ((c-mode   . lsp-deferred)
	     (c++-mode . lsp-deferred)
	     (haskell-mode . lsp-mode)
	     (haskell-literate-mode . lsp-mode)
             (gluon-mode . lsp-mode)
	     (lsp-mode . lsp-enable-which-key-integration)
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
  (setq lsp-keymap-prefix       om-kbd-keymap-prefix-lsp
		  lsp-clients-clangd-args '("--header-insertion=never"
					    "--completion-style=bundled"
					    "--background-index")
		  
		  )
            
  :commands (lsp lsp-deferred))


;; https://github.com/emacs-lsp/lsp-ui


(use-package lsp-ui
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

;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
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
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))
(add-hook
 'c++-mode-hook
 (lambda ()
   (setq c-default-style "bsd"
	 c-basic-offset  4
	 tab-width       4)
   (c-set-offset 'innamespace 0)
   (highlight-doxygen-mode +1)
   ;; https://www.emacswiki.org/emacs/ElectricPair
;;   (electric-pair-mode +1)
   ;; https://wikemacs.org/wiki/Subword-mode
   (subword-mode +1)
   (display-line-numbers-mode +1)
   ;; Set the right margin according to Epic Games conding standard
   (setq-local fill-column 120)
   (local-set-key om-kbd-clang-format-buffer #'clang-format-buffer)
   (local-set-key om-kbd-yasnippet-complete  #'company-yasnippet)))

;;; ue.el
;; https://gitlab.com/unrealemacs/ue.el
(use-package ue
  :init   (ue-global-mode +1)
  :config (define-key
	    ue-mode-map
	    om-kbd-keymap-prefix-ue
	    'ue-command-map))

;;; Misc

;; Run Emacs server if not already  running.  This is to make Emacs to
;; open new files in the same frame (graphical window).
(require 'server)
(when (not (server-running-p))
  (server-start))


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)
;;; init.el ends here
