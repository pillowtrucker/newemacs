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

;;

;;; Code:

;; This is  for testing.  Do not uncomment.  It allows loading  of this  file as
;; alternative   `init.el'    when   you    have   an   existing    init   file.
;; To do so: $ emacs -q --load init.el
;(setq user-init-file (or load-file-name (buffer-file-name)))
;(setq user-emacs-directory (file-name-directory user-init-file))

;; Put customisations to a separate file under ~/.emacs.d/custom.el:
(setq custom-file (expand-file-name "custom.el"
				    user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Performance

(setq
 ;; Always load newest byte code.
 load-prefer-newer t
 ;; Reduce the frequency of garbage collection  by making it happen on each 50MB
 ;; of allocated data.
 gc-cons-threshold 50000000
 ;; Warn when opening files bigger than 100MB.
 large-file-warning-threshold 100000000
 ;; Disable startup message.
 inhibit-startup-message t
 ;; Turn off alarm bell.
 ring-bell-function 'ignore
 ;; Do not auto save files.
 auto-save-default nil
 auto-save-list-file-prefix nil
 ;; No need for `~' files when editing.
 create-lockfiles nil
 ;; If Emacs  is slow to exit  after enabling `saveplace', set  this variable to
 ;; nil. See `Built-in global modes' section where it is enabled by default.
 save-place-forget-unreadable-files nil
 ;; Show trailing whitespaces.
 show-trailing-whitespace t
 ;; Emacs can  automatically create backup  files. This  tells Emacs to  put all
 ;; backup files to ~/.emacs.d/.backups.
 ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
 backup-directory-alist   `(("." . ,(expand-file-name ".backups"
						      user-emacs-directory))))

;; https://www.masteringemacs.org/article/introduction-to-ido-mode
(setq
 ;; Enable flexible string matching. Flexible matching means that if the entered
 ;; string does not  match any item, any item containing  the entered characters
 ;; in the given sequence will match.
 ido-enable-flex-matching               t
 ido-use-filename-at-point              nil
 ido-use-virtual-buffers                t
 ido-auto-merge-work-directories-length -1)

;; https://www.emacswiki.org/emacs/RecentFiles
(setq recentf-max-menu-items 10)

;;; Built-in global modes

(require 'ido)
(require 'recentf)

;; Enable blinking cursor.
(blink-cursor-mode +1)
;; Show column number in the mode line.
(column-number-mode +1)
;; Do not highlight current line. Set this to +1 to enable highlighting.
(global-hl-line-mode -1)
;; Highlight matching parenthesis.
(show-paren-mode +1)
;; Display right margin indicator.
(global-display-fill-column-indicator-mode +1)
;; When you  visit a file, point  goes to the last  place where it was  when you
;; previously visited the same file.
(save-place-mode +1)
;; Interactively DO things
(ido-mode       +1)
(ido-everywhere +1)
;; Recent Files
(recentf-mode   +1)

;;; Misc goodies

;; Change all yes/no questions to y/n type so that you don't need to type "yes".
(fset 'yes-or-no-p 'y-or-n-p)

;;; Global keybindings

;; Interactive search key bindings.  By  default, `C-s' runs isearch-forward, so
;; this swaps the bindings.
(global-set-key (kbd "C-s")   'isearch-forward-regexp)
(global-set-key (kbd "C-r")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; On a text terminal, the `C-z' command suspends Emacs. Turn this off.
(global-unset-key (kbd "C-z"))

;; Insert  a newline,  then adjust  indentation of  following line  when hitting
;; `Enter'.
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Move point from window to window using Shift and the arrow keys.
;; https://www.emacswiki.org/emacs/WindMove
(windmove-default-keybindings)

;; https://www.emacswiki.org/emacs/IbufferMode
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Package management

(require 'package)

;; Add MELPA to the list of package archives.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initialise package manager.
(package-initialize)

;; Refresh the list of packages available unless it was done before.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Make sure `use-package' is installed and enable it.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;;; UI

;; Remove menu, toolbar,  and scrollbar so that we have  less things to distruct
;; us from the coding.
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

;;; GUI

(when (display-graphic-p)
  ;; Configure system clipboard interop.
  (setq x-select-enable-clipboard           t
	x-select-enable-primary             t
	save-interprogram-paste-before-kill t
	mouse-yank-at-point                 t))

;;; GNU/Linux

(when (eq system-type 'gnu/linux)
  ;; Add GNU/Linux specific stuff here
  )

;;; macOS

(when (eq system-type 'darwin)
  ;; Ensure environment  variables inside Emacs look  the same as in  the user's
  ;; shell.
  (use-package exec-path-from-shell
    :ensure t
    :config
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

;;; Windows

(when (eq system-type 'windows-nt)
  ;; Add Windows specific stuff here
  (setq
   clang-format-executable  "C:\\Program Files\\LLVM\\bin\\clang-format.exe"
   company-clang-executable "C:\\Program Files\\LLVM\\bin\\clang++.exe"))

;;; Convenient packages

;; https://www.emacswiki.org/emacs/Smex
(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; https://github.com/DarwinAwardWinner/ido-completing-read-plus
(use-package ido-completing-read+
  :ensure t
  :config (ido-ubiquitous-mode +1))

;; https://company-mode.github.io/
(use-package company
  :ensure t
  :config (setq company-tooltip-align-annotations nil
		;; You have to type at least 3 characters to get autocompletion
		company-minimum-prefix-length     3)
  :hook   (prog-mode . company-mode))

;; https://joaotavora.github.io/yasnippet/
(use-package yasnippet
  :ensure t
  :config
  (setq yas-indent-line 'fixed)
  (yas-reload-all)
  :hook   (prog-mode . yas-minor-mode))

;; https://magit.vc/
(use-package magit
  :ensure t
  :config (global-set-key (kbd "C-x g") 'magit-status))

;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(use-package modern-cpp-font-lock
  :ensure t
  :config (modern-c++-font-lock-global-mode t))

;; https://clang.llvm.org/docs/ClangFormat.html
(use-package clang-format
  :ensure t)

;; https://github.com/Lindydancer/highlight-doxygen
(use-package highlight-doxygen
  :ensure t)

;; https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :ensure   t
  :init     (setq lsp-keymap-prefix "C-c l")
  :hook     ((c-mode   . lsp-deferred)
	     (c++-mode . lsp-deferred)
	     (lsp-mode . lsp-enable-which-key-integration))
  :config   (setq lsp-clients-clangd-args '("--header-insertion=never"
					    "--completion-style=bundled"
					    "--background-index"))
  :commands (lsp lsp-deferred))

;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :ensure   t
  :commands lsp-ui-mode)

;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
  :ensure   t
  :commands (lsp-treemacs-errors-list
	     lsp-treemacs-symbols)
  :config   (lsp-treemacs-sync-mode 1))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :bind   (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :init   (projectile-mode +1)
;;  :config (setq projectile-indexing-method 'hybrid)
  )

;;; C++ config
(add-hook
 'c++-mode-hook
 (lambda ()
   (setq c-default-style "bsd"
	 c-basic-offset  4
	 tab-width       4)
   (c-set-offset 'innamespace 0)
   (highlight-doxygen-mode +1)
   ;; https://www.emacswiki.org/emacs/ElectricPair
   (electric-pair-mode +1)
   ;; https://wikemacs.org/wiki/Subword-mode
   (subword-mode +1)
   (display-line-numbers-mode +1)
   ;; Set the right margin according to Epic Games conding standard
   (setq-local fill-column 120)
   (local-set-key (kbd "C-c f") #'clang-format-region)
   (local-set-key (kbd "C-c y") #'company-yasnippet)))

;;; ue.el

;; ue.el is not on MELPA yet so we  install it manually as git submodule for the
;; time being
;; https://gitlab.com/unrealemacs/ue.el

(add-to-list 'load-path (expand-file-name "ue-el" user-emacs-directory))
(require 'ue)
(define-key ue-mode-map (kbd "C-c u") 'ue-command-map)
(ue-global-mode +1)

;;; Misc

;; Run Emacs server  if not already running.  This is to make Emacs  to open new
;; files in the same frame (graphical window).
(require 'server)
(when (not (server-running-p))
  (server-start))

(provide 'init)
;;; init.el ends here
