;;; custom.el --- my dingdongs
;;; Commentary:

;;; Code:


;; color theme
(use-package color-theme-sanityinc-tomorrow)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(forge-alist
   '(("github-pillow" "api.github.com" "github.com" forge-github-repository)
     ("github.com" "api.github.com" "github.com" forge-github-repository)
     ("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)
     ("salsa.debian.org" "salsa.debian.org/api/v4" "salsa.debian.org" forge-gitlab-repository)
     ("framagit.org" "framagit.org/api/v4" "framagit.org" forge-gitlab-repository)
     ("gitlab.gnome.org" "gitlab.gnome.org/api/v4" "gitlab.gnome.org" forge-gitlab-repository)
     ("codeberg.org" "codeberg.org/api/v1" "codeberg.org" forge-gitea-repository)
     ("code.orgmode.org" "code.orgmode.org/api/v1" "code.orgmode.org" forge-gogs-repository)
     ("bitbucket.org" "api.bitbucket.org/2.0" "bitbucket.org" forge-bitbucket-repository)
     ("git.savannah.gnu.org" nil "git.savannah.gnu.org" forge-cgit**-repository)
     ("git.kernel.org" nil "git.kernel.org" forge-cgit-repository)
     ("repo.or.cz" nil "repo.or.cz" forge-repoorcz-repository)
     ("git.suckless.org" nil "git.suckless.org" forge-stagit-repository)
     ("git.sr.ht" nil "git.sr.ht" forge-srht-repository)))
 '(lsp-auto-configure t)
 '(minions-prominent-modes '(lsp-ui-mode lsp-mode))
 '(safe-local-variable-values
   '((projectile-project-run-cmd . "cargo run")
     (projectile-project-compilation-cmd . "cargo build -j8")
     (projectile-project-run-cmd . "cabal run")
     (projectile-project-compilation-cmd . "cabal build --enable-shared")
     (c-file-offsets
      (innamespace . 0)
      (substatement-open . 0)
      (c . c-lineup-dont-change)
      (inextern-lang . 0)
      (comment-intro . c-lineup-dont-change)
      (arglist-cont-nonempty . c-lineup-arglist)
      (block-close . 0)
      (statement-case-intro . ++)
      (brace-list-intro . ++)
      (cpp-define-intro . +))
     (c-auto-align-backslashes)
     (whitespace-style quote
                       (face trailing empty tabs))
     (whitespace-action)
     (projectile-project-run-cmd . "./build/GAME")
     (projectile-project-compilation-cmd . "cmake -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=RelWithDebInfo -S Linux -B Linux/build;cmake --build Linux/build --parallel 8;cmake -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=RelWithDebInfo -S Linux/App -B Linux/App/build; cmake --build Linux/App/build --parallel 8")
     (projectile-project-run-cmd . "ROOT=$(pwd);cd Linux/App/build;cmake -DBUILD_TESTING:BOOL=ON .;ctest --output-on-failure --verbose;cd $ROOT")
     (projectile-project-run-cmd . "ROOT=$(pwd);cmake -DBUILD_TESTING:BOOL=ON Linux/App/build;cd Linux/App/build;ctest --output-on-failure --verbose;cd $ROOT")
     (projectile-project-run-cmd . "cmake -DBUILD_TESTING:BOOL=ON Linux/App/build;ctest --output-on-failure --verbose Linux/App/build")
     (projectile-project-compilation-cmd . "cmake -GNinja -DCMAKE_BUILD_TYPE=RelWithDebInfo -S Linux -B Linux/build;cmake --build Linux/build --parallel 8;cmake -GNinja -DCMAKE_BUILD_TYPE=RelWithDebInfo -S Linux/App -B Linux/App/build")
     (projectile-project-compilation-cmd . "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -S . -B build;cmake --build build")
     (projectile-project-compilation-cmd . "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -S . -B build;cmake --build build --parallel 8")
     (projectile-project-run-cmd . "LD_LIBRARY_PATH=mir/build/lib/ MIR_SERVER_PLATFORM_PATH=mir/build/lib/server-modules/ ./build/X13 --enable-x11 --platform-display-libs=mir:x11 --platform-rendering-libs=mir:egl-generic --debug")
     (projectile-project-run-cmd . "LD_LIBRARY_PATH=./mir/build/lib/ MIR_SERVER_PLATFORM_PATH=.mir/build/lib/server-modules/ ./build/X13 --enable-x11 --platform-display-libs=mir:x11 --platform-rendering-libs=mir:egl-generic --debug")
     (projectile-project-compilation-cmd . "cmake -S . -B build;cmake --build build --parallel 8")
     (projectile-project-run-cmd . "./build/bin/miral-shell --enable-x11 --platform-display-libs=mir:x11 --platform-rendering-libs=mir:egl-generic --debug")
     (projectile-project-compilation-cmd . "cmake -S . -B build;cmake --build build")
     (projectile-project-compile-cmd . "cmake -S . -B build;cmake --build build")
     (projectile-project-compile-cmd . "cmake --build build")
     (projectile-project-run-cmd . "./build/X13 --enable-x11 --platform-display-libs=mir:x11 --platform-rendering-libs=mir:egl-generic --debug")
     (projectile-project-run-cmd . "cd build;./X13 --enable-x11 --platform-display-libs=mir:x11 --platform-rendering-libs=mir:egl-generic --debug")
     (company-clang-arguments "-I/usr/include/libxml2" "-I/usr/include/ncursesw"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; xclip
(use-package xclip)
(xclip-mode 1)

;; hello darkness my old friend
(use-package rustic
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
(setq auto-mode-alist (cons '("\\.ipp$" . c++-mode) auto-mode-alist))
(setq compilation-scroll-output t)
;; for regex-highlights and other stuff
;(use-package icicles)
;(icicle-mode)
(use-package popup)
;; tab management
(use-package bufler)
;; github etc
(use-package forge)
(use-package emacsql-sqlite)
(use-package code-review)
(add-hook 'code-review-mode-hook #'emojify-mode)
(setq code-review-fill-column 120)
(setq code-review-auth-login-marker 'forge)

;; nix
(use-package nix-mode
  :mode "\\.nix\\'")

;; music
(use-package tidal
  :config
  (setq tidal-boot-script-path "/usr/share/haskell-tidal/BootTidal.hs")
  (setq tidal-interpreter-arguments '("-package base"))
  :hook
  (tidal-mode . (lambda () (lsp-mode -1)))
  )
(use-package sclang)
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
(use-package org-edna)
(use-package org-journal)
(use-package org-contrib)
(use-package org-ql)
(use-package helm-org-ql)
(use-package hyperbole)
(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/org-roam"))
  (setq find-file-visit-truename t)
  (org-roam-db-autosync-mode)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n r" . org-roam-node-random)
   (:map org-mode-map
         (("C-c n i" . org-roam-node-insert)
          ("C-c n o" . org-id-get-create)
          ("C-c n t" . org-roam-tag-add)
          ("C-c n a" . org-roam-alias-add)
          ("C-c n l" . org-roam-buffer-toggle))))
  )

(use-package org-roam-bibtex)
(use-package org-roam-ql)
(use-package org-roam-ui)
;; emms
(use-package emms)
;; w3m
(use-package w3m)
;; I like helm
(use-package epg)
(use-package epa)
(require 'helm-init)
;(require 'emms-config.el)

; quran
; broken :(
;(use-package holy-books
;  :straight (:repo "alhassy/holy-books"
;             :host github
;             :type git
;             )
;  )
;(setq holy-books-quran-translation "131"  ;; The Clear Quran
;      holy-books-bible-version     "niv") ;; New International Version
;(holy-books-mode)

;translate
(use-package go-translate)
(setq gts-translate-list '(("ja" "en") ("fr" "en") ("en" "de") ("de" "en") ("pl" "en")))

(setq gts-default-translator
      (gts-translator
       :picker (gts-prompt-picker)
       :engines (list (gts-bing-engine) (gts-google-engine))
       :render (gts-buffer-render)))

(use-package delve
  :straight (:repo "publicimageltd/delve"
             :host github
             :type git)
  :after (org-roam)
  ;; this is necessary if use-package-always-defer is true
  :demand t
  :bind
  ;; the main entry point, offering a list of all stored collections
  ;; and of all open Delve buffers:
  (("C-c d" . delve))
  :config
  ;; set meaningful tag names for the dashboard query
  (setq delve-dashboard-tags '("X13" "GAME"))
  ;; optionally turn on compact view as default
  (add-hook #'delve-mode-hook #'delve-compact-view-mode)
 ;; turn on delve-minor-mode when Org Roam file is opened:
  (delve-global-minor-mode))

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
;;(use-package dap-mode
;;  :defer
;;  :custom
;;  (dap-auto-configure-mode t                           "Automatically configure dap.")
;;  (dap-auto-configure-features
 ;;  '(sessions locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
;;  :hook
;;  ((rustic-mode . dap-mode) (c++-mode . dap-mode))
;;  :config
;;  (require 'dap-lldb)
;;  (require 'dap-cpptools)
;;  
;;  )
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


(use-package minions
  :config (minions-mode 1))
(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter)) 
(add-to-list 'auto-mode-alist '("\\.*rc$" . shell-script-mode))

(provide 'custom.el)
;;; custom.el ends here
