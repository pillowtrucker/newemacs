;;; custom.el --- my dingdongs
;;; Commentary:

;;; Code:


;; color theme
(use-package color-theme-sanityinc-tomorrow)
(use-package gruvbox-theme)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(custom-safe-themes
   '("871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" "046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(dape-breakpoint-global-mode t)
 '(dape-buffer-window-arrangment 'gud)
 '(dape-configs
   '((codelldb-cc modes
                  (c-mode c-ts-mode c++-mode c++-ts-mode)
                  command "~/.emacs.d/debug-adapters/codelldb/extension/adapter/codelldb" command-cwd dape-cwd-fn :type "lldb" :request "launch" command-args
                  ("--port" :autoport)
                  ensure dape-ensure-command port :autoport fn dape-config-autoport :cwd dape-cwd-fn :program dape-find-file :args
                  [])
     (codelldb-rust modes
                    (rust-mode rust-ts-mode)
                    command "~/.emacs.d/debug-adapters/codelldb/extension/adapter/codelldb" command-cwd dape-cwd-fn :type "lldb" :request "launch" command-args
                    ("--port" :autoport "--settings" "{\"sourceLanguages\":[\"rust\"]}")
                    ensure dape-ensure-command port :autoport fn dape-config-autoport :cwd dape-cwd-fn :program dape-find-file :args
                    []
                    env "{\"CARGO_MANIFEST_DIR\":\"${workspaceFolder}\"}" :environment "[{\"name\":\"CARGO_MANIFEST_DIR\", \"value\":\"${workspaceFolder}\"}]")
     (cpptools modes
               (c-mode c-ts-mode c++-mode c++-ts-mode)
               command "~/.emacs.d/debug-adapters/cpptools/extension/debugAdapters/bin/OpenDebugAD7" :type "cppdbg" :request "launch" ensure dape-ensure-command :cwd dape-cwd-fn :program dape-find-file :MIMode "lldb")
     (debugpy modes
              (python-mode python-ts-mode)
              command "python3" command-args
              ("-m" "debugpy.adapter")
              :type "executable" :request "launch" ensure
              (lambda
                (config)
                (let
                    ((python
                      (dape--config-eval-value
                       (plist-get config 'command))))
                  (unless
                      (zerop
                       (call-process-shell-command
                        (format "%s -c \"import debugpy.adapter\"" python)))
                    (user-error "%s module debugpy is not installed" python))))
              :cwd dape-cwd-fn :program dape-find-file-buffer-default :justMyCode nil :showReturnValue t)
     (dlv command "dlv" command-args
          ("dap" "--listen" "127.0.0.1::autoport")
          command-cwd dape-cwd-fn :type "debug" :request "launch" modes
          (go-mode go-ts-mode)
          ensure dape-ensure-command fn dape-config-autoport port :autoport :cwd dape-cwd-fn :program dape-cwd-fn)
     (flutter command "flutter" command-args
              ("debug_adapter")
              command-cwd dape-cwd-fn :type "dart" ensure dape-ensure-command modes
              (dart-mode)
              :cwd dape-cwd-fn :program dape-find-file-buffer-default :toolArgs
              #[0 "\300\301\302\303!\"\207"
                  [vector "-d" read-string "Device id: "]
                  4])
     (godot port 6006 :type "server" :request "launch" modes
            (gdscript-mode)
            :cwd dape-cwd-fn)
     (js-debug-node modes
                    (js-mode js-ts-mode)
                    command "node" command-cwd "~/.emacs.d/debug-adapters/js-debug" :type "pwa-node" ensure
                    #[257 "\300\1!\210\301\302\303\3\304\"!\302\303\4\305\"@!\"\306\1!?\205\36\0\307\310\2\"\207"
                          [dape-ensure-command file-name-concat dape--config-eval-value plist-get command-cwd command-args file-exists-p user-error "File %S does not exist"]
                          7 "\12\12(fn CONFIG)"]
                    command-args
                    ("src/dapDebugServer.js" :autoport)
                    port :autoport fn dape-config-autoport :cwd dape-cwd-fn :program dape-find-file-buffer-default :outputCapture "console" :sourceMapRenames t :pauseForSourceMap nil :autoAttachChildProcesses t :console "internalConsole" :killBehavior "forceful")
     (js-debug-chrome modes
                      (js-mode js-ts-mode)
                      command "node" command-cwd "~/.emacs.d/debug-adapters/js-debug" :type "pwa-chrome" ensure
                      #[257 "\300\1!\210\301\302\303\3\304\"!\302\303\4\305\"@!\"\306\1!?\205\36\0\307\310\2\"\207"
                            [dape-ensure-command file-name-concat dape--config-eval-value plist-get command-cwd command-args file-exists-p user-error "File %S does not exist"]
                            7 "\12\12(fn CONFIG)"]
                      command-args
                      ("src/dapDebugServer.js" :autoport)
                      port :autoport fn dape-config-autoport :trace t :url
                      #[0 "\300\301\302\"\207"
                          [read-string "Url: " "http://localhost:3000"]
                          3]
                      :webRoot dape-cwd-fn :outputCapture "console")
     (lldb-vscode modes
                  (c-mode c-ts-mode c++-mode c++-ts-mode rust-mode rust-ts-mode)
                  command "lldb-vscode" :type "lldb-vscode" ensure dape-ensure-command :cwd dape-cwd-fn :program dape-find-file)
     (netcoredbg modes
                 (csharp-mode csharp-ts-mode)
                 command "netcoredbg" :request "launch" ensure dape-ensure-command command-args
                 ["--interpreter=vscode"]
                 :cwd dape-cwd-fn :program dape-find-file :stopAtEntry nil)))
 '(dape-cwd-fn 'projectile-project-root)
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
 '(lsp-haskell-server-path "haskell-language-server")
 '(lsp-nix-nil-formatter ["nixfmt"])
 '(lsp-rust-analyzer-cargo-watch-command "clippy")
 '(lsp-rust-analyzer-diagnostics-disabled ["parse_async_move_block_in_2015"])
 '(lsp-rust-analyzer-diagnostics-enable-experimental t)
 '(lsp-rust-analyzer-display-chaining-hints t)
 '(lsp-rust-analyzer-display-closure-return-type-hints t)
 '(lsp-rust-analyzer-display-lifetime-elision-hints-enable t)
 '(lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
 '(lsp-rust-analyzer-display-parameter-hints t)
 '(lsp-rust-analyzer-display-reborrow-hints t)
 '(lsp-rust-analyzer-macro-expansion-method 'rustic-analyzer-macro-expand)
 '(minions-prominent-modes '(lsp-ui-mode lsp-mode))
 '(safe-local-variable-values
   '((eval progn
           (require 'lisp-mode)
           (defun emacs27-lisp-fill-paragraph
               (&optional justify)
             (interactive "P")
             (or
              (fill-comment-paragraph justify)
              (let
                  ((paragraph-start
                    (concat paragraph-start "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
                   (paragraph-separate
                    (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
                   (fill-column
                    (if
                        (and
                         (integerp emacs-lisp-docstring-fill-column)
                         (derived-mode-p 'emacs-lisp-mode))
                        emacs-lisp-docstring-fill-column fill-column)))
                (fill-paragraph justify))
              t))
           (setq-local fill-paragraph-function #'emacs27-lisp-fill-paragraph))
     (geiser-repl-per-project-p . t)
     (eval with-eval-after-load 'yasnippet
           (let
               ((guix-yasnippets
                 (expand-file-name "etc/snippets/yas"
                                   (locate-dominating-file default-directory ".dir-locals.el"))))
             (unless
                 (member guix-yasnippets yas-snippet-dirs)
               (add-to-list 'yas-snippet-dirs guix-yasnippets)
               (yas-reload-all))))
     (eval setq-local guix-directory
           (locate-dominating-file default-directory ".dir-locals.el"))
     (eval add-to-list 'completion-ignored-extensions ".go")
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (geiser-guile-binary "guix" "repl")
     (geiser-insert-actual-lambda)
     (projectile-project-run-cmd . "cargo run -j8 -- --shadow-resolution 16384 --shadow-distance 100")
     (projectile-project-run-cmd . "cargo run -j8 -- --shadow-resolution 8192 --shadow-distance 50 --directional-light \"-1.0, -4.0, 2.0\"  --gltf-disable-directional-lights")
     (projectile-project-run-cmd . "cargo run -j8 -- --shadow-resolution 8192 --shadow-distance 50 --directional-light \"-1.0, -4.0, 2.0\" ")
     (projectile-project-run-cmd . "cargo run -j8")
     (projectile-project-run-cmd . "cargo runcc -c runcc.yml")
     (projectile-project-run-cmd . "cargo runcc")
     (dape)
     (projectile-project-run-cmd . "cargo run")
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
;(use-package nix-mode
;  :mode "\\.nix\\'")

;; music
;(use-package tidal
;  :config
;  (setq tidal-boot-script-path "/usr/share/haskell-tidal/BootTidal.hs")
;  (setq tidal-interpreter-arguments '("-package base"))
;  :hook
;  (tidal-mode . (lambda () (lsp-mode -1)))
;  )
;(use-package sclang)
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

;(use-package delve
;  :straight (:repo "publicimageltd/delve"
;             :host github
;             :type git)
;  :after (org-roam)
;  ;; this is necessary if use-package-always-defer is true
;  :demand t
;  :bind
;  ;; the main entry point, offering a list of all stored collections
;  ;; and of all open Delve buffers:
;  (("C-c d" . delve))
;  :config
;  ;; set meaningful tag names for the dashboard query
;  (setq delve-dashboard-tags '("X13" "GAME"))
;  ;; optionally turn on compact view as default
;  (add-hook #'delve-mode-hook #'delve-compact-view-mode)
; ;; turn on delve-minor-mode when Org Roam file is opened:
;  (delve-global-minor-mode))

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
;(use-package geiser-guile)
;(use-package geiser)
;(use-package guix)
;(use-package lsp-scheme
;  :straight (
;             :repo "rgherdt/emacs-lsp-scheme"
;                   :host codeberg
;                   :type git)
;
;  
;  
;  )
;; Assuming the Guix checkout is in ~/src/guix.
;(with-eval-after-load 'geiser-guile
;  (dolist (e '("." "..." "/home/wrath/.guix-profile/share/guile/site/2.2" "~/.config/guix/current/share/guile/site/3.0/" "~/nonguix/nongnu" "~/nonguix/nonguix")) (add-to-list 'geiser-guile-load-path e)))
;(add-to-list 'load-path "~/.emacs.d/lisp/emacs-lsp-scheme/")
;(add-to-list 'load-path "~/.guix-profile/bin/")
;(require 'lsp-scheme)
;(setq lsp-scheme-implementation "guile") ;;; also customizable
;(add-hook 'scheme-mode-hook #'lsp-scheme-guile)
;(add-hook 'scheme-mode-hook #'lsp-scheme)



(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)
(add-hook 'nix-mode-hook
          (lambda () (add-hook 'before-save-hook 'nix-format-before-save nil 'local)))
;(setq lsp-nix-nil-server-path "/home/wrath/.cargo/bin/nil")
(with-eval-after-load 'lsp-mode
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                     :major-modes '(nix-mode)
                     :priority -10
                     :server-id 'nixd)))
(use-package envrc
  :init (envrc-global-mode)
  )
(with-eval-after-load 'envrc
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))
(use-package diredc
  :init (global-set-key (kbd "S-<f11>") 'diredc)
  )
(lsp-defcustom lsp-nix-nil-max-mem 10000
  "Max Memory MB"
  :type 'number
  :group 'lsp-nix-nil
  :lsp-path "nil.nix.maxMemoryMB"
  :package-version '(lsp-mode . "8.0.1"))
(lsp-defcustom lsp-nix-nil-auto-eval-inputs t
  "Auto-evaluate inputs"
  :type 'boolean
  :group 'lsp-nix-nil
  :lsp-path "nil.nix.flake.autoEvalInputs"
  :package-version '(lsp-mode . "8.0.1"))
(provide 'custom.el)
;;; custom.el ends here
