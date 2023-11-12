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
 '(lsp-auto-configure t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; xclip
(use-package xclip)
(xclip-mode 1)

;; for regex-highlights and other stuff
(use-package icicles)
(icicle-mode)
(use-package popup)
(use-package burly)

;; nix
(use-package nix-mode
  :mode "\\.nix\\'")


;; cmake
(use-package cmake-mode)
(use-package cmake-font-lock)


;; for assigning blame
(use-package git-timemachine)

;; lol bolt
(use-package rmsbolt)

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

(use-package dap-mode
  :defer
  :custom
  (dap-auto-configure-mode t                           "Automatically configure dap.")
  (dap-auto-configure-features
   '(sessions locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
  :config
  (require 'dap-lldb)
  
  )

(add-to-list 'auto-mode-alist '("\\.*rc$" . shell-script-mode))

(provide 'custom.el)
;;; custom.el ends here
