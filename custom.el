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
