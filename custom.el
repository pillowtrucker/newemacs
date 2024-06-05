(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-clang-executable "clang")
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(custom-safe-themes
   '("871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" "046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(dape-breakpoint-global-mode t)
 '(dape-buffer-window-arrangement 'gud)
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
 '(lsp-clients-clangd-args
   '("--header-insertion=iwyu" "--completion-style=detailed" "--background-index" "-j=8" "--clang-tidy" "--all-scopes-completion" "--cross-file-rename" "--header-insertion-decorators" "--rename-file-limit=0" "--use-dirty-headers"))
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
 '(org-fold-core-style 'overlays)
 '(safe-local-variable-values
   '((projectile-project-run-cmd . "./build/GAME")
     (projectile-project-compilation-cmd . "cmake -S . -B build;cmake --build build")
     (projectile-project-run-cmd . "cabal run")
     (projectile-project-compilation-cmd . "cabal build --enable-shared")
     (eval remove-hook 'before-save-hook 'lsp-format-buffer t)
     (projectile-project-run-cmd . "~/godot-mine/result/bin/godot4 -e --path ~/if_your_happy_and_you_know_it/")
     (projectile-project-compilation-cmd . "cmake -GNinja -S . -B build -DCMAKE_BUILD_TYPE=Debug -DTOOTING_WARNING_AS_ERROR=OFF -DBUILD_GUI=OFF -DWITH_DEMOS=OFF -DWITH_INSTRUMENTS=OFF -DWITH_WAVETABLES=OFF -DSYSTEM_SDL2=ON -DSYSTEM_ZLIB=ON -DSYSTEM_RTMIDI=ON -DSYSTEM_FFTW=OFF -DSYSTEM_FMT=ON -DSYSTEM_LIBSNDFILE=ON -DSYSTEM_FREETYPE=OFF -DWITH_RENDER_OPENGL=OFF -DWITH_RENDER_OPENGL1=OFF -DWITH_RENDER_SDL=OFF -DWITH_RENDER_DX9=OFF -DWITH_RENDER_DX11=OFF -DUSE_GLES=OFF -DUSE_SDL2=ON;cmake --build build --parallel 8"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
