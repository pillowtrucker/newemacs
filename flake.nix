{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      #      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, fenix, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            fenix.overlays.default
            (final: prev: {

              llvmPackages = final.llvmPackages_19;
              #              racket = prev.racket.overrideAttrs (oldAttrs: {
              #                configureFlags = prev.lib.lists.remove "--disable-libs"
              #                 oldAttrs.configureFlags;
              #              });
            })
          ];
        };
        # lib = nixpkgs.lib;
      in {
        devShells.default = pkgs.mkShell {
          #          stdenv = pkgs.llvmPackages_19.stdenv;
          nativeBuildInputs = with pkgs; [
            nixfmt-rfc-style
            python3
            git
            eask-cli
            slint-lsp
            fenix.packages.${system}.complete.toolchain
            # Package location
            pkg-config
            # Window and Input
            # x11
            openssl
            fontconfig
            dbus # for nvidia-powerd
            udev # device management
            #              clangStdenv
            llvmPackages_19.llvm
            llvmPackages_19.stdenv
            llvmPackages_19.stdenv.cc
            llvmPackages_19.stdenv.cc.cc.lib
            zlib
            ncurses
            #            libvterm-neovim
            #            libvterm
            libtool
          ];
          dontWrapQtApps = true;
          APPEND_LIBRARY_PATH = with pkgs;
            lib.makeLibraryPath [
              fenix.packages.${system}.complete.toolchain
              llvmPackages_19.llvm
              ncurses

              llvmPackages_19.stdenv.cc
              llvmPackages_19.stdenv.cc.cc
              llvmPackages_19.stdenv.cc.cc.lib
              openssl
              fontconfig
              zlib
              cmake
              #              libvterm-neovim
              #             libvterm
            ];

          shellHook = ''
            export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$APPEND_LIBRARY_PATH"
            #eval $(echo "''${qtWrapperArgs[@]}"|perl -n -e '$_ =~ s/--prefix (\w+) : ([\w-.\/]+)/export ''${1}="''${2}:\''${''${1}}";/g;print')
          '';

        };
      });
}
