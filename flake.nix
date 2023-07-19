{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";

  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (_self: super: {
            ayatana-appindicator3 = super.libayatana-appindicator;
            ayatana-indicator3 = super.libayatana-indicator;
          })
        ];
      };
      tray-hs = pkgs.haskellPackages.developPackage {
            name = "tray-hs";
            root = ./.;
            modifier = with pkgs; haskell.lib.compose.addPkgconfigDepends [
              gtk3
              glib
              pcre
              pcre2
              util-linux
              libselinux
              libsepol
              libthai
              libdatrie
              libxkbcommon
              libepoxy
              xorg.libXdmcp
              xorg.libXtst
              libayatana-appindicator
              libayatana-indicator
            ];
          };
    in
    {
      packages.${system}.default = tray-hs;
      devShells.${system}.default = pkgs.mkShell {
        inputsFrom = [ tray-hs.env ];
        packages = with pkgs.haskellPackages; [
          cabal-install

          haskell-language-server
          hsc2hs
          ghcid

          cabal-fmt
          # fourmolu
          stylish-haskell
        ];

        shellHook = ''
          export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${pkgs.lib.makeLibraryPath [
            pkgs.zlib
            pkgs.libayatana-appindicator
            pkgs.libayatana-indicator
          ]}"
        '';
        
      };

    };
}
