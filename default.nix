# nix-env -if ./default.nix
# nix-env --uninstall anki-sync
let pkgs = import
  ( fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/25cf937a30bf0801447f6bf544fc7486c6309234.tar.gz";
    sha256 = "16incdl8chihc1aw7i18mhv8k848iv7ib4wyn5qn485241c19z82";
  }) { };
    ghc = pkgs.haskell.packages.ghc943.ghcWithPackages (pkgs: with pkgs; [
      pandoc
    ]);
in {
  ghc = pkgs.stdenv.mkDerivation {
    name = "anki-sync";
    buildInputs = [ghc];
    src = ./.;
    buildPhase = "ghc app/*.hs src/*.hs";
    installPhase = ''
      mkdir -p $out/bin
      cp app/ankiMd $out/bin/
    '';
  };
}
