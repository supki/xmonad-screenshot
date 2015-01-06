{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "xmonad-screenshot";
  version = "0.1.0.0";
  src = builtins.filterSource (_: type: type != "unknown") ./.;
  buildDepends = with haskellPackages; [ gtk xmonad ];
  meta = {
    homepage = "http://github.com/supki/xmonad-screenshot";
    description = "Workspaces screenshooting utility for XMonad";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
