{ mkDerivation, base, gtk, stdenv, xmonad }:
mkDerivation {
  pname = "xmonad-screenshot";
  version = "0.1.1.0";
  src = ./.;
  buildDepends = [ base gtk xmonad ];
  homepage = "http://github.com/supki/xmonad-screenshot";
  description = "Workspaces screenshooting utility for XMonad";
  license = stdenv.lib.licenses.mit;
}
