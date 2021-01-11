{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, stdenv, pandoc, shake, cpphs, lmodern, zip, happstack-server, happstack-hsp, happstack-jmacro, hsx-jmacro, shakespeare, hsx2hs, reform, reform-happstack, reform-hsp, web-routes, web-routes-happstack, web-routes-th, web-routes-boomerang, web-routes-hsp, acid-state, ixset, lifted-base }:
      mkDerivation {
        pname = "";
        version = "";
        src = ./.;
        buildDepends = [ pandoc shake pkgs.zip lifted-base happstack-server happstack-jmacro hsx2hs happstack-hsp hsx-jmacro shakespeare reform reform-hsp reform-happstack web-routes web-routes-th web-routes-happstack web-routes-boomerang web-routes-hsp acid-state ixset ];
        buildTools = [ cpphs lmodern (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-medium; }) pkgs.zip ];
        isLibrary = false;
        isExecutable = false;
        license = stdenv.lib.licenses.unfree;
      };

  drv = pkgs.haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
