{nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc}:

with nixpkgs;

let
    dontCheck = pkgs.haskell.lib.dontCheck;
    haskellPackages_ = haskellPackages.override {
        overrides = self: super: {
                   twitter-conduit = dontCheck (super.twitter-conduit);
                   aeson = super.aeson.overrideAttrs (oldAttrs: rec {
                       configureFlags = [
                           "-ffast"
                       ];
                   });
        };
    };
in
pkgs.haskell.lib.buildStackProject {
    name = "tweetdelete";
    buildInputs = [
        haskellPackages_.twitter-conduit
        haskellPackages_.twitter-types
        haskellPackages_.twitter-types-lens
        pkgs.gmp
        pkgs.libffi
        pkgs.zlib
        haskellPackages_.aeson
    ];
    inherit ghc;
}
