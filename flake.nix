{
  description = "IPLD Brainz";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays =
        [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          tweetdelete =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell = {
                tools = {
                  cabal = {};
                  hlint = {};
                  haskell-language-server = {};
                };
                withHoogle = true;
              };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.tweetdelete.flake { };
    in flake // rec {
      defaultPackage = flake.packages."tweetdelete:exe:tweetdelete-exe";
      nixosModules.tweetdelete = {
        imports = [./module.nix ];
        nixpkgs.overlays = overlays;
      };
      nixosConfigurations.testContainer = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          self.nixosModules.${system}.tweetdelete
          {
            services.tweetdelete =
              {
                enable = true;

                accounts =
                  { alice =
                      {
                        deleteAll = true;
                        deleteLikes = false;
                      };
                    bob =
                      {
                        deleteBefore = "1 week";
                        deleteLikes = true;
                      };
                  };

              };
          }
        ];


      };
    });
}
