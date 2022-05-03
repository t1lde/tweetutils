{
  description = "tweetutils";

  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs";

  outputs = inputs@{ self, nixpkgs, haskell-nix, ... }:
    let
      project = "tweetutils";
      projectApp = "tweetutils-exe";

      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs { inherit system; overlays = [ haskell-nix.overlay ]; inherit (haskell-nix) config; };
      nixpkgsFor' = system: import nixpkgs { inherit system; inherit (haskell-nix) config; };

      ghcVersion = "ghc8107";

      tools = {};

      completionsFor = system: nixpkgs.lib.genAttrs ["fish" "bash" "zsh"] (shell:
        let
          pkgs = nixpkgsFor' system;
        in
          pkgs.callPackage ./completions.nix {
            inherit shell;
            name = "completions.${shell}";
            appExe = projectApp;
            app = self.flake.${system}.packages."${project}:exe:${projectApp}";
          }

      );

      projectFor = system:
        let pkgs = nixpkgsFor system; in
        let pkgs' = nixpkgsFor' system; in
        (nixpkgsFor system).haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = ghcVersion;
          ## extraSources = [
          ##   {
          ##     src = inputs.etc;
          ##     subdirs = [
          ##     ];
          ##   }
          ## ];
          shell = {
            withHoogle = true;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [];

            inherit tools;

            additional = ps: [
            ];
          };
        };

    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });
      packages = perSystem (system: self.flake.${system}.packages // { completions = completionsFor system; });
      devShell = perSystem (system: self.flake.${system}.devShell);
      defaultApp = perSystem (system: self.flake.${system}.packages."${project}:exe:${projectApp}");
    };
}
