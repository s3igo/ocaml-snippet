{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    opam-nix.url = "github:tweag/opam-nix";
    neovim-builder.url = "github:s3igo/dotfiles?dir=neovim";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      opam-nix,
      neovim-builder,
      opam-repository,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        package = "ocaml_snippet";
        pkgs = import nixpkgs { inherit system; };
        on = opam-nix.lib.${system};
        devPackagesQuery = {
          ocamlformat = "0.26.1";
        };
        query = devPackagesQuery // {
          ocaml-base-compiler = "*";
        };
        scope = on.buildOpamProject' { repos = [ opam-repository ]; } ./. query;
        overlay = final: prev: {
          ${package} = prev.${package}.overrideAttrs (_: {
            doNixSupport = false;
          });
        };
        scope' = scope.overrideScope' overlay;
        main = scope'.${package};
        devPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
        snakeToKebab = str: builtins.replaceStrings [ "_" ] [ "-" ] str;
      in
      {
        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/${snakeToKebab package}";
        };
        packages = {
          default = main;
          neovim = neovim-builder.withModules {
            inherit system pkgs;
            grammars = [
              "ocaml"
              "ocaml_interface"
              "ocamllex"
            ];
            modules = with neovim-builder.modules; [
              nix
              im-select
              {
                autoCmd = [
                  {
                    event = "FileType";
                    pattern = "ocaml";
                    command = "setlocal shiftwidth=2";
                  }
                ];
                plugins.lsp.servers.ocamllsp.enable = true;
              }
            ];
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ main ];
          packages = devPackages ++ [ self.packages.${system}.neovim ];
        };
      }
    );
}
