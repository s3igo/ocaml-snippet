{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    neovim.url = "github:s3igo/dotfiles?dir=neovim";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      neovim,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages.neovim = neovim.withModules {
          inherit system pkgs;
          modules = with neovim.nixosModules; [ nix ];
        };

        devShells.default = pkgs.mkShell { buildInputs = [ self.packages.${system}.neovim ]; };
      }
    );
}
