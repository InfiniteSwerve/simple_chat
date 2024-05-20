{
  description = "Simple Chat";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "github:nix-ocaml/nix-overlays";
  };

  outputs = { self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
          # Set the OCaml set of packages to the 5.1 release line
          (self: super: { ocamlPackages = super.ocaml-ng.ocamlPackages_5_1; })
        ];
        inherit (pkgs) ocamlPackages;
      in

      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with ocamlPackages; [
            ocaml
            dune_3
            lwt
            logs
            findlib
            ocaml-lsp
            ocamlformat
            lambda-term
            ppx_deriving_yojson
            ppx_deriving
          ];
        };
      });
}
