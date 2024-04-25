{
  description = "Discord API wrapper for OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs @ {
    self,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        inherit (pkgs) ocamlPackages mkShell;
        inherit (ocamlPackages) buildDunePackage;
        version = "0.0.1";
      in {
        devShells = {
          default = mkShell {
            inputsFrom = [
              self'.packages.default
            ];
            buildInputs = [ocamlPackages.utop];
            packages = builtins.attrValues {
              inherit (pkgs) clang_17 clang-tools_17 pkg-config;
              inherit (ocamlPackages) ocaml-lsp ocamlformat ocamlformat-rpc-lib odoc;
            };
          };
        };

        packages = {
          default = buildDunePackage {
            inherit version;
            pname = "okitten";
            propagatedBuildInputs = with ocamlPackages; [
              self'.packages.websocket
              self'.packages.websocket-lwt-unix
              cohttp
              cohttp-lwt-unix
              conduit-lwt
              dune-build-info
              fmt
              ipaddr
              logs
              lwt
              lwt_ppx
              ppx_deriving
              ppx_deriving_yojson
              yojson
            ];
            src = ./.;
          };

          websocket-lwt-unix = buildDunePackage {
            version = "2.16";
            pname = "websocket-lwt-unix";
            src = builtins.fetchGit {
              url = "git@github.com:vbmithr/ocaml-websocket";
              rev = "a5d7cb0710e6df49cc0c328b2ed28be4d1a397b4";
            };
            propagatedBuildInputs = with ocamlPackages; [
              self'.packages.websocket
              lwt_log
              cohttp-lwt-unix
            ];
          };

          websocket = buildDunePackage {
            version = "2.16";
            pname = "websocket";
            src = builtins.fetchGit {
              url = "git@github.com:vbmithr/ocaml-websocket";
              rev = "a5d7cb0710e6df49cc0c328b2ed28be4d1a397b4";
            };
            propagatedBuildInputs = with ocamlPackages; [
              cohttp
              base64
              conduit
              cohttp
              ocplib-endian
              astring
              mirage-crypto-rng
            ];
          };
        };
      };
    };
}
