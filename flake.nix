{
  description = "A Nix-flake-based Scala development environment";
  
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      javaVersion = 17; # Change this value to update the whole stack

      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };
      });


    in
    {
      overlays.default = final: prev:
        let
          jdk = prev."jdk${toString javaVersion}";
        in
        {
          sbt = prev.sbt.override { jre = jdk; };
          scala = prev.scala_3.override { jre = jdk; };
        };

      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            stdenv
            sbt
            openjdk
            boehmgc
            libunwind
            clang
            zlib
            secp256k1
            nodejs
            yarn 
            just
          ];
          packages = with pkgs; [ 
            scala 
            sbt 
            coursier 
            scala-cli 
          ];
        };
      });
    };
}
