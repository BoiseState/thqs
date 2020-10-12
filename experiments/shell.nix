let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in pkgs.mkShell {
  buildInputs = [
    pkgs.python38
    pkgs.python38Packages.numpy
    pkgs.python38Packages.prettytable
  ];
}
