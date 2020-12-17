{ pkgs ? import <nixpkgs> {} }:
let
  jdk = pkgs.jdk11;
  clojure = pkgs.clojure;
  maven = (pkgs.maven.override {
    jdk = jdk;
  });
  leiningen = (pkgs.leiningen.override {
    jdk = jdk;
  });
in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    jdk
    clojure
    leiningen
    maven

    parallel
    python38
    python38Packages.numpy
    python38Packages.prettytable
    tokei

    bashInteractive
  ];

  buildInputs = [
    jdk
    clojure
  ];
}
