let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
  jdk = pkgs.jdk11;
  clojure = pkgs.clojure;
  maven = (pkgs.maven.override {
    jdk = jdk;
  });
  leiningen = (pkgs.leiningen.override {
    jdk = jdk;
  });
in pkgs.mkShell {
  buildInputs = [
    jdk
    clojure
    leiningen
    maven
  ];
}
