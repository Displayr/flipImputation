{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipImputation";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = "Functions for imputing data using mice and hot.deck.";
  propagatedBuildInputs = with pkgs.rPackages; [ 
    hot_deck
    mice
    flipU
  ];
}
