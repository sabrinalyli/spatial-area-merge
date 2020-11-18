{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/f01bfd6843cada594383a5ab67c01f30b571a185.tar.gz") {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.which
    pkgs.htop
    pkgs.zlib
    pkgs.R
    pkgs.rPackages.dplyr
    pkgs.rPackages.reshape2
    pkgs.rPackages.purrr
    pkgs.rPackages.magrittr
    pkgs.rPackages.ggplot2
    pkgs.rPackages.stringr
    pkgs.rPackages.sf
  ];
}


