{ pkgs ? import <nixpkgs> {} }:
let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
	evil
	evil-leader
	nlinum-relative
	helm
	projectile
	helm-projectile
	diminish
	haskell-mode
	flycheck
	zenburn-theme
	nix-mode
	ace-window
	flycheck-haskell
	nix-sandbox
  ]))
