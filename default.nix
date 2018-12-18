{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
{
    bcapc = nixpkgs.haskellPackages.callPackage ./bcapc {};
    bcapc-internals = nixpkgs.callPackage ./bcapc-internals {};
}
