{ mkDerivation
, base
, bytestring
, cereal
, dlist
, hashable
, hspec
, lens
, mtl
, text
, transformers
, unordered-containers
, vector }:
mkDerivation {
    # That's what it's called!
    pname = "bcapc";

    # Keep the version in sync with the one in bcapc.cabal.
    version = "0.0.0.0";

    # For now, just set this to null. We have to set it to something.
    license = null;

    # Otherwise it'll try to download it from Hackage.
    src = ./.;

    # Keep the dependencies in sync with those in bcapc.cabal.
    buildDepends = [
        base
        bytestring
        cereal
        dlist
        hashable
        hspec
        lens
        mtl
        text
        transformers
        unordered-containers
        vector
    ];
}
