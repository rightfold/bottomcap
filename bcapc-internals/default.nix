{ stdenv, texlive }:
let
    texlive' = texlive.combine {
        inherit (texlive)
            scheme-basic
            helvetic
            listings
            parskip
            pgf
                xcolor
            tcolorbox
                environ
                etoolbox
                trimspaces
        ;
    };
in
    stdenv.mkDerivation {
        name = "bcapc-internals";
        src = ./.;
        buildInputs = [
            texlive'
        ];
        buildPhase = ''
            for i in {1..2}; do
                pdflatex -halt-on-error src/index.tex
            done
        '';
        installPhase = ''
            mkdir "$out"
            mv index.pdf "$out"
        '';
    }
