The source code for "enumerator" is literate. To build the library from scratch,
install the "anansi" application and then run:

    anansi -o hs/ src/enumerator.anansi

To generate the woven PDF, install NoWeb and then run:

    anansi -w -l latex-noweb -o enumerator.tex src/enumerator.anansi
    xelatex enumerator.tex
    xelatex enumerator.tex
