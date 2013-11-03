# -*- mode: perl -*-
## latex
$latex  = 'uplatex -src-specials -interaction=nonstopmode -synctex=1';
#$latex = 'xelatex -no-pdf -src-specials -file-line-error -shell-escape -synctex=1';

## pdflatex
# LuaTeX は オプション引数は"--"とマイナスが２つ。
#$pdflatex = 'xelatex -file-line-error -shell-escape -synctex=1';
$pdflatex = 'lualatex --interaction=nonstopmode --shell-escape --synctex=1';

## bibtex
$bibtex = 'biber -u -U';
#$bibtex = 'upbibtex %O %B';
#$bibtex = 'bibtexu %O %B';

## dvi (for $latex)
$dvi_previewer ='xdvi';
# $dvi_previewer = 'start dviout'; # -pv option

## dvipdf (for $latex)
$dvipdf = "dvipdfmx %O -o %D %S";
#$dvipdf = "xdvipdfmx %O -o %D %S";

## pdf_previewer
$pdf_previewer = 'open -a skim'; # MacOS
# $pdf_previewer = 'evince' # Ubuntu
# $pdf_previewer = '"C:\Program Files\SumatraPDF\SumatraPDF.exe" %O %S'; # windows

## makeindex
# $makeindex = 'mendex %O -U -o %D %S'
# $makeindex = 'texindy %O -o %D %S'
# $makeindex = 'mendex -r -c -s jind.ist';

## clean
$clean_ext = 'bbl rel %R-blx.bib %R.synctex.gz';

# $pdf_mode = 3;
# $pdf_update_method = 0;
