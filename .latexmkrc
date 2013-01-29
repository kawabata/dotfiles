## eupLaTex (latexmk)
$latex  = 'euplatex -src-specials';
$bibtex = 'biber -u -U';
$dvi_previewer ='xdvi';
## eupLaTex (latexmk -pdfdvi)
$dvipdf = "dvipdfmx %O -o %D %S 2> ~/tmp.log";

# XeTeX/LuaTeX (latexmk -pdf)
$pdflatex = 'lualatex';

## PDF Previewr
# Acrobat Reader を使う場合 (非推奨。xdvi / xpdf を利用するのが良い)
# $pdf_previewer = 'open -a Adobe\ Reader.app %S';
$pdf_previewer = 'open %S';

# XPDFを使う場合
#$pdf_previewer = "start xpdf -remote %R %O %S";
#$pdf_update_method = 4;
#$pdf_update_command = "xpdf -remote %R -reload";
