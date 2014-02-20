#!/bin/bash
# html
pandoc -s -S --number-section --toc -H /home/aurelius/workspace/web/css/rmarkdown.css input.md -o article_demokr.html
# pdf
pandoc --toc --number-section --latex-engine=xelatex -V lang=english -V papersize:a4paper -V documentclass=scrartcl input.md -o article_demokr.pdf
# word
pandoc --toc --number-section input.md -o article_demokr.docx
