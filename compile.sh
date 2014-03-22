#!/bin/bash


# Create and move to backup directory
cd ~/workspace/russia/demokratizatsiya
curl -o input.md http://pad.okfn.org/p/Demokratizatsiya/export/txt

pandoc -s -S --number-section --toc --from=markdown+yaml_metadata_block -H /home/aurelius/workspace/web/css/rmarkdown.css input.md -o article_demokr.html
# pdf
pandoc --toc --number-section --latex-engine=xelatex -V lang=english -V papersize:a4paper -V documentclass=scrartcl input.md -o article_demokr.pdf
# word
pandoc --toc --number-section input.md -o article_demokr.docx

git commit -am "article updated"
git push
