all:index.html

index.html:index.Rmd ./css/revealOpts.css
	Rscript -e "rmarkdown::render('$<')"
	Rscript -e "knitr::purl('$<', documentation = 0)"

clean:
	rm index.html
	rm index_files/