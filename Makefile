# Makefile for generating the bde R package
#
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)
R_FILES := $(wildcard R/*.R)
STAN_FILES := $(wildcard stan/*.stan)
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES)

ifeq ($(OS),Windows_NT) 
	RM = rm -rf
	CP = cp -f
	CD = cd
else
	RM = rm -rf
	CP = cp -f
	CD = cd
endif

all: install clean

./inst/doc/*.html: ./vignettes/*.Rmd
	Rcmd INSTALL --build .
	Rscript -e "library(devtools); build_vignettes()"
	
./data/*.RData: $(STAN_FILES)
	$(RM) data/*.RData
	Rscript -e "reg01a <- readLines('./stan/regression01_year.stan');                      save(reg01a, file = './data/reg01a.RData')"
	Rscript -e "reg01b <- readLines('./stan/regression01_year_area.stan');                 save(reg01b, file = './data/reg01b.RData')"
	Rscript -e "reg01c <- readLines('./stan/regression01_year_area_method.stan');          save(reg01c, file = './data/reg01c.RData')"
	Rscript -e "reg01d <- readLines('./stan/regression01_year_area_method_category.stan'); save(reg01d, file = './data/reg01d.RData')"
	Rscript -e "reg02b <- readLines('./stan/regression02_year_area.stan');                 save(reg02b, file = './data/reg02b.RData')"
	Rscript -e "reg02c <- readLines('./stan/regression02_year_area_method.stan');          save(reg02c, file = './data/reg02c.RData')"
	
install: $(PKG_FILES) ./inst/doc/*.html ./data/*.RData
	Rcmd build --no-build-vignettes .
	Rcmd INSTALL --html .
	
DESCRIPTION NAMESPACE: $(R_FILES)
	Rscript version_update.R

clean:
	$(RM) $(PKG_NAME)_*.zip
	$(RM) $(PKG_NAME)_*.tar.gz
	$(RM) man/
