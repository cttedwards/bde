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
	Rscript -e "reg_Y000_00 <- readLines('./stan/Y000_00.stan');  save(reg_Y000_00, file = './data/reg_Y000_00.RData')"
	Rscript -e "reg_YA00_00 <- readLines('./stan/YA00_00.stan');  save(reg_YA00_00, file = './data/reg_YA00_00.RData')"
	Rscript -e "reg_YA00_D0 <- readLines('./stan/YA00_D0.stan');  save(reg_YA00_D0, file = './data/reg_YA00_D0.RData')"
	Rscript -e "reg_YA00_0I <- readLines('./stan/YA00_0I.stan');  save(reg_YA00_0I, file = './data/reg_YA00_0I.RData')"
	Rscript -e "reg_YA00_DI <- readLines('./stan/YA00_DI.stan');  save(reg_YA00_DI, file = './data/reg_YA00_DI.RData')"
	Rscript -e "reg_YAM0_00 <- readLines('./stan/YAM0_00.stan');  save(reg_YAM0_00, file = './data/reg_YAM0_00.RData')"
	Rscript -e "reg_YAM0_D0 <- readLines('./stan/YAM0_D0.stan');  save(reg_YAM0_D0, file = './data/reg_YAM0_D0.RData')"
	Rscript -e "reg_YAM0_0I <- readLines('./stan/YAM0_0I.stan');  save(reg_YAM0_0I, file = './data/reg_YAM0_0I.RData')"
	Rscript -e "reg_YAM0_DI <- readLines('./stan/YAM0_DI.stan');  save(reg_YAM0_DI, file = './data/reg_YAM0_DI.RData')"
	Rscript -e "reg_YAMC_00 <- readLines('./stan/YAMC_00.stan');  save(reg_YAMC_00, file = './data/reg_YAMC_00.RData')"
	Rscript -e "reg_YAMC_D0 <- readLines('./stan/YAMC_D0.stan');  save(reg_YAMC_D0, file = './data/reg_YAMC_D0.RData')"
	Rscript -e "reg_YAMC_0I <- readLines('./stan/YAMC_0I.stan');  save(reg_YAMC_0I, file = './data/reg_YAMC_0I.RData')"
	Rscript -e "reg_YAMC_DI <- readLines('./stan/YAMC_DI.stan');  save(reg_YAMC_DI, file = './data/reg_YAMC_DI.RData')"
	
install: $(PKG_FILES) ./inst/doc/*.html ./data/*.RData
	Rcmd build --no-build-vignettes .
	Rcmd INSTALL --html .
	
DESCRIPTION NAMESPACE: $(R_FILES)
	Rscript version_update.R

clean:
	$(RM) $(PKG_NAME)_*.zip
	$(RM) $(PKG_NAME)_*.tar.gz
	$(RM) man/
