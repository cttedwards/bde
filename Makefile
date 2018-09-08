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
	Rscript -e "reg_Y000_000 <- readLines('./stan/Y000_000.stan');  save(reg_Y000_000, file = './data/reg_Y000_000.RData')"
	Rscript -e "reg_Y000_00H <- readLines('./stan/Y000_00H.stan');  save(reg_Y000_00H, file = './data/reg_Y000_00H.RData')"
	Rscript -e "reg_YA00_00H <- readLines('./stan/YA00_00H.stan');  save(reg_YA00_00H, file = './data/reg_YA00_00H.RData')"
	Rscript -e "reg_YA00_D0H <- readLines('./stan/YA00_D0H.stan');  save(reg_YA00_D0H, file = './data/reg_YA00_D0H.RData')"
	Rscript -e "reg_YA00_0IH <- readLines('./stan/YA00_0IH.stan');  save(reg_YA00_0IH, file = './data/reg_YA00_0IH.RData')"
	Rscript -e "reg_YA00_DIH <- readLines('./stan/YA00_DIH.stan');  save(reg_YA00_DIH, file = './data/reg_YA00_DIH.RData')"
	Rscript -e "reg_YA00_000 <- readLines('./stan/YA00_000.stan');  save(reg_YA00_000, file = './data/reg_YA00_000.RData')"
	Rscript -e "reg_YA00_D00 <- readLines('./stan/YA00_D00.stan');  save(reg_YA00_D00, file = './data/reg_YA00_D00.RData')"
	Rscript -e "reg_YA00_0I0 <- readLines('./stan/YA00_0I0.stan');  save(reg_YA00_0I0, file = './data/reg_YA00_0I0.RData')"
	Rscript -e "reg_YA00_DI0 <- readLines('./stan/YA00_DI0.stan');  save(reg_YA00_DI0, file = './data/reg_YA00_DI0.RData')"
	Rscript -e "reg_YAM0_00H <- readLines('./stan/YAM0_00H.stan');  save(reg_YAM0_00H, file = './data/reg_YAM0_00H.RData')"
	Rscript -e "reg_YAM0_D0H <- readLines('./stan/YAM0_D0H.stan');  save(reg_YAM0_D0H, file = './data/reg_YAM0_D0H.RData')"
	Rscript -e "reg_YAM0_0IH <- readLines('./stan/YAM0_0IH.stan');  save(reg_YAM0_0IH, file = './data/reg_YAM0_0IH.RData')"
	Rscript -e "reg_YAM0_DIH <- readLines('./stan/YAM0_DIH.stan');  save(reg_YAM0_DIH, file = './data/reg_YAM0_DIH.RData')"
	Rscript -e "reg_YAMC_00H <- readLines('./stan/YAMC_00H.stan');  save(reg_YAMC_00H, file = './data/reg_YAMC_00H.RData')"
	Rscript -e "reg_YAMC_D0H <- readLines('./stan/YAMC_D0H.stan');  save(reg_YAMC_D0H, file = './data/reg_YAMC_D0H.RData')"
	Rscript -e "reg_YAMC_0IH <- readLines('./stan/YAMC_0IH.stan');  save(reg_YAMC_0IH, file = './data/reg_YAMC_0IH.RData')"
	Rscript -e "reg_YAMC_DIH <- readLines('./stan/YAMC_DIH.stan');  save(reg_YAMC_DIH, file = './data/reg_YAMC_DIH.RData')"
	
install: $(PKG_FILES) ./inst/doc/*.html ./data/*.RData
	Rcmd build --no-build-vignettes .
	Rcmd INSTALL --html .
	
DESCRIPTION NAMESPACE: $(R_FILES)
	Rscript version_update.R

clean:
	$(RM) $(PKG_NAME)_*.zip
	$(RM) $(PKG_NAME)_*.tar.gz
	$(RM) man/
