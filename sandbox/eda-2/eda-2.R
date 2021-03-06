# this script imports the raw data described in this shared document 
# https://drive.google.com/file/d/10idMxy8eX8nTHr6wr2Q40x4XOP3Y5ck7/view
# and prepares a state of data used as a standard point of departure for any subsequent reproducible analytics

# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./manipulation/0-greeter.R",
#   output = "./manipulation/stitched-output/0-greeter.md"
# )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(mmpipe)
library(ggplot2)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
# requireNamespace("ggplot2"                 )
requireNamespace("readr"                   )
requireNamespace("tidyr"                   )
requireNamespace("dplyr"                   ) #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"                  ) #For asserting conditions meet expected patterns.

# ---- declare-globals ---------------------------------------------------------
path_input <- "./data-unshared/derived/dto-0-greeted.rds"
path_input_meta <- "./data-public/raw/meta-2019-01-15.xlsx"
# ---- load-data ---------------------------------------------------------------
ds0 <- readRDS(file = path_input)
ds_meta <- readxl::read_excel(path_input_meta)


# ---- tweak-data ------------------------------
ds <- ds0

ds %>% dplyr::glimpse(50)  

# create a categorical variable for metro area for graphing
metro_area_hot_one_vars <-  c("m01_large","m02_mid","m03_small","m04_nometro")
ds1 <- ds %>% 
  tidyr::gather("key","value",metro_area_hot_one_vars ) %>% 
  dplyr::mutate(
    m01_metro = gsub("m\\d+_(\\w+)", "\\1", key)
  ) %>% 
  dplyr::select(-key, -value) 
ds1 %>% dplyr::glimpse()
# add the newly created variable back to the dataset
ds2 <- dplyr::left_join(
  ds
  ,ds1
  ) 
ds2 %>% dplyr::glimpse()

sorted_names <- names(ds2)




# ----- local-functions --------------------
quick_save <- function(g,name,...){
  ggplot2::ggsave(
    filename = paste0(name,".png"), 
    plot     = g,
    device   = png,
    path     = "./sandbox/eda-2/prints/1/", # female marital educ poor_healt
    # width    = width,
    # height   = height,
    # units = "cm",
    # dpi      = 50,
    limitsize = FALSE,
    ...
  )
}

# ---- linear-correlations ---------
# all possible correlations among columsn

d_cor <- ds2 %>% 
  dplyr::select(-m01_metro) %>% 
  as.matrix %>% 
  cor() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("var1") %>% 
  tidyr::gather(var2,correlation, -var1) 

  
# correlation between two outcomes and each of the predictors
g1 <- d_cor %>% 
  dplyr::filter(var1 %in% c("n_services","total_payment") ) %>% 
  # dplyr::arrange(var2) %>% 
  dplyr::mutate(
    var2 = factor(var2, levels = sorted_names),
    var2 = factor(var2, levels = rev(levels(var2)))
  ) %>% 
  ggplot2::ggplot(aes(x = var2, y = correlation, fill = var1)) +
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  theme_minimal()


g1 %>% quick_save(name = "outcome-correlations",width = 600, height = 900, res = 120)

# graph

  

# ----- define-groups-of-variables ------------------
# create vectores with vector names for each group
(var_practice <- grep("p\\d+_\\w+",names(ds2), value = TRUE))
(var_market   <- grep("m\\d+_\\w+",names(ds2), value = TRUE))
(var_outcome  <- setdiff(names(ds2), c(var_practice,var_market)))
(var_metro_one_hot <- c("m02_mid", "m03_small", "m04_nometro", "m01_large"))
# ----- pairs --------------------
set.seed(42)

# create separate data sets for practice and market
d_practice <- ds2 %>%  
  dplyr::select_(.dots = c(var_outcome, var_practice ) ) %>% 
  dplyr::mutate(
    p01_female = as.factor(p01_female)
    ,p02_dpt = as.factor(p02_dpt)
  )
d_practice %>% dplyr::glimpse(50)

d_market <- ds2 %>%  
  dplyr::select_(.dots = c(var_outcome, setdiff(var_market,var_metro_one_hot )))
d_market %>% dplyr::glimpse(50)


# variables describing the practice
g2 <- d_practice %>%  
  dplyr::sample_frac(size = .01) %>% 
  GGally::ggpairs() 
g2 %>% quick_save(name = "pairs-practice",width = 1200, height = 900, res = 100)

# variables describing the market
g3 <- d_market %>%  
  dplyr::sample_frac(size = .01) %>% 
  GGally::ggpairs() 
g3 %>% quick_save(name = "pairs-market",width = 1200, height = 900, res = 100)


# facets

d_practice_long <- d_practice %>%
# d_market_long <- d_market %>%  
  dplyr::select(-p07_thera ) %>%
  dplyr::sample_frac(size = .01) %>% 
  reshape2::melt( id.vars= c(var_outcome,"p01_female","p02_dpt"))

d_practice_long %>% dplyr::glimpse()

g4 <- d_practice_long %>% 
  ggplot2::ggplot(aes_string(x = "n_services", y = "value", color = "p01_female", shape = "p02_dpt"))+
  # ggplot2::ggplot(aes_string(x = "total_payment", y = "value", color = "p01_female", shape = "p02_dpt"))+
  geom_point(alpha = .4, size = 3)+
  geom_rug(data = d_practice_long %>% dplyr::filter(is.na(value)))+
  # labs(y = "", x = st.labs["n_services"])+
  scale_color_brewer(palette="Set2")+
  facet_wrap(~variable, scales="free_y", ncol=4) +
  theme_minimal()
g4 %>% quick_save(name = "facet-practice-n_services",width = 1200, height = 900, res = 100)
# g4 %>% quick_save(name = "facet-practice-total_payment",width = 1200, height = 900, res = 100)


d_market_long <- d_market %>%
  dplyr::sample_frac(size = .01) %>% 
  dplyr::mutate(
    m01_metro = factor(m01_metro)
  ) %>% 
  reshape2::melt( id.vars= c(var_outcome,"m01_metro"))

d_market_long %>% dplyr::glimpse()

g5 <- d_market_long %>% 
  # ggplot2::ggplot(aes_string(x = "n_services", y = "value", color = "m01_metro"))+
  ggplot2::ggplot(aes_string(x = "total_payment", y = "value", color = "m01_metro"))+
  geom_point(alpha = .4, size = 3)+
  geom_rug(data = d_market_long %>% dplyr::filter(is.na(value)))+
  scale_color_brewer(palette="Set2")+
  facet_wrap(~variable, scales="free_y", ncol=4) +
  theme_minimal()
# g5 %>% quick_save(name = "facet-market-n_services",width = 1200, height = 900, res = 100)
g5 %>% quick_save(name = "facet-market-total_payment",width = 1200, height = 900, res = 100)

  
# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------------
path_report_1 <- "./sandbox/eda-1/eda-1.Rmd"
path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}



