### ************************************
### PREFACE ----
### ************************************

# ** note for KDP: PREFACE version 0.2 ** #

# configure the script
# NOTE: my machine runs MacOS; paths have not been tested on other platforms
# NOTE: this information and more is stored in section I below

setwd("~/Desktop/") # *gasp*
date_ofrun <- "(August 11, 2020 (08_11_20)" # date script was run
name_scrpt <- "UnbreakableVow.R" # script filename
name_projd <- "UnbreakableVow" # name of project directory
path_projd <- paste(sep = "", getwd(), "/", name_projd) # path to project dir
path_vault <- paste(sep = "", path_projd, "/vault") # path to vault (storage)
wksp_extsn <- paste(sep = "", name_projd, ".Rdata") # recyclable ext for outputs
info_extsn <- paste(sep = "", name_projd, ".txt") # recyclable ext for outputs
PREFACE <- c("date_ofrun","name_scrpt", "name_projd", "path_projd", 
             "path_vault", "wksp_extsn", "info_extsn")

# paths for outputs stored in the central "/vault" (ofv = output file to vault)
ofv_COSMOS_wksp <- paste(sep = "", path_vault, "/WS_COSMOS_", wksp_extsn)
I.ofv_info <- paste(sep = "", path_vault, "/info_Section_I_", info_extsn)
I.ofv_wksp <- paste(sep = "", path_vault, "/WS_Section_I_", wksp_extsn)
G.ofv_plot_graded <- paste(sep = "", path_vault, 
                           "/plot_breakdown_BIO204_SP20.pdf")
G.ofv_wksp <- paste(sep = "", path_vault, "/WS_Section_G_", wksp_extsn)
ofv_scrpt_wksp <- paste(sep = "", path_vault, "/WS_", wksp_extsn)

# save PREFACE workspace
PREFACE.lst <- c(ls(pattern = "ofv"), PREFACE, "PREFACE", "PREFACE.lst")
save(list = PREFACE.lst,
     file = paste(sep = "", path_vault, "/WS_PREFACE_", wksp_extsn))

# description of sections:
# PREFACE: configure the script
# UNIVERSAL OBJECTS ~ COSMOS: contains items used universally across sections
# SECTION I - Info: gathers path, machine, software, and package information
# SECTION G - Graded activities: piechart grade breakdown for BIO204 Spring 2020
# EPILOGUE: save the final workspace

### ************************************
### UNIVERSAL OBJECTS ~ COSMOS ----
### ************************************

# this section contains items that are universal across sections/plots/datasets

# Life Aquatic with Steve Zissou custom color palettes
ziss_blue <- c("#218ec4", "#2ca1db", "#67bbe5", "#92ceec", "#e9f5fb", "#ffffff")
ziss_navy <- c("#0f1b3e", "#192e67", "#2d52b9", "#466cd2", "#eaeffa", "#ffffff")
ziss_reds <- c("#b31300", "#f21a00", "#ff604d", "#ffbbb3", "#ffe8e5", "#ffffff")
ziss_ylow <- c("#fec72A", "#fede80", "#ffebb3", "#fff8e6", "#ffffff")
ziss_teal <- c("#285854", "#489e97", "#84c7c2", "#cae7e5", "#edf7f6", "#ffffff") 
ziss_grey <- c("#252525", "#3d3d3d", "#c4c4c4", "#d9d9d9", "#e9e9e9", "#ffffff")

# hex codes for a gradient of black/grey/white (left to right = dark to light)
greydient <- c("#000000", "#252525", "#525252",
               "#969696", "#bbbbbb", "#d9d9d9", "#e0e0e0", 
               "#ffffff")

# font family
fnt <- "Courier"

# create a vector naming all of the above (useful when saving workspaces)
cosmos <- c("ziss_blue", "ziss_navy", "ziss_reds", "ziss_ylow", "ziss_teal", 
            "ziss_grey", "greydient", "fnt")

# save universal objects workspace
COSMOS <- c(cosmos, "COSMOS")
save(list = COSMOS, file = ofv_COSMOS_wksp)

### ************************************
### I - MACHINE/PACKAGE/VERSION Info ----
### ************************************

# NOTE: section I requires objects from the PREFACE to be in the environment
# ** note for KDP: section I version 0.2 ** #

# R packages accessed via require:
require(ggplot2, quietly = T)
require(ggpubr, quietly = T)

# R packages accessed via namespace:
# benchmarkme
# dplyr

# capture R package-related information:
I.Rpac_ctg <- "R package version"
I.Rpackge_a <- data.frame(info = "benchmarkme", section = "I", 
                          category = I.Rpac_ctg, script = name_scrpt, 
                          stringsAsFactors = F)
I.Rpackge_b <- data.frame(info = "dplyr", section = "I; G", 
                          category = I.Rpac_ctg, script = name_scrpt, 
                          stringsAsFactors = F)
I.Rpackge_c <- data.frame(info = "ggplot2", section = "G", 
                          category = I.Rpac_ctg, script = name_scrpt, 
                          stringsAsFactors = F)
I.Rpackge_d <- data.frame(info = "ggpubr", section = "G", 
                          category = I.Rpac_ctg, script = name_scrpt, 
                          stringsAsFactors = F)
I.Rpackge_0 <- rbind(I.Rpackge_a, I.Rpackge_b, I.Rpackge_c, I.Rpackge_d)

## *********************************** ## *********************************** ##
## *********************************** ## *********************************** ##

# ** note for KDP: captureVersions() version 0.1 ** #
# function takes an input data.frame with:
# column 'info' listing the package name and returns a data.frame with a ...
# ... new column 'value' listing the version information for the package
# NOTE: input column names are rigid; column 'info' must list package name
captureVersions <- function(data = data.frame) {
  # internal checks to ensure correct input classes
  if (!inherits(data, "data.frame")) {
    stop("input data must be class 'data.frame'")
  }
  # store original input df and format the new df
  new_dat <- data
  new_dat$value <- ""
  for(i in 1:length(new_dat$info)) {
    # create package name variable to be tested to ensure correct inputs/outputs
    pack <- unlist(
      packageDescription(
        new_dat$info[i], fields = c("Package", "Version"))[1])
    # test if package name in input data matches 'pack' variable
    # if TRUE, add package version value to the new data.frame
    if (pack == new_dat$info[i]) {
      new_dat$value[i] <- unlist(
        packageDescription(
          new_dat$info[i], fields = c("Package", "Version"))[2])
    }
    # if FALSE, print error message
    else {
      if (!pack == new_dat$info[i]) {
        stop("'pack' variable returned by packageDescription();
       does not match row value in column 'package' of input data;
       incorrect package version valuermation likely returned")
      }
    }
  }
  return(new_dat)
}
# 
# # example usage:
# new_dataframe <- captureVersions(data = dataframe)

## *********************************** ## *********************************** ##
## *********************************** ## *********************************** ##

# captureVersions() function
I.Rpackge_1 <- captureVersions(I.Rpackge_0)
I.Rpackge <- dplyr::select(I.Rpackge_1, category, info, value, script, section)

# capture Project-related information:
I.project <- data.frame(category = "project", 
                        info = "name of project directory",
                        value = name_projd,
                        script = name_scrpt,
                        section = "all", stringsAsFactors = F)

# capture PATH-related information:
I.path_ctg <- "filepath"
I.path_sec <- "all"
I.pathsto_a <- data.frame(info = "working directory",
                          value = paste(getwd(), "/", sep = ""),
                          category = I.path_ctg, script = name_scrpt,
                          section = I.path_sec, stringsAsFactors = F)
I.pathsto_b <- data.frame(info = "path to project directory",
                          value = paste(path_projd, "/", sep = ""),
                          category = I.path_ctg, script = name_scrpt,
                          section = I.path_sec, stringsAsFactors = F)
I.pathsto_c <- data.frame(info = "path to project's central '/vault/'",
                          value = paste(path_vault, "/", sep = ""),
                          category = I.path_ctg, script = name_scrpt,
                          section = I.path_sec, stringsAsFactors = F)
I.pathsto_0 <- rbind(I.pathsto_a, I.pathsto_b, I.pathsto_c)
I.pathsto <- dplyr::select(I.pathsto_0, category, info, value, script, section)


# capture Machine-related information:
I.mach_ctg <- "machine"
I.mach_sec <- "all"
I.mach_cvr <- 1073741824 # number of bytes in 1GB (used for conversion of RAM)
I.machine_a <- data.frame(info = "OS",
                          value = sessionInfo()$running,
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_b <- data.frame(info = "processor",
                          value = benchmarkme::get_cpu()$model_name,
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_c <- data.frame(info = "number of cores",
                          value = parallel::detectCores(logical = F),
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_d <- data.frame(info = "number of threads",
                          value = parallel::detectCores(logical = T),
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_e <- data.frame(info = "RAM",
                          value = 
                            paste(
                              as.numeric(benchmarkme::get_ram()) / I.mach_cvr,
                              "GB", sep = ""),
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_0 <- rbind(I.machine_a, I.machine_b, I.machine_c, I.machine_d, 
                     I.machine_e)
I.machine <- dplyr::select(I.machine_0, category, info, value, script, section)

# capture R-related information:
I.rlan_ctg <- "base R"
I.rlan_sec <- "all"
I.baseRpl_a <- data.frame(info = "version",
                          value = R.Version()$version.string,
                          category = I.rlan_ctg, script = name_scrpt,
                          section = I.rlan_sec, stringsAsFactors = F)
I.baseRpl_b <- data.frame(info = "nickname",
                          value = R.Version()$nickname,
                          category = I.rlan_ctg, script = name_scrpt,
                          section = I.rlan_sec, stringsAsFactors = F)
I.baseRpl_c <- data.frame(info = "platform",
                          value = R.Version()$platform,
                          category = I.rlan_ctg, script = name_scrpt,
                          section = I.rlan_sec, stringsAsFactors = F)
I.baseRpl_0 <- rbind(I.baseRpl_a, I.baseRpl_b, I.baseRpl_c)
I.baseRpl <- dplyr::select(I.baseRpl_0, category, info, value, script, section)

# capture RStudio-related information:
I.RStudio <- data.frame(info = "version",
                        value = as.character(RStudio.Version()$version),
                        category = "R Studio", script = name_scrpt,
                        section = "all", stringsAsFactors = F)

# rbind all of the above data.frames together and write outputs
I.info <- rbind(I.project, I.machine, I.pathsto, I.baseRpl, I.RStudio, 
                I.Rpackge)

# outputs to the vault
write.table(sep = "\t", row.names = F, x = I.info, file = I.ofv_info)

I.obj <- ls(pattern = "I.")
I.lst <- c(I.obj[grep(pattern = "I.", x = I.obj, ignore.case = F, fixed = T)],
           PREFACE.lst, "captureVersions")
save(list = I.lst, file = I.ofv_wksp)

### ************************************
### Graded activities - Step 1: do it all ----
### ************************************

# create the data
G.acts_0 <- data.frame("num" = c(30, 100, 140, 80, 400, 100, 150),
                       "ctg" = c("C.prep", "C.read", "C.quiz", "CEPost", 
                                 "LExams", "LabRep", "LPract"),
                       stringsAsFactors = F)

# define plot parameters/aesethics
G.fil_ctg <- c(ziss_reds[2], ziss_blue[2], ziss_navy[2], ziss_ylow[1],  
               ziss_teal[2], ziss_grey[3], greydient[1])
G.lvl_ctg <- c("LExams", "LPract", "C.quiz", "C.read", 
               "LabRep", "CEPost", "C.prep")
G.acts_1 <- G.acts_0
G.acts_1$ctg <- factor(G.acts_1$ctg, levels = G.lvl_ctg)
G.pie_lab <- rep("", times = 7)
G.sze_lne <- 1.42

# create the plot
G.gpr <- ggpie(data = G.acts_1, x = "num", label = G.pie_lab, font.family = fnt,
               fill = "ctg", color = greydient[8], size = G.sze_lne) +
  scale_fill_manual(values = G.fil_ctg) +
  theme_void() +
  theme(text = element_text(family = fnt, color = greydient[1]),
        legend.position = "none")
# print(G.gpr)

### ************************************
### G - WRITE OUTPUTS ----
### ************************************

# outputs to the vault
ggsave(device = "pdf", dpi = 600, units = "mm", width = 81, height = 81,
       filename = G.ofv_plot_graded, plot = G.gpr)

G.obj <- ls(pattern = "G.")
G.lst <- c(G.obj[grep(pattern = "G.", x = G.obj, ignore.case = F, fixed = T)],
           PREFACE.lst, COSMOS)
save(list = G.lst, file = G.ofv_wksp)

### ************************************
### EPILOGUE ----
### ************************************

# output to the vault
save.image(file = ofv_scrpt_wksp)