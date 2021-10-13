# Replication materials
# Ruettenauer + Best (2021): Environmental Inequality and Residential Sorting in Germany
# Create descriptive tables and figures


rm(list = ls())

library(plyr)
library(dplyr)
library(sf)
library(ggplot2)
library(scales)
library(tmap)
library(texreg)
library(stargazer)
library(xtable)

# library(extrafont)
# loadfonts()



setwd("../02_Data")


### Load ###

load("EPER_INKAR_2017.RData")
load("Gemeindeverbaende_Spatial_2017.RData")
load("Eper_sp_v16.RData")
load("EPER_INKAR_2017_reg.RData")


# Read federal states shape
bld.shape <- st_read(dsn = "./vg1000-ew_2017-12-31.utm32s.shape.ebenen/vg1000-ew_ebenen", 
                     "VG1000_LAN")
bld.shape <- bld.shape[bld.shape$EWZ > 0, ]
bld.shape <- st_transform(bld.shape, st_crs(gemverb.spdf))



######################
#### Plot Figure1 #### 
######################

### Simplify gemverb borders (to reduce resolution and file size)
eper_inkar_final.spdf <- st_simplify(eper_inkar_final.spdf, dTolerance = 200)
bld.shape <- st_simplify(bld.shape, dTolerance = 500)


### Define colours
col1 <- c("#FBF6E7",
          "#D6C39D",
          "#B59E77",
          "#957A51",
          "#74552B",
          "#543005")
# col1 <- rev(col1)
col2 <- c("#f5f5f5",
          "#c7eae5",
          "#80cdc1",
          "#35978f",
          "#01665e",
          "#003c30")

years <- c(2015)

for(i in years){
  tmp.spdf <- eper_inkar_final.spdf[eper_inkar_final.spdf$reportingyear == i, ]
  cuts <- classInt::classIntervals(tmp.spdf$number_ind_perc, style = "kmeans", n = 6, largeN = 5000)
  brks <- cuts$brks
  brks[1] <- min(tmp.spdf$number_ind_perc); brks[length(brks)] <- max(tmp.spdf$number_ind_perc)
  plot1 <- tm_shape(tmp.spdf) +  #print
    tm_fill(col = "number_ind_perc", style = "fixed", # Variable
            title = "Number of \nindustrial facilities",
            breaks = brks,
            n = 6,
            midpoint = NA,
            palette = col1,
            legend.format = list(scientific = TRUE, format = "f")) +
    tm_borders(col = alpha("grey60", 0.3), lwd = 0.1)+
    tm_layout(legend.outside = T, legend.outside.position = "right",
              main.title = "Industrial facilities", main.title.position = "center",
              #legend.width = 0.5, legend.height = 0.9, legend.hist.height = 0.3,
              legend.title.size = 1.2, 
              legend.text.size = 1, legend.hist.size = 1,
              legend.bg.color = "white",
              legend.bg.alpha = 1,
              main.title.fontface = "bold",
              frame = FALSE)+
    tm_shape(bld.shape) +
    tm_borders(col = "grey30", lwd = 0.8 )
  plot1
  
  cuts <- classInt::classIntervals(tmp.spdf$einkommensteuer, style = "quantile", n = 6, largeN = 5000)
  brks <- cuts$brks
  brks[1] <- min(tmp.spdf$einkommensteuer); brks[length(brks)] <- max(tmp.spdf$einkommensteuer)
  plot2 <- tm_shape(tmp.spdf) +  #print
    tm_fill(col = "einkommensteuer", style = "fixed", # Variable
            title = "Income tax revenue \nper capita",
            breaks = brks,
            n = 6,
            midpoint = NA,
            palette = col2,
            legend.format = list(scientific = TRUE, format = "f")) +
    tm_borders(col = alpha("grey60", 0.3), lwd = 0.1)+
    tm_layout(legend.outside = T, legend.outside.position = "right",
              main.title = "Income tax revenue", main.title.position = "center",
              #legend.width = 0.5, legend.height = 0.9, legend.hist.height = 0.3,
              legend.title.size = 1.2, 
              legend.text.size = 1, legend.hist.size = 1,
              legend.bg.color = "white",
              legend.bg.alpha = 1,
              main.title.fontface = "bold",
              frame = FALSE)+
    tm_shape(bld.shape) +
    tm_borders(col = "grey30", lwd = 0.8 )
  plot2
  
  name = paste("Figure1_", i, ".pdf", sep="")
  cairo_pdf(file=paste("../03_Output/", name, sep=""), width = 14, height = 7, bg = "white", 
            # family = "CM Roman"
            )
  print(tmap_arrange(plot1, plot2, ncol = 2))
  dev.off() 
  
}







###########################
#### Descriptive table #### 
###########################


# Summarize command (wide format) with within variance option
# df=Dataframe, list=list of variables to calculate stat for (string or vector object)
# group=grouping vector (as vector incl dataset if necess.) round=numbers of decimals
# id=Name of id variable (as string) for within sd computation, 
# within=within standard deviation if TRUE
# wide= SD in extra columns if TRUE


summarize.wide <-
  function(df,
           list,
           group,
           round = 6,
           total = TRUE,
           na.rm = TRUE,
           id = NULL,
           within = FALSE,
           wide = FALSE) {
    #Prepare DF
    summary.df <-
      data.frame(matrix(
        data = NA,
        ncol = length(unique(group[!is.na(group)])) + 1,
        nrow = 2 * length(list) + 1
      ))
    if (grepl("\\$", deparse(substitute(group)))) {
      name.group <- unlist(strsplit(deparse(substitute(group)), "\\$"))[2]
    } else {
      name.group <- deparse(substitute(group))
    }
    colnames(summary.df)[1] <- name.group
    
    if (is.numeric(group) | is.character(group)) {
      group <- as.factor(group)
      groups <- as.character(levels(group))
    }
    if (is.factor(group)) {
      groups <- as.character(levels(group))
    }
    
    colnames(summary.df)[2:ncol(summary.df)] <- groups
    summary.df[seq(2, nrow(summary.df), 2), 1] <- ""
    
    if (any(rowSums(is.na(df[, which(colnames(df) %in% list)])) > 0)) {
      warning("All obersvations containing at least one missing value excluded",
              call. = TRUE)
    }
    
    nobs <-
      as.vector(table(group[which(rowSums(is.na(df[, which(colnames(df) %in% list)])) ==
                                    0)]))
    summary.df[2 * length(list) + 1, 2:(length(unique(group)) + 1)] <-
      t(nobs)
    summary.df[2 * length(list) + 1, 1] <- "N.Obs."
    
    
    #Calculating stats (mean und sd per group)
    
    j = 1
    for (i in list) {
      tmp <-
        by(df[, which(colnames(df) %in% i)], group, function(x)
          mean(x, na.rm = na.rm))
      summary.df[j, 1] <- i
      summary.df[j, 2:ncol(summary.df)] <-
        t(round(as.vector(unname(tmp)), round))
      
      #Within variance
      if (within) {
        m <- 1
        for (h in levels(group)) {
          t <-
            by(df[group == h, which(colnames(df) %in% i)], df[group == h, which(colnames(df) %in% id)],
               function(x)
                 sd(x, na.rm = na.rm))
          if (m == 1) {
            tmp <- mean(t, na.rm = na.rm)
          } else{
            tmp <- c(tmp, mean(t, na.rm = na.rm))
          }
          m <- m + 1
        }
      } else{
        # Overall Variance
        tmp <-
          by(df[, which(colnames(df) %in% i)], group, function(x)
            sd(x, na.rm = na.rm))
      }
      
      
      tmp <- paste("(", round(as.vector(unname(tmp)), round), ")", sep = "")
      summary.df[j + 1, 2:ncol(summary.df)] <- t(tmp)
      
      j <- j + 2
    }
    
    # Total sample stat
    if (total) {
      total <- matrix(data = character(),
                      nrow = 2 * length(list) + 1,
                      ncol = 1)
      colnames(total)[1] <- "Total"
      
      help <- rep(1, length(group))
      
      j = 1
      for (i in list) {
        tmp <-
          by(df[, which(colnames(df) %in% i)], help, function(x)
            mean(x, na.rm = na.rm))
        total[j, 1] <- round(as.vector(unname(tmp)), round)
        
        #Within variance
        if (within) {
          tmp <-
            mean(by(df[, which(colnames(df) %in% i)], df[, which(colnames(df) %in% id)], function(x)
              sd(x, na.rm = na.rm)), na.rm = na.rm)
        } else{
          # Overall Variance
          tmp <-
            by(df[, which(colnames(df) %in% i)], help, function(x)
              sd(x, na.rm = na.rm))
        }
        total[j + 1, 1] <-
          paste("(", round(as.vector(unname(tmp)), round), ")", sep = "")
        
        j <- j + 2
      }
      summary.df <- cbind(summary.df, total)
      summary.df$Total <- as.character(summary.df$Total)
      summary.df[nrow(summary.df), ncol(summary.df)] <-
        sum(as.numeric(summary.df[nrow(summary.df), 2:ncol(summary.df)]), na.rm =
              TRUE)
    }
    
    if (wide) {
      tmp <-
        as.matrix(apply(summary.df[2:ncol(summary.df)], 2, function(x)
          x[seq(2, length(x), 2)]))
      colnames(tmp) <- rep("SD", ncol(tmp))
      tmp <- rbind(tmp, rep(" ", ncol(tmp)))
      
      tmp2 <- summary.df[seq(1, nrow(summary.df), 2), ]
      
      summary.df <-
        matrix(NA,
               nrow = nrow(tmp2),
               ncol = ncol(tmp2) + ncol(tmp))
      
      summary.df[, c(1, seq(2, ncol(summary.df), 2))] <- as.matrix(tmp2)
      summary.df[, c(seq(3, ncol(summary.df), 2))] <- as.matrix(tmp)
      
      summary.df <- data.frame(summary.df)
      
      colnames(summary.df)[c(1, seq(2, ncol(summary.df), 2))] <-
        colnames(tmp2)
      colnames(summary.df)[c(seq(3, ncol(summary.df), 2))] <-
        colnames(tmp)
    }
    
    
    return(summary.df)
  }





### Table Descirptives ###

vars <- c(
  "number_ind_perc",
  "rank",
  "einkommensteuer",
  "gewerbesteuer",
  "anteilarbeitsloseauslnder",
  "einwohner65jahreundlter",
  "einwohner18juj",
  "popdens"
)



desc.df <-
  summarize.wide(
    data.frame(eper_inkar_final.df[eper_inkar_final.df$reportingyear > 2007, ]),
    vars,
    eper_inkar_final.df$east,
    round = 2,
    within = FALSE,
    wide = TRUE
  )

replace <- list(
  "number_ind_perc" = "Number industrial facilities",
  "rank" = "Relative rank",
  "einkommensteuer" = "Income tax revenue",
  "gewerbesteuer" = "Trade tax revenue",
  "anteilarbeitsloseauslnder" = "Percent foreigners (proxy)",
  "einwohner65jahreundlter" = "Percent 65 and older",
  "einwohner18juj" = "Percent 18 and younger",
  "popdens"  = "Population density"
)

for (i in names(replace)) {
  desc.df[, 1] <- sub(paste(i), paste(replace[i]), desc.df[, 1])
}

# Write table
file = "../03_Output/TableA1.tex"

cat("\\begin{table}[ht!]\n", file = file)
cat("\\centering\n", file = file, append = T)
cat("\\footnotesize\n", file = file, append = T)
cat("{\\begin{threeparttable}\n",
    file = file,
    append = T)

cat(
  paste0("\\caption{", "Summary statistics", "}\n"),
  file = file,
  append = T
)
cat(paste0("\\label{", "tab:desc", "}\n"),
    file = file,
    append = T)
cat(
  paste0(
    "\\begin{tabular}{l D{.}{.}{3.4} D{.}{.}{3.4} D{.}{.}{3.4} D{.}{.}{3.4} D{.}{.}{3.4} D{.}{.}{3.4} }\n"
  ),
  file = file,
  append = T
)

cat("\\hline \n", file = file, append = T)
cat(
  " & \\multicolumn{2}{c}{West} & \\multicolumn{2}{c}{East} & \\multicolumn{2}{c}{Overall} \\\\ \n",
  file = file,
  append = T
)
cat(
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \n",
  file = file,
  append = T
)
cat(
  " & \\tc{Mean} & \\tc{SD} & \\tc{Mean} & \\tc{SD} & \\tc{Mean} & \\tc{SD} \\\\ \n",
  file = file,
  append = T
)
cat("\\hline \n", file = file, append = T)

default_args  =  list(
  include.colnames = FALSE,
  only.contents = TRUE,
  include.rownames = FALSE,
  hline.after = NULL,
  comment = FALSE,
  print.results = FALSE,
  sanitize.text.function  =  function(z)
    z
)
calling_args2  =  c(list(x = xtable(desc.df[-nrow(desc.df), ], digits = 2)),
                    c(default_args))
cat(do.call(print.xtable, calling_args2),
    file = file,
    append = T)

cat("\\hline \n", file = file, append = T)
desc.df <- apply(
  desc.df,
  2,
  FUN  =  function(x)
    as.character(x)
)
ll <- paste(desc.df[nrow(desc.df), ])
ll[-1] <- paste0("\\tc{", ll[-1], "}")
ll <- paste0(paste0(ll, collapse  =  " & "), "\\\\ \n")
cat(ll, file = file, append = T)
cat("\\hline \n", file = file, append = T)

cat("\\end{tabular}\n", file = file, append = T)
cat("\\end{threeparttable}}\n",
    file = file,
    append = T)
cat("\\end{table}\n", file = file, append = T)
