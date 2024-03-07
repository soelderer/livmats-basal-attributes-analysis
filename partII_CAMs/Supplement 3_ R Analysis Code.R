# ==============================================================================
# R-Code - Analysis article:
# Reuter, L; Fenn, J.; Bilo, T.; Schulz, M.; Weyland, A.; Kiesel, A.; Thomaschke, R. (2020).
# Leisure Walks Modulate the Cognitive and Affective Representation of the Corona Pandemic: Employing Cognitive Affective Maps (CAMs) to a Randomized Experimental Design. Manuscript submitted for publication.
# date of creation: November, December 2020
# authors: Julius Fenn
# ==============================================================================
############################################################################
# load packages, data
############################################################################
################
# packages
################
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("afex")
usePackage("ggpubr")
usePackage("rstatix")
usePackage("stargazer")
usePackage("plyr")
usePackage("xlsx")
usePackage("tidyverse")

################
# data
################
## set working directory by hand
# best would be to set the working directory in the following way:
# global_dir_output <- "dirpath"
setwd(global_dir_output)
## or use if R file is in same folder as data sets
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
## load files
indicators_long <- read.xlsx(file = "indicatorsCAMs_long.xlsx", sheetIndex = 1)
indicators_wide <- read.xlsx(file = "indicatorsCAMs_wide_GenAge.xlsx", sheetIndex = 1)
blocks_all_links <- read.xlsx(file = "blocks_all_links_clean.xlsx", sheetIndex = 1)




############################################################################
# Part Descriptive Analysis
############################################################################

## used concepts, links
mean(indicators_long$num_nodes); sd(indicators_long$num_nodes)
mean(indicators_long$num_edges); sd(indicators_long$num_edges)


## mean valence corona concept
mean(indicators_long$V_valence_corona.pandemie); sd(indicators_long$V_valence_corona.pandemie); hist(indicators_long$V_valence_corona.pandemie)
mean(indicators_long$mean_valence); sd(indicators_long$mean_valence); hist(indicators_long$mean_valence)


## average number of self-added concepts
`%notin%` <- Negate(`%in%`)
d = 0
vec_tmp <- c()
for(i in 1:length(unique(blocks_all_links$CAM))){
  if(any(blocks_all_links$title[blocks_all_links$CAM == blocks_all_links$CAM[i]] %notin% c("corona-pandemie", "angst", "wirtschaftliche folgen", "stress", "einsamkeit", "soziale isolierung",
                                                                         "physische isolierung", "psychische belastung", "zusammenhalt", "befinden",
                                                                         "depressivit?t"))){
    d = d + 1
    vec_tmp[d] <- sum(blocks_all_links$title[blocks_all_links$CAM == blocks_all_links$CAM[i]] %notin% c("corona-pandemie", "angst", "wirtschaftliche folgen", "stress", "einsamkeit", "soziale isolierung",
                                                                        "physische isolierung", "psychische belastung", "zusammenhalt", "befinden",
                                                                        "depressivit?t"))
  }
  blocks_all_links$title[blocks_all_links$CAM[i]]
}



d / length(unique(blocks_all_links$CAM))
mean(vec_tmp); sd(vec_tmp)




############################################################################
# Part Hypothesis Driven Tests
############################################################################
################
# V_valence_corona.pandemie
################
###
# > check assumptions
###
### Shapiro-Wilk test of normality
indicators_long %>%
  group_by(Time, Condition) %>%
  shapiro_test(V_valence_corona.pandemie)

ggqqplot(indicators_long, "V_valence_corona.pandemie", ggtheme = theme_bw()) +
  facet_grid(Time ~ Condition)

### outliers
# visual check by boxplots
bxp <- ggboxplot(
  indicators_long, x = "Time", y = "V_valence_corona.pandemie",
  color = "Condition", palette = "jco"
)
bxp

# values above Q3 + 1.5xIQR or below Q1 - 1.5xIQR are considered as outliers:
indicators_long %>%
  group_by(Time, Condition) %>%
  identify_outliers(V_valence_corona.pandemie)

### Levene's test for homogeneity of variance across groups
indicators_long %>%
  group_by(Time) %>%
  levene_test(V_valence_corona.pandemie ~ Condition)
# alternative:
# car::leveneTest(V_valence_corona.pandemie ~ Condition, data=indicators_long,
#                 center=mean)


### Box's (1949) M-test for homogeneity of covariance matrices
box_m(indicators_long[, "V_valence_corona.pandemie", drop = FALSE], indicators_long$Condition)
# alternative:
# BoxResult <- heplots::boxM(indicators_wide[ ,c("V_valence_corona.pandemie_1", "V_valence_corona.pandemie_2")],indicators_wide$Condition) # time also
# BoxResult$cov
# BoxResult


###
# > mixed anovas
###
fit1 <- afex::aov_car(V_valence_corona.pandemie ~ Time*Condition + Error(Subject / Time),
               data = indicators_long)

fit1a <- afex::aov_ez(id = "Subject", dv = "V_valence_corona.pandemie",
                      data = indicators_long, between=c("Condition"), within=c("Time"))
# partical eta squared
anova(fit1, es = "pes")
# generalized eta squared
fit1a

###
# > pairwise comparisons
###
### Pairwise comparisons between group levels at each time point
pwc <- indicators_long %>%
  group_by(Time) %>%
  pairwise_t_test(V_valence_corona.pandemie ~ Condition, p.adjust.method = "bonferroni")
pwc

### Pairwise comparisons between time points at each group levels
pwc2 <- indicators_long %>%
  group_by(Condition) %>%
  pairwise_t_test(
    V_valence_corona.pandemie ~ Time, paired = TRUE,
    p.adjust.method = "bonferroni"
  ) # %>% select(-df, -statistic, -p) # to remove details
pwc2

### t-Tests conditions
## at time point 1
tmp_ttest <- indicators_long[, c("Condition", "Time", "V_valence_corona.pandemie")]
tmp_ttest_time1 <- tmp_ttest[tmp_ttest$Time == 1,]
res.ttest <- t.test(tmp_ttest_time1$V_valence_corona.pandemie~tmp_ttest_time1$Condition)
res.ttest

## at time point 2
tmp_ttest_time2 <- tmp_ttest[tmp_ttest$Time == 2,]
res.ttest <- t.test(tmp_ttest_time2$V_valence_corona.pandemie~tmp_ttest_time2$Condition)
res.ttest


### t-Tests time
## at condition 1
tmp_ttest_cond1 <- indicators_wide[indicators_wide$Condition == 1,]
res.ttest <- t.test(tmp_ttest_cond1$V_valence_corona.pandemie_1, tmp_ttest_cond1$V_valence_corona.pandemie_2, paired = TRUE)
res.ttest
cat("mean timepoint 1:", mean(tmp_ttest_cond1$V_valence_corona.pandemie_1, na.rm = TRUE), "\n")
cat("SD timepoint 1:", sd(tmp_ttest_cond1$V_valence_corona.pandemie_1, na.rm = TRUE), "\n")
cat("mean timepoint 2:", mean(tmp_ttest_cond1$V_valence_corona.pandemie_2, na.rm = TRUE), "\n")
cat("SD timepoint 2:", sd(tmp_ttest_cond1$V_valence_corona.pandemie_2, na.rm = TRUE), "\n")


tmp <- indicators_long[indicators_long$Condition == 1, ]
tmp  %>% cohens_d(V_valence_corona.pandemie ~ Time, paired = TRUE)
# euqal to: res.ttest$statistic / sqrt(30)
# alternative: Pearsons r
# sqrt(res.ttest$statistic^2 / (res.ttest$statistic^2 + res.ttest$parameter))


## at condition 2
tmp_ttest_cond2 <- indicators_wide[indicators_wide$Condition == 2,]
res.ttest <- t.test(tmp_ttest_cond2$V_valence_corona.pandemie_1, tmp_ttest_cond2$V_valence_corona.pandemie_2, paired = TRUE)
res.ttest
cat("mean timepoint 1:", mean(tmp_ttest_cond2$V_valence_corona.pandemie_1, na.rm = TRUE), "\n")
cat("mean timepoint 2:", mean(tmp_ttest_cond2$V_valence_corona.pandemie_2, na.rm = TRUE), "\n")

cat("mean timepoint 1:", mean(tmp_ttest_cond2$V_valence_corona.pandemie_1, na.rm = TRUE), "\n")
cat("SD timepoint 1:", sd(tmp_ttest_cond2$V_valence_corona.pandemie_1, na.rm = TRUE), "\n")
cat("mean timepoint 2:", mean(tmp_ttest_cond2$V_valence_corona.pandemie_2, na.rm = TRUE), "\n")
cat("SD timepoint 2:", sd(tmp_ttest_cond2$V_valence_corona.pandemie_2, na.rm = TRUE), "\n")

# effect size: Cohens d
tmp <- indicators_long[indicators_long$Condition == 2, ]
tmp  %>% cohens_d(V_valence_corona.pandemie ~ Time, paired = TRUE)




################
# mean_valence
################
###
# > check assumptions
###
### Shapiro-Wilk test of normality
indicators_long %>%
  group_by(Time, Condition) %>%
  shapiro_test(mean_valence)

ggqqplot(indicators_long, "mean_valence", ggtheme = theme_bw()) +
  facet_grid(Time ~ Condition)

### outliers
# visual check by boxplots
bxp <- ggboxplot(
  indicators_long, x = "Time", y = "mean_valence",
  color = "Condition", palette = "jco"
)
bxp

# values above Q3 + 1.5xIQR or below Q1 - 1.5xIQR are considered as outliers:
indicators_long %>%
  group_by(Time, Condition) %>%
  identify_outliers(mean_valence)


### Levene's test for homogeneity of variance across groups
indicators_long %>%
  group_by(Time) %>%
  levene_test(mean_valence ~ Condition)


### Box's (1949) M-test for homogeneity of covariance matrices
box_m(indicators_long[, "mean_valence", drop = FALSE], indicators_long$Condition)



###
# > mixed anovas
###
fit1 <- afex::aov_car(mean_valence ~ Time*Condition + Error(Subject / Time),
                      data = indicators_long)

fit1a <- afex::aov_ez(id = "Subject", dv = "mean_valence",
                      data = indicators_long, between=c("Condition"), within=c("Time"))
# partical eta squared
anova(fit1, es = "pes")
# generalized eta squared
fit1a


###
# > pairwise comparisons
###
### Pairwise comparisons between group levels at each time point
pwc <- indicators_long %>%
  group_by(Time) %>%
  pairwise_t_test(mean_valence ~ Condition, p.adjust.method = "bonferroni")
pwc

### Pairwise comparisons between time points at each group levels
pwc2 <- indicators_long %>%
  group_by(Condition) %>%
  pairwise_t_test(
    mean_valence ~ Time, paired = TRUE,
    p.adjust.method = "bonferroni"
  ) # %>% select(-df, -statistic, -p) # to remove details
pwc2

### t-Tests conditions
## at time point 1
tmp_ttest <- indicators_long[, c("Condition", "Time", "mean_valence")]
tmp_ttest_time1 <- tmp_ttest[tmp_ttest$Time == 1,]
res.ttest <- t.test(tmp_ttest_time1$mean_valence~tmp_ttest_time1$Condition)
res.ttest

## at time point 2
tmp_ttest_time2 <- tmp_ttest[tmp_ttest$Time == 2,]
res.ttest <- t.test(tmp_ttest_time2$mean_valence~tmp_ttest_time2$Condition)
res.ttest


### t-Tests time
## at condition 1
tmp_ttest_cond1 <- indicators_wide[indicators_wide$Condition == 1,]
res.ttest <- t.test(tmp_ttest_cond1$mean_valence_1, tmp_ttest_cond1$mean_valence_2, paired = TRUE)
res.ttest
cat("mean timepoint 1:", mean(tmp_ttest_cond1$mean_valence_1, na.rm = TRUE), "\n")
cat("SD timepoint 1:", sd(tmp_ttest_cond1$mean_valence_1, na.rm = TRUE), "\n")
cat("mean timepoint 2:", mean(tmp_ttest_cond1$mean_valence_2, na.rm = TRUE), "\n")
cat("SD timepoint 2:", sd(tmp_ttest_cond1$mean_valence_2, na.rm = TRUE), "\n")

# effect size: Cohens d
tmp <- indicators_long[indicators_long$Condition == 1, ]
tmp  %>% cohens_d(mean_valence ~ Time, paired = TRUE)



## at condition 2
tmp_ttest_cond2 <- indicators_wide[indicators_wide$Condition == 2,]
res.ttest <- t.test(tmp_ttest_cond2$mean_valence_1, tmp_ttest_cond2$mean_valence_2, paired = TRUE)
res.ttest
cat("mean timepoint 1:", mean(tmp_ttest_cond2$mean_valence_1, na.rm = TRUE), "\n")
cat("SD timepoint 1:", sd(tmp_ttest_cond2$mean_valence_1, na.rm = TRUE), "\n")
cat("mean timepoint 2:", mean(tmp_ttest_cond2$mean_valence_2, na.rm = TRUE), "\n")
cat("SD timepoint 2:", sd(tmp_ttest_cond2$mean_valence_2, na.rm = TRUE), "\n")

# effect size: Cohens d
tmp <- indicators_long[indicators_long$Condition == 2, ]
tmp  %>% cohens_d(mean_valence ~ Time, paired = TRUE)




############################################################################
# Part Quantitative Exploratory Tests
############################################################################
vec_vars <- c("centralityIndex", "density", "diameter_unweighted",
              "num_nodes", "num_edges", "num_edges_solid", "num_edges_dashed",
              "transitivity")

for(i in 1:length(vec_vars)){
  tmp_anova <- na.omit(indicators_long[, c("Subject", "Time", "Condition", vec_vars[i])])
  res.aov <- rstatix::anova_test(data=tmp_anova, dv=vec_vars[i], wid = Subject,
                                 within=Time,
                                 between=Condition)

  res.aov_2 <- aov_ez(id = "Subject", dv = vec_vars[i], tmp_anova, between=c("Condition"), within=c("Time"))
  res.aov_2 <- anova(res.aov_2, es = "pes")

  mat <- matrix(data = NA, nrow = 3, ncol = 7)
  mat[,1] <- c(vec_vars[i], NA, NA)
  mat[,2] <- res.aov$Effect
  mat[,3] <- paste0(res.aov$DFn, ", ",res.aov$DFd)
  mat[,4] <- round(x = res.aov$F, digits = 2)
  mat[,5] <- paste0(round(x = res.aov$p, digits = 2), res.aov$`p<.05`)
  mat[,6] <- round(x = res.aov_2$pes, digits = 3)
  mat[,7] <- round(x = res.aov$ges, digits = 3)


  if(i == 1){
    mat2 <- mat
  }
  if(i > 1){
    mat2 <- rbind(mat2, mat)
  }
}

colnames(mat2) <- c("DV", "Effect", "df", "F", "p", "eta_p", "eta_ges")
stargazer::stargazer(mat2, summary = FALSE, type = "html", out = "anova_networkmeasures.html")



############################################################################
# Appendix refering to Qualitative exploratory analyses
############################################################################
################
# set A, B, C, D types
################
vec_type <- c()
error <- 0
for(i in 1:nrow(indicators_wide)){

  praeCAM <- blocks_all_links[blocks_all_links$CAM == indicators_wide$CAM_ID_1[i], ]
  postCAM <- blocks_all_links[blocks_all_links$CAM == indicators_wide$CAM_ID_2[i], ]

  ## to test:
  # praeCAM$title %in% postCAM$title
  # postCAM$title %in% praeCAM$title
  # length(praeCAM$title)
  # length(postCAM$title)
  # praeCAM$title
  # postCAM$title

  ## Typ A
  if(all(postCAM$title %in% praeCAM$title) & length(postCAM$title) < length(praeCAM$title)){
    vec_type[i] <- "A"
    # print(i)
    error = error + 1
  }

  ## Typ B
  if(all(praeCAM$title %in% postCAM$title) & length(postCAM$title) > length(praeCAM$title)){
    vec_type[i] <- "B"
    # print(i)
    error = error + 1
  }

  ## Typ C
  if(all(praeCAM$title %in% postCAM$title) & all(postCAM$title %in% praeCAM$title)){
    vec_type[i] <- "C"
    # print(i)
    error = error + 1
  }

  ## Typ D
  # smaller > pr? UE post, post UE pr?
  if(sum(praeCAM$title %in% postCAM$title) < length(praeCAM$title) &
     sum(postCAM$title %in% praeCAM$title) < length(postCAM$title)){
    vec_type[i] <- "D"
    # print(i)
    error = error + 1
  }

  if(error > 1){
    print("ERROR in (not exclusive logical condition)", i)
    stop("check your data and adjust this function")
  }
  error = 0
}


table(vec_type)
table(vec_type) / nrow(indicators_wide)

## append to indicators_wide
indicators_wide$Type_CAM <- vec_type

## to long format (indicators_long = tmp)
tmp <- indicators_long %>%
  select(mean_valence, num_nodes_pos, num_nodes_neg, num_nodes,
         Time, Condition, Subject, CAM_ID)
tmp$Type_CAM <- NA
for(i in 1:length(unique(tmp$Subject))){
  tmp$Type_CAM[tmp$Subject == unique(tmp$Subject)[i]] <- indicators_wide$Type_CAM[
    indicators_wide$Subject == unique(tmp$Subject)[i]]
}

head(tmp)


## keep only type D CAM
tmp <- tmp[tmp$Type_CAM == "D",] # only condition D


################
# type D choosen, set terms A, Kone, N, Ktwo
################
tmp <- tmp[rep(1:nrow(tmp), each = 2),] # duplicate each row 2 times
tmp$type_terms <- rep(c("A", "Kone", "N", "Ktwo"), time = sum(indicators_wide$Type_CAM == "D"))
tmp$mean_valence_terms <- NA
tmp$perc_pos_terms <- NA
tmp$perc_neg_terms <- NA

for(i in 1:length(unique(tmp$Subject))){

  cam_ids <- unique(tmp$CAM_ID[tmp$Subject == unique(tmp$Subject)[i]])

  if(unique(tmp$Time[tmp$CAM_ID == cam_ids[1]]) == 1){
    praeCAM <- blocks_all_links[blocks_all_links$CAM == cam_ids[1], ]
  }else if(unique(tmp$Time[tmp$CAM_ID == cam_ids[2]]) == 2){
    postCAM <- blocks_all_links[blocks_all_links$CAM == cam_ids[2], ]
  }

  ## A
  tmp_shape_A <- praeCAM$shape[!praeCAM$title %in% postCAM$title]

  ## Kone
  tmp_shape_Kone <- praeCAM$shape[praeCAM$title %in% postCAM$title]


  ## N
  tmp_shape_N <- postCAM$shape[!postCAM$title %in% praeCAM$title]

  ## Ktwo
  tmp_shape_Ktwo <- postCAM$shape[postCAM$title %in% praeCAM$title]


  for(j in 1:4){
    if(j == 1){
      tmp_shape <- tmp_shape_A
    } else if(j == 2){
      tmp_shape <- tmp_shape_Kone
    } else if(j == 3){
      tmp_shape <- tmp_shape_N
    } else if(j == 4){
      tmp_shape <- tmp_shape_Ktwo
    }

    tmp_shape_num <- ifelse(test = tmp_shape == "negative strong", -3,
                            ifelse(test =  tmp_shape == "negative", -2,
                                   ifelse(test =  tmp_shape == "negative weak", -1,
                                          ifelse(test =  tmp_shape == "neutral", 0,
                                                 ifelse(test =  tmp_shape == "ambivalent", 0,
                                                        ifelse(test =  tmp_shape == "positive weak", 1,
                                                               ifelse(test =  tmp_shape == "positive", 2,
                                                                      ifelse(test =  tmp_shape == "positive strong", 3, "ERROR"))))))))


    tmp_shape_num <- as.numeric(tmp_shape_num)
    tmp$mean_valence_terms[tmp$Subject ==  unique(tmp$Subject)[i]][j] <- mean(tmp_shape_num)


    ## % positive nodes in term X
    tmp_shape_pos <- sum(str_detect(string = tmp_shape, pattern = "positive")) / length(tmp_shape)
    ## % negative nodes in term X
    tmp_shape_neg <- sum(str_detect(string = tmp_shape, pattern = "negative")) / length(tmp_shape)

    tmp$perc_pos_terms[tmp$Subject ==  unique(tmp$Subject)[i]][j] <- tmp_shape_pos
    tmp$perc_neg_terms[tmp$Subject ==  unique(tmp$Subject)[i]][j] <- tmp_shape_neg
  }
}

tmp %>%
  select(type_terms, Time, Condition, mean_valence_terms) %>%
  group_by(type_terms, Time, Condition) %>%
  dplyr::summarize(n(), mean(mean_valence_terms), sd(mean_valence_terms))


tmp$mean_valence <- NULL

hist(tmp$mean_valence_terms); summary(tmp$mean_valence_terms)
head(tmp)
tail(tmp)

# tmp$Subject[is.na(tmp$mean_valence_terms)]
# unique(tmp$Subject)
# i <- 4
# tmp$Subject[is.na(tmp$perc_pos_terms)]


################
# analyze terms A, Kone, N, Ktwo by mixed ANOVAs - two approaches
################
###
# > use all terms, no time point:
###
fit <-aov_car(mean_valence_terms ~ Condition*type_terms + Error(Subject / type_terms),
              data = tmp)
fit

# Pairwise tmp between group levels at each time point
pwc <- tmp %>%
  group_by(type_terms) %>%
  pairwise_t_test(mean_valence_terms ~ Condition, p.adjust.method = "bonferroni")
pwc

# Pairwise comparisons between time points at each group levels
pwc2 <- tmp %>%
  group_by(Condition) %>%
  pairwise_t_test(
    mean_valence_terms ~ type_terms, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
# %>%   select(-df, -statistic, -p) # Remove details

# stargazer(pwc2, type = "html", out = "pairwise comparison.html", summary = FALSE, digits = 2)


###
# > summarize terms to constant / new, with time point:
###
tmp2 <- tmp
tmp2$type_terms <- ifelse(tmp2$type_terms == "A", yes = "N",
                          no = ifelse(test = tmp2$type_terms == "Kone", yes = "Ktwo", no = tmp2$type_terms))

tmp2$type_terms <- ifelse(tmp2$type_terms == "N", yes = "new",
                          ifelse(test = tmp2$type_terms == "Ktwo", yes = "constant", no = "ERROR"))
table(tmp2$type_terms)

fit2 <-aov_car(mean_valence_terms ~ Condition*Time*type_terms + Error(Subject / Time*type_terms),
               data = tmp2)
fit2

tmp2 %>%
  select(type_terms, mean_valence_terms, Condition, Time) %>%
  group_by(type_terms, Condition, Time) %>%
  dplyr::summarize(n = n(), mean = round(x = mean(mean_valence_terms), digits = 2),
                   SD = round(x = sd(mean_valence_terms), digits = 2))

tmp2 %>%
  select(type_terms, mean_valence_terms, Condition, Time) %>%
  group_by(type_terms) %>%
  dplyr::summarize(n = n(), mean = round(x = mean(mean_valence_terms), digits = 2),
                   SD = round(x = sd(mean_valence_terms), digits = 2))

tmp2 %>%
  select(type_terms, mean_valence_terms, Condition, Time) %>%
  group_by(Time) %>%
  dplyr::summarize(n = n(), mean = round(x = mean(mean_valence_terms), digits = 2),
                   SD = round(x = sd(mean_valence_terms), digits = 2))


## pos terms
fit2 <-aov_car(perc_pos_terms ~ Condition*Time*type_terms + Error(Subject / Time*type_terms),
               data = tmp2)
fit2

tmp2 %>%
  select(type_terms, perc_pos_terms, Condition, Time) %>%
  group_by(type_terms, Condition, Time) %>%
  dplyr::summarize(n = n(), mean = round(x = mean(perc_pos_terms, na.rm = TRUE), digits = 2),
                   SD = round(x = sd(perc_pos_terms, na.rm = TRUE), digits = 2))

tmp2 %>%
  select(type_terms, perc_pos_terms, Condition, Time) %>%
  group_by(type_terms) %>%
  dplyr::summarize(n = n(), mean = round(x = mean(perc_pos_terms, na.rm = TRUE), digits = 2),
                   SD = round(x = sd(perc_pos_terms, na.rm = TRUE), digits = 2))

tmp2 %>%
  select(type_terms, perc_pos_terms, Condition, Time) %>%
  group_by(Time) %>%
  dplyr::summarize(n = n(), mean = round(x = mean(perc_pos_terms, na.rm = TRUE), digits = 2),
                   SD = round(x = sd(perc_pos_terms, na.rm = TRUE), digits = 2))


# t-test
res.ttest <- t.test(tmp2$perc_pos_terms ~ tmp2$type_terms)
res.ttest

tmp2 %>%
  select(type_terms, perc_pos_terms, Condition, Time) %>%
  group_by(type_terms) %>%
  dplyr::summarize(n = n(), mean = round(x = mean(perc_pos_terms, na.rm = TRUE), digits = 2),
                   SD = round(x = sd(perc_pos_terms, na.rm = TRUE), digits = 2))

# effect size: Cohens d
tmp2  %>% cohens_d(perc_pos_terms ~ type_terms, paired = FALSE)


## neg terms
fit2 <-aov_car(perc_neg_terms ~ Condition*Time*type_terms + Error(Subject / Time*type_terms),
               data = tmp2)
fit2

tmp2 %>%
  select(type_terms, perc_neg_terms, Condition, Time) %>%
  group_by(type_terms, Condition, Time) %>%
  dplyr::summarize(n = n(), mean = round(x = mean(perc_neg_terms, na.rm = TRUE), digits = 2),
                   SD = round(x = sd(perc_neg_terms, na.rm = TRUE), digits = 2))

tmp2 %>%
  select(type_terms, perc_neg_terms, Condition, Time) %>%
  group_by(type_terms) %>%
  dplyr::summarize(n = n(), mean = round(x = mean(perc_neg_terms, na.rm = TRUE), digits = 2),
                   SD = round(x = sd(perc_neg_terms, na.rm = TRUE), digits = 2))

tmp2 %>%
  select(type_terms, perc_neg_terms, Condition, Time) %>%
  group_by(Time) %>%
  dplyr::summarize(n = n(), mean = round(x = mean(perc_neg_terms, na.rm = TRUE), digits = 2),
                   SD = round(x = sd(perc_neg_terms, na.rm = TRUE), digits = 2))



# t-test
res.ttest <- t.test(tmp2$perc_neg_terms ~ tmp2$type_terms)
res.ttest

tmp2 %>%
  select(type_terms, perc_neg_terms, Condition, Time) %>%
  group_by(type_terms) %>%
  dplyr::summarize(n = n(), mean = round(x = mean(perc_neg_terms, na.rm = TRUE), digits = 2),
                   SD = round(x = sd(perc_neg_terms, na.rm = TRUE), digits = 2))

# effect size: Cohens d
tmp2  %>% cohens_d(perc_neg_terms ~ type_terms, paired = FALSE)





## save pos, neg terms as APA table
vec_vars <- c("perc_pos_terms", "perc_neg_terms")

for(i in 1:length(vec_vars)){
  tmp_anova <- na.omit(tmp2[, c("Subject", "Time", "Condition", "type_terms", vec_vars[i])])
  # tmp_anova <- tmp2[, c("Subject", "Time", "Condition", "type_terms", vec_vars[i])]
  res.aov <- rstatix::anova_test(data=tmp_anova, dv=vec_vars[i], wid = Subject,
                                 within=c(Time, type_terms),
                                 between=Condition)

  res.aov_2 <- aov_ez(id = "Subject", dv = vec_vars[i], tmp_anova,
                      between=c("Condition"), within=c("Time", "type_terms"))
  res.aov_2 <- anova(res.aov_2, es = "pes")

  mat <- matrix(data = NA, nrow = length(res.aov$Effect), ncol = 7)
  mat[,1] <- c(vec_vars[i], rep(NA, times = 6))
  mat[,2] <- res.aov$Effect
  mat[,3] <- paste0(res.aov$DFn, ", ",res.aov$DFd)
  mat[,4] <- round(x = res.aov$F, digits = 2)
  mat[,5] <- paste0(round(x = res.aov$p, digits = 2), res.aov$`p<.05`)
  for(j in 1:length(res.aov$Effect)){
    mat[j,6] <- round(x = res.aov_2$pes, digits = 3)[res.aov$Effect[j] == rownames(res.aov_2)]
  }
  mat[,7] <- round(x = res.aov$ges, digits = 3)

  if(i == 1){
    mat2 <- mat
  }
  if(i > 1){
    mat2 <- rbind(mat2, mat)
  }
}

colnames(mat2) <- c("DV", "Effect", "df", "F", "p", "eta_p", "eta_ges")
stargazer::stargazer(mat2, summary = FALSE, type = "html", out = "anova_posnegterms_appendix.html")
##################


############################################################################
############################################################################
# create data set (means + SE) - three equivalent procedures for Part Hypothesis Driven Tests
############################################################################
# remark:
# other possibilites are SD, confidence intervall
################
# using plyr package
################
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE) / sqrt(length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  # data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

dfvalcor <- data_summary(indicators_long, varname="V_valence_corona.pandemie",
                    groupnames=c("Time","Condition"))
dfvalcor$Condition <- c("walking", "control", "walking", "control")
dfvalcor

dfmeanval <- data_summary(indicators_long, varname="mean_valence",
                    groupnames=c("Time","Condition"))
dfmeanval$Condition <- c("walking", "control", "walking", "control")
dfmeanval


###
# > use ggplot2 to draw barplot:
###
# adjust aesthetics of ggplot
ggplot_theme <- theme(axis.title.x = element_blank(),
                      axis.title.y = element_text(size=12),
                      axis.text.x = element_text(size=10,hjust=0.5,vjust=0.5,face="plain", colour = "black"),
                      axis.text.y = element_text(size=12,face="plain", colour = "black"),
                      panel.border = element_blank(),
                      axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank())

# create ggplot2 object for V_valence_corona.pandemie
p <- ggplot(dfvalcor, aes(x=Time, y=V_valence_corona.pandemie, fill=Condition)) +
  geom_bar(stat="identity", color="black",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=V_valence_corona.pandemie-se, ymax=V_valence_corona.pandemie+se), width=.2,
                position=position_dodge(.9)) + ggplot_theme + ylab(label = "average valence assigned to corona concept")
print(p)

# create ggplot2 object for mean_valence
p <- ggplot(dfmeanval, aes(x=Time, y=mean, fill=Condition)) +
  geom_bar(stat="identity", color="black",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) + ggplot_theme + ylab(label = "average valence CAM")
print(p)
# scale_fill_manual(values=c('#999999','#E69F00')) # adjust color manually by RGB color space



################
# using dplyr package (tidyverse)
################
df2a <- indicators_long %>%
  select(Time, Condition, mean_valence) %>%
  group_by(Time, Condition) %>%
  dplyr::summarise(mean = mean(mean_valence),
                   se = sd(mean_valence) / sqrt(length(mean_valence)))
df2a



################
# save as file and use Excel
################
tmp <- indicators_long %>%
  select(Time, Condition, Subject, V_valence_corona.pandemie, mean_valence) %>%
  arrange(Time, desc(Condition), Subject)

setwd(global_dir_output)
write.xlsx(tmp, file = "dat_MelanieBalkendiagramme.xlsx", row.names = FALSE)
rm(tmp)



############################################################################
############################################################################
############################################################################
############################################################################
# further not considered explorative analysis
############################################################################
######## explorativ t-Tests Dummy "corona-pandemic concept" (1=central)
vec_vars <- colnames(indicators_long)[c(2:21, 23:46)]
tmp_ttest <- indicators_long[, c("centralityDummy_CoronaPandemie", "Time", vec_vars)]
for(i in vec_vars){
  res.ttest <- t.test(tmp_ttest[[i]]~tmp_ttest$centralityDummy_CoronaPandemie)
  if(res.ttest$p.value < .05){
    cat("\n", i, ":\n\n")
    print(res.ttest)
  }
}

# < .1
t.test(tmp_ttest$V_valence_corona.pandemie~tmp_ttest$centralityDummy_CoronaPandemie)
tmp_ttest %>%
  select(V_valence_corona.pandemie, centralityDummy_CoronaPandemie) %>%
  group_by(centralityDummy_CoronaPandemie) %>%
  dplyr::summarise(n(), mean(V_valence_corona.pandemie))



######## explorativ t-Tests gender
## at time point 1
vec_vars <- indicators_wide %>%
  select(matches(match = "_1"), Alter) %>%
  select(c(1,2, 4:47)) %>%
  select(-centralityDummy_CoronaPandemie_1) %>%
  colnames()

tmp_ttest <- indicators_wide[, c("Geschlecht", vec_vars)]
tmp_ttest$Geschlecht <- factor(x = tmp_ttest$Geschlecht, labels = c("female", "male"))
for(i in vec_vars){
  res.ttest <- t.test(tmp_ttest[[i]]~tmp_ttest$Geschlecht)
  if(res.ttest$p.value < .05){
    cat("\n", i, ":\n\n")
    print(res.ttest)
  }
}

## at time point 2
vec_vars <- indicators_wide %>%
  select(matches(match = "_2"), Alter) %>%
  select(c(1,2, 4:47)) %>%
  select(-centralityDummy_CoronaPandemie_2) %>%
  colnames()

tmp_ttest <- indicators_wide[, c("Geschlecht", vec_vars)]
tmp_ttest$Geschlecht <- factor(x = tmp_ttest$Geschlecht, labels = c("female", "male"))
for(i in vec_vars){
  res.ttest <- t.test(tmp_ttest[[i]]~tmp_ttest$Geschlecht)
  if(res.ttest$p.value < .05){
    cat("\n", i, ":\n\n")
    print(res.ttest)
  }
}






