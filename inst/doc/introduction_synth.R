## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages, eval=T, echo=T, message=F, warning = FALSE---------------------
## Data prep:
library(tidyverse) # version: tidyverse_2.0.0 
library(dplyr) #version: dplyr_1.1.4
library(stats) # version: stats4
library(fastDummies) # version: fastDummies_1.7.3
library(reshape2) #version: reshape2_1.4.4

## IRT-M estimation:
#devtools::install_github("dasiegel/IRT-M")
library(IRTM) #version 1.00

## Results visualization: 
library(ggplot2)  # version: ggplot2_3.4.4
library(ggridges) #version: ggridges_0.5.6 
library(RColorBrewer) #version: RColorBrewer_1.1-3
library(ggrepel) # version: ggrepel_0.9.5 

## ----load-dat, eval=T, echo=T, message=F, warning = FALSE---------------------

synth_questions <- NULL  # Initialize to avoid R CMD check notes

load("./vdata/synth_questions.rda")
## Convert numeric ordinal responses to factors

ebdatsub <- lapply(ebdatsynth[,], factor) ## that's a list now

## converts the list back into a dataframe:
Y <- dummy_cols(.data=ebdatsub,
                       remove_selected_columns=TRUE)

## remove the .data that dummy_cols adds to the column names
colnames(Y) <- gsub(".data.", '', colnames(Y))

## remove the data objects:
rm(ebdatsub)
rm(ebdatsynth)

## ----load-mcodes, eval=T, echo=T, message=F, warning = F----------------------
load('./vdata/mcodes.rda')

## Only keep M-Codes with loadings or outcomes:
MCodes$encoding <- rowSums(abs(MCodes[,4:9])) 
MCodes <- MCodes[which(MCodes$encoding > 0),]


## ----format-mcodes, eval=T, echo=T, message=F, warning = F--------------------
## Produce a K-coded questions x R-responses data frame:

d <- 6 #number of coded dimensions
mcolumns <- c("QMap", "D1-Culture threat", 
              "D2-ReligionThreat", 
              "D3-Economic Threat",
              "D4-HealthThreat",
              "O1-OutcomeSupportImmigration", "O2-OutcomeSupportEU")

combine <- MCodes[,mcolumns] %>% ## question codes and loadings 
    inner_join(
        Y %>% 
        t() %>% 
        as.data.frame(stringsAsFactors = FALSE) %>% 
        type_convert() %>%
        rownames_to_column(var = "question"),
        by = c("QMap" = "question"  )
    )

M_matrix <- as.data.frame(combine[, 1:(d+1)])

#Reverse the earlier transposition of the observations:
Y_in <- combine[, (d+2):ncol(combine)]%>%
    t() %>%
    as.data.frame()

Y_in <- as.data.frame(sapply(Y_in, as.numeric))

## Take the question names and 
## convert to column names

question <- combine[,1] %>%
    as.data.frame() 
colnames(Y_in) <- question[,1]
rm(combine)
rm(question)

## ----run-irtm, eval=T, echo=T, message=F, warning = FALSE---------------------
d=6
irt <- irt_m(Y_in = Y_in, d = d, M_matrix = M_matrix, nsamp = 1000, nburn=20, thin=1)


## ----theta-average, eval=T, echo=T, message=F, warning = FALSE----------------
avgthetas <- theta_av(irt$theta)


## ----read-idvs, eval=T, echo=T------------------------------------------------

## load idvs:
load("./vdata/synth_idvs.rda")

thetas <- cbind(avgthetas, synthidvs)

## Rename columns for readability:
colnames(thetas)[1:6] <- paste0("Theta", 1:6)
colnames(thetas)[colnames(thetas)=="qb7_2"] <- "MoreBorderControl"

## Cast into factors:
thetas$mediatrust <- as.factor(thetas$mediatrust)
thetas$class <- as.factor(thetas$class)
thetas$polorient <- as.factor(thetas$polorient)

head(thetas)


## ----corr-matrixes, eval=T, echo=T, message=F, warning = FALSE----------------

#Compute correlation matrix of latent dimensions
theta_names <- c("Culture Threat", "Religion Threat", "Economic Threat", "Health Threat", "Support Immigration", "Support EU")
theta_corr <- dim_corr(irt$Sigma, theta_names)
theta_corr


## ----viz-thetas, eval=T, echo=T, echo=T, message=F, warning = FALSE-----------
library(ggplot2) #version: ggplot2_3.4.4 
library(ggridges) #version: ggridges_0.5.6 
library(RColorBrewer) #version: RColorBrewer_1.1-3
library(dplyr) #version: dplyr_1.1.4
library(ggrepel) # version: ggrepel_0.9.5 
library(reshape2) #version: reshape2_1.4.4


## Rename for interpretability:
## Mapping:
## Theta1-Culture threat
## Theta2-ReligionThreat
## Theta3-Economic Threat
## Theta4-HealthThreat
## Theta5-OutcomeSupportImmigration
## Theta6-OutcomeSupportEU

colnames(thetas)[1:6] <-  recode(colnames(thetas)[1:6],
                                 "Theta1" = "Culture Threat",
                                 "Theta2" = "Religion Threat",
                                 "Theta3" = "Economic Threat",
                                 "Theta4" = "Health Threat",
                                 "Theta5" = "Support Immigration",
                                 "Theta6" = "Support EU")

#Save aggregate plot
ggbase <- irt_vis(d = d, T_out = thetas, sub_name = NULL, out_file = "ebirtm-synth.png")


## ----medtrust-viz, eval=T, echo=T, message=F, warning = FALSE-----------------

#Save plot subset by media trust
ggmt <- irt_vis(d = d, T_out = thetas, sub_name = "mediatrust", out_file = "theta-media-synth.png")


## ----lambdas, eval=TRUE, echo=T, message=F, warning = FALSE-------------------

#Extract relevant substantive notes and create data frame with them and item codes
filtered_MCodes <- MCodes[MCodes[[2]] %in% M_matrix$QMap, , drop = FALSE]
M_df <- data.frame(QMap = M_matrix$QMap, sn = filtered_MCodes[[3]])

#Explore item loadings
lambdas <- get_lambdas(irt$lambda, item_names = M_df$QMap, dim_names = theta_names, item_elab = M_df$sn)
average_lambdas <- lambdas[[1]]
highest_lambdas <- lambdas[[2]]
average_lambdas
highest_lambdas


