################################################################################
#  IBpil results - plots of performance statistics                             # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                         #
#   created:  04/04/2019                                                       #
#   modified:                                                                  #
################################################################################

# Copyright: AZTI, 2019
# Author: AZTI (<libaibarriaga@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

#wd <- "C:/use/GitHub/FLBEIA_mseIBpil/" # main directory
wd <- "D:/LWise/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil"
setwd(wd)

# directory with results
res.dir  <- file.path("./output_modHCR4/")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(ggplot2)
library(dplyr)

theme_set(theme_bw())

#==============================================================================
# Tables for the report
#==============================================================================

df <- read.table(file.path(res.dir,"stats2018.csv"), header=T, sep=";")

df$Rec <- ordered(df$Rec, c("RECmed","REClow","REClowmed","RECmix"))


# tables for the report NO ASSESSMENT

aux <- df %>% subset(Rule %in% c("HCR1","HCR2") & Ass=="ASSnone") %>%
  group_by(Rec, Rule) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000, 0),
            round(Median_B1plus[period=="short"]/1000, 0),
            round(Median_B1plus[period=="last"]/1000, 0),
            round(Median_F[period=="initial"], 3),
            round(Median_F[period=="short"], 3),
            round(Median_F[period=="last"], 3),
            round(Median_Catch[period=="initial"]/1000, 0),
            round(Median_Catch[period=="short"]/1000, 0),
            round(Median_Catch[period=="last"]/1000, 0),
            round(IAV1_Catch[period=="initial"]/1000, 0),
            round(IAV1_Catch[period=="short"]/1000, 0),
            round(IAV1_Catch[period=="last"]/1000, 0),
            round(closure[period=="initial"]*100, 0),
            round(closure[period=="short"]*100, 0),
            round(closure[period=="last"]*100, 0),
            round(P_B1plus_0.8Blim[period=="initial"]*100, 0),
            round(P_B1plus_0.8Blow[period=="initial"]*100, 0),
            round(firstyear_B1plus_0.8Blim[period=="all"], 0),
            round(firstyear_B1plus_0.8Blow[period=="all"], 0),
            round(firstyear_B1plus_Blim[period=="all"], 0),
            round(firstyear_B1plus_Blow[period=="all"], 0),
            round(max_P_B1plus_Blim[period=="last"]*100, 0),
            round(max_P_B1plus_Blow[period=="last"]*100, 0)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR1&2_ASSnone.csv"), sep=";", col.names=F)


aux <- df %>% subset(Rule %in% c("HCR3","HCR4") & Ass=="ASSnone") %>%
  group_by(Rec, Rule, Ftgt) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000, 0),
            round(Median_B1plus[period=="short"]/1000, 0),
            round(Median_B1plus[period=="last"]/1000, 0),
            round(Median_F[period=="initial"], 3),
            round(Median_F[period=="short"], 3),
            round(Median_F[period=="last"], 3),
            round(Median_Catch[period=="initial"]/1000, 0),
            round(Median_Catch[period=="short"]/1000, 0),
            round(Median_Catch[period=="last"]/1000, 0),
            round(IAV1_Catch[period=="initial"]/1000, 0),
            round(IAV1_Catch[period=="short"]/1000, 0),
            round(IAV1_Catch[period=="last"]/1000, 0),
            round(closure[period=="initial"]*100, 0),
            round(closure[period=="short"]*100, 0),
            round(closure[period=="last"]*100, 0),
            round(P_B1plus_0.8Blim[period=="initial"]*100, 0),
            round(P_B1plus_0.8Blow[period=="initial"]*100, 0),
            round(firstyear_B1plus_0.8Blim[period=="all"], 0),
            round(firstyear_B1plus_0.8Blow[period=="all"], 0),
            round(firstyear_B1plus_Blim[period=="all"], 0),
            round(firstyear_B1plus_Blow[period=="all"], 0),
            round(max_P_B1plus_Blim[period=="last"]*100, 0),
            round(max_P_B1plus_Blow[period=="last"]*100, 0)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR3&4_ASSnone.csv"), sep=";", col.names=F)

aux <- df %>% subset(Rule %in% c("HCR5","HCR6") & Ass=="ASSnone") %>%
  group_by(Rec, Rule) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000, 0),
            round(Median_B1plus[period=="short"]/1000, 0),
            round(Median_B1plus[period=="last"]/1000, 0),
            round(Median_F[period=="initial"], 3),
            round(Median_F[period=="short"], 3),
            round(Median_F[period=="last"], 3),
            round(Median_Catch[period=="initial"]/1000, 0),
            round(Median_Catch[period=="short"]/1000, 0),
            round(Median_Catch[period=="last"]/1000, 0),
            round(IAV1_Catch[period=="initial"]/1000, 0),
            round(IAV1_Catch[period=="short"]/1000, 0),
            round(IAV1_Catch[period=="last"]/1000, 0),
            round(closure[period=="initial"]*100, 0),
            round(closure[period=="short"]*100, 0),
            round(closure[period=="last"]*100, 0),
            round(P_B1plus_0.8Blim[period=="initial"]*100, 0),
            round(P_B1plus_0.8Blow[period=="initial"]*100, 0),
            round(firstyear_B1plus_0.8Blim[period=="all"], 0),
            round(firstyear_B1plus_0.8Blow[period=="all"], 0),
            round(firstyear_B1plus_Blim[period=="all"], 0),
            round(firstyear_B1plus_Blow[period=="all"], 0),
            round(max_P_B1plus_Blim[period=="last"]*100, 0),
            round(max_P_B1plus_Blow[period=="last"]*100, 0)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR5&6_ASSnone.csv"), sep=";", col.names=F)

aux <- df %>% subset(Rule %in% c("HCR7") & Ass=="ASSnone") %>%
  group_by(Rec, Rule) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000, 0),
            round(Median_B1plus[period=="short"]/1000, 0),
            round(Median_B1plus[period=="last"]/1000, 0),
            round(Median_F[period=="initial"], 3),
            round(Median_F[period=="short"], 3),
            round(Median_F[period=="last"], 3),
            round(Median_Catch[period=="initial"]/1000, 0),
            round(Median_Catch[period=="short"]/1000, 0),
            round(Median_Catch[period=="last"]/1000, 0),
            round(IAV1_Catch[period=="initial"]/1000, 0),
            round(IAV1_Catch[period=="short"]/1000, 0),
            round(IAV1_Catch[period=="last"]/1000, 0),
            round(closure[period=="initial"]*100, 0),
            round(closure[period=="short"]*100, 0),
            round(closure[period=="last"]*100, 0),
            round(P_B1plus_0.8Blim[period=="initial"]*100, 0),
            round(P_B1plus_0.8Blow[period=="initial"]*100, 0),
            round(firstyear_B1plus_0.8Blim[period=="all"], 0),
            round(firstyear_B1plus_0.8Blow[period=="all"], 0),
            round(firstyear_B1plus_Blim[period=="all"], 0),
            round(firstyear_B1plus_Blow[period=="all"], 0),
            round(max_P_B1plus_Blim[period=="last"]*100, 0),
            round(max_P_B1plus_Blow[period=="last"]*100, 0)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR7_ASSnone.csv"), sep=";", col.names=F)

# tables for the report WITH ASSESSMENT

aux <- df %>% subset(Rule %in% c("HCR1","HCR2") & Ass=="ASSss3") %>%
  group_by(Rec, Rule) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000, 0),
            round(Median_B1plus[period=="short"]/1000, 0),
            round(Median_B1plus[period=="last"]/1000, 0),
            round(Median_F[period=="initial"], 3),
            round(Median_F[period=="short"], 3),
            round(Median_F[period=="last"], 3),
            round(Median_Catch[period=="initial"]/1000, 0),
            round(Median_Catch[period=="short"]/1000, 0),
            round(Median_Catch[period=="last"]/1000, 0),
            round(IAV1_Catch[period=="initial"]/1000, 0),
            round(IAV1_Catch[period=="short"]/1000, 0),
            round(IAV1_Catch[period=="last"]/1000, 0),
            round(closure[period=="initial"]*100, 0),
            round(closure[period=="short"]*100, 0),
            round(closure[period=="last"]*100, 0),
            round(P_B1plus_0.8Blim[period=="initial"]*100, 0),
            round(P_B1plus_0.8Blow[period=="initial"]*100, 0),
            round(firstyear_B1plus_0.8Blim[period=="all"], 0),
            round(firstyear_B1plus_0.8Blow[period=="all"], 0),
            round(firstyear_B1plus_Blim[period=="all"], 0),
            round(firstyear_B1plus_Blow[period=="all"], 0),
            round(max_P_B1plus_Blim[period=="last"]*100, 0),
            round(max_P_B1plus_Blow[period=="last"]*100, 0)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR1&2_ASSss3.csv"), sep=";", col.names=F)


aux <- df %>% subset(Rule %in% c("HCR3","HCR4") & Ass=="ASSss3") %>%
  group_by(Rec, Rule, Ftgt) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000, 3),
            round(Median_B1plus[period=="short"]/1000, 3),
            round(Median_B1plus[period=="last"]/1000, 3),
            round(Median_F[period=="initial"], 3),
            round(Median_F[period=="short"], 3),
            round(Median_F[period=="last"], 3),
            round(Median_Catch[period=="initial"]/1000, 3),
            round(Median_Catch[period=="short"]/1000, 3),
            round(Median_Catch[period=="last"]/1000, 3),
            round(IAV1_Catch[period=="initial"]/1000, 3),
            round(IAV1_Catch[period=="short"]/1000, 3),
            round(IAV1_Catch[period=="last"]/1000, 3),
            round(closure[period=="initial"]*100, 0),
            round(closure[period=="short"]*100, 0),
            round(closure[period=="last"]*100, 0),
            round(P_B1plus_0.8Blim[period=="initial"]*100, 0),
            round(P_B1plus_0.8Blow[period=="initial"]*100, 0),
            round(firstyear_B1plus_0.8Blim[period=="all"], 0),
            round(firstyear_B1plus_0.8Blow[period=="all"], 0),
            round(firstyear_B1plus_Blim[period=="all"], 0),
            round(firstyear_B1plus_Blow[period=="all"], 0),
            round(max_P_B1plus_Blim[period=="last"]*100, 0),
            round(max_P_B1plus_Blow[period=="last"]*100, 3)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR3&4_ASSss3.csv"), sep=",", col.names=F)

aux <- df %>% subset(Rule %in% c("HCR5","HCR6") & Ass=="ASSss3") %>%
  group_by(Rec, Rule) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000, 0),
            round(Median_B1plus[period=="short"]/1000, 0),
            round(Median_B1plus[period=="last"]/1000, 0),
            round(Median_F[period=="initial"], 3),
            round(Median_F[period=="short"], 3),
            round(Median_F[period=="last"], 3),
            round(Median_Catch[period=="initial"]/1000, 0),
            round(Median_Catch[period=="short"]/1000, 0),
            round(Median_Catch[period=="last"]/1000, 0),
            round(IAV1_Catch[period=="initial"]/1000, 0),
            round(IAV1_Catch[period=="short"]/1000, 0),
            round(IAV1_Catch[period=="last"]/1000, 0),
            round(closure[period=="initial"]*100, 0),
            round(closure[period=="short"]*100, 0),
            round(closure[period=="last"]*100, 0),
            round(P_B1plus_0.8Blim[period=="initial"]*100, 0),
            round(P_B1plus_0.8Blow[period=="initial"]*100, 0),
            round(firstyear_B1plus_0.8Blim[period=="all"], 0),
            round(firstyear_B1plus_0.8Blow[period=="all"], 0),
            round(firstyear_B1plus_Blim[period=="all"], 0),
            round(firstyear_B1plus_Blow[period=="all"], 0),
            round(max_P_B1plus_Blim[period=="last"]*100, 0),
            round(max_P_B1plus_Blow[period=="last"]*100, 0)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR5&6_ASSss3.csv"), sep=";", col.names=F)


