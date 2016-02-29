# Clear Environment ---------------------------------------------
rm(list=ls())

#Default library-------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)

#Other packages-------------------------------------------------

#Data in--------------------------------------------------------
data.in <- read.csv("data/Iodine_2015_on.csv", as.is=TRUE, header = TRUE)
colnames(data.in)[1] <- "SAMPLE_NUMBER"

data.in$ENTRY <- as.numeric(data.in$ENTRY)
data.2 <- data.in %>%
        filter(PRODUCT == "PDR_FCMP") 

data.2 <- data.2 %>%
  filter(ENTRY <900)

boxplot (ENTRY~CUSTOMER, 
         data = data.2, 
         na.rm = TRUE,
         las = 2,
         main = "Infant Formula - Iodine by client")

plot.I <- ggplot(data.2, aes(x=SAMPLE_NUMBER, y = ENTRY, fill = ANALYSIS)) +
        geom_point(size=4, shape = 21, colour = "black") +
        scale_fill_brewer(palette = "Set1") +
        labs(title = "All SMP, by method\n")
plot.I

# Results by MG location ----------------------------------------------------
data.3 <- data.2 %>%
        filter(grepl("MG", CUSTOMER))

plot.I2 <- ggplot(data.3, aes(x=SAMPLE_NUMBER, y=ENTRY, fill = CUSTOMER)) +
        geom_point(size=4, shape = 21, colour = "black") +
        labs(title = "FCMP Iodine results by MG location\n", y = "ug/100g") +
        theme(plot.title = element_text(size=18)) +
        scale_fill_brewer(palette = "Set1")
plot.I2

# Results by all customers ----------------------------------------------------
plot.I2a <- ggplot(data.2, aes(x=SAMPLE_NUMBER, y=ENTRY, fill = CUSTOMER)) +
        geom_point(size=4, shape = 21, colour = "black")+
        labs(title = "All SMP Iodine results by Customer\n", y = "ug/100g") +
        theme(plot.title = element_text(size=22))
plot.I2a

# Results by method ----------------------------------------------------
plot.I2b <- ggplot(data.2, aes(x=SAMPLE_NUMBER, y=ENTRY, fill = ANALYSIS)) +
        geom_point(size=4, shape = 21, colour = "black") +
        labs(title = "SMP Iodine results by Method\n", y = "ug/100g") +
        theme(plot.title = element_text(size=22))
plot.I2b


# MG Results by method ----------------------------------------------------
plot.I4 <- ggplot(data.3, aes(x=SAMPLE_NUMBER, y=ENTRY, fill = ANALYSIS)) +
        geom_point(size=4, shape = 21, colour = "black", alpha = 0.8)+
        scale_fill_brewer(palette = "Set1") +
        labs(title = "MG Iodine results by Method\n", y = "ug/100g") +
        theme(plot.title = element_text(size=22))
plot.I4


# Boxplot - MG results, filtered by sample number
data.5 <- data.in %>%
        #filter(SAMPLE_NUMBER > 8000000)%>%
        filter(ENTRY < 900) %>%
        filter(grepl("MG", CUSTOMER))

boxplot (ENTRY~CUSTOMER, 
         data = data.5, 
         na.rm = TRUE,
         las = 2)

# IRM by method -------------------------------------------------------------

data.irm <- data.in %>%
  filter(SAMPLING_POINT == "IRM001A") %>%
  filter(ENTRY <200)

plot.irm <- ggplot(data.irm, aes(x=SAMPLE_NUMBER, y=ENTRY, fill = ANALYSIS)) +
  geom_point(size=4, shape = 21, colour = "black") +
  labs(title = "IRM001A - Iodine results by Method\n", y = "ug/100g") +
  theme(plot.title = element_text(size=18)) +
  scale_fill_brewer(palette = "Set1")
plot.irm

# All MG results by location -------------------------------------------------

data.6 <- data.in %>%
  filter(grepl("MG", CUSTOMER)) %>%
  filter(ENTRY < 800)

plot.all_mg <- ggplot(data.6, aes(x=SAMPLE_NUMBER, y=ENTRY, fill = CUSTOMER)) +
  geom_point(size=4, shape = 21, colour = "black") +
  labs(title = "All Iodine results by MG location\n", y = "ug/100g") +
  theme(plot.title = element_text(size=18)) 
  #scale_fill_brewer(palette = "Set3")
plot.all_mg
