# Clear Environment ---------------------------------------------
rm(list=ls())

#Default library-------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)

#Data in--------------------------------------------------------
data.in <- read.csv("All_MG.csv", as.is=TRUE, header = TRUE)
colnames(data.in)[1] <- "SAMPLE_NUMBER"

data.in$ENTRY <- as.numeric(data.in$ENTRY)
data.in$LOGIN_DATE <- dmy_hms(data.in$LOGIN_DATE)


#data.2 <- data.in %>% filter(PRODUCT == "PDR_SMP") 
data.2 <- data.in


data.2 <- data.2 %>%
  filter(ENTRY <900) %>%
  filter(CUSTOMER == "MG70"|CUSTOMER == "MG80")%>%
  filter(LOGIN_DATE > "2011-01-01")


plot.I <- ggplot(data.2, aes(x=LOGIN_DATE, y = ENTRY, fill = CUSTOMER)) +
        geom_point(size=4, shape = 21, colour = "black") +
        scale_fill_brewer(palette = "Set1") +
        labs(title = "Koroit v MGN SMP Results\n", x="") +
        scale_y_continuous(limits = c(0,450))
plot.I





