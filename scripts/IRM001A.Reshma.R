
# Clear Environment ---------------------------------------------
rm(list=ls())

#Default library-------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)

#Data in--------------------------------------------------------
data.in <- read.csv("IRM001A.csv", as.is=TRUE, header = TRUE)
colnames(data.in)[1] <- "SAMPLE_NUMBER"

data.in$ENTRY <- as.numeric(data.in$ENTRY)
data.in$LOGIN_DATE <- dmy_hms(data.in$LOGIN_DATE)


# IRM by method -------------------------------------------------------------

data.irm <- data.in %>%
  filter(SAMPLING_POINT == "IRM001A") %>%
  filter(ENTRY <200)%>%
  filter(ENTRY>50)

plot.irm <- ggplot(data.irm, aes(x=LOGIN_DATE, y=ENTRY, fill = ANALYSIS)) +
  geom_point(size=4, shape = 21, colour = "black") +
  labs(title = "IRM001A - Iodine results by Method\n", y = "ug/100g") +
  theme(plot.title = element_text(size=18)) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0,190))
plot.irm

#---------------------Quality parameters--------------------------

irm_data <- data.irm %>%
  group_by(ANALYSIS)%>%
  summarise(n=n(), Mean = mean(ENTRY), SD = sd(ENTRY))
irm_data
write.csv(irm_data, "IRM001A_data.csv")
