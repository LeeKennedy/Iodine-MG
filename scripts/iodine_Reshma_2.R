# Clear Environment ---------------------------------------------
rm(list=ls())

#Default library-------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)

#Data in--------------------------------------------------------
data.in <- read.csv("data/Iodine_Extract.csv", as.is=TRUE, header = TRUE)
colnames(data.in)[1] <- "SAMPLE_NUMBER"

data.in$ENTRY <- as.numeric(data.in$ENTRY)
data.in$LOGIN_DATE <- dmy_hms(data.in$LOGIN_DATE)


data.2 <- data.in %>% filter(PRODUCT == "LIQ_MILK") 



data.2 <- data.2 %>%
  filter(ENTRY <900) %>%
  #filter(CUSTOMER == "MG70"|CUSTOMER == "MG80")%>%
  filter(LOGIN_DATE > "2011-01-01")


plot.I <- ggplot(data.2, aes(x=LOGIN_DATE, y = ENTRY, fill = CUSTOMER)) +
        geom_point(size=4, shape = 21, colour = "black") +
        scale_fill_brewer(palette = "Set1") +
        labs(title = "", x="") +
        scale_y_continuous(limits = c(0,450))
plot.I

hist(data.2$ENTRY, 
     breaks=60,
     main = "LIQ_MILK Samples",
     xlab="",
     ylab="")

data.3 <- data.in %>% filter(PRODUCT == "LIQM_RAW") 

data.3 <- data.3 %>%
  filter(ENTRY <900) %>%
  filter(LOGIN_DATE > "2011-01-01")

hist(data.3$ENTRY, 
     breaks=60,
     main = "LIQM_RAW Samples",
     xlab="",
     ylab="")

data4 <- data.in %>% filter(PRODUCT == "LIQM_RAW"| PRODUCT == "LIQ_MILK")

ggplot(data4, aes(x=ENTRY, fill = PRODUCT)) + 
  geom_histogram(position = "identity", alpha = 0.6, binwidth=5, colour="black") +
  scale_fill_brewer(palette="Set1")+

  labs(x="", y="ug/100mL", title="Iodine in Liquid Milk") +
  theme_bw() +
  theme(plot.title = element_text(size=20, face="bold", vjust=1.5, lineheight=1.2)) + 
  theme(axis.text.x=element_text(angle=0, size=20, vjust=0.5)) + theme(axis.title.x = element_text(color="black", vjust=-0.35),  axis.title.y = element_text(color="black" , vjust=0.35)   ) + 
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"), text = element_text(size = 14))

