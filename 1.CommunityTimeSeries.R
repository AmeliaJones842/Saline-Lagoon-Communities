# SALINE LAGOONS PROJECT

# AIM: VISUALLY INSPECT THE TEMPORAL CHANGES IN COMMUNITY STRUCTURE

# FIGURE 1: TIME SERIES WITH HIGH TIDES AND SPECIES COMPOSITION AND ABUNDANCE

###

rm(list=ls())
graphics.off()
dir()

###

# LOAD PACKAGES

library(vegan)

###

# DATA IMPORT AND FORMATTING

SALINE <- read.csv("Saline_lagoon2.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
SALINE <- na.omit(SALINE)
SALINE$Date <- as.Date(SALINE$Date, format = "%d/%m/%Y")

SPECAV <- aggregate(. ~ Date, data = SALINE, FUN = mean)

TIDES <- read.csv("Llanelli_tides.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
TIDES <- na.omit(TIDES)
TIDES$Date <- as.Date(TIDES$Date, format = "%d/%m/%Y")

SIZELIST <- list(SPECAV, TIDES)
SAL.TID <- Reduce(function(x,y) merge(x,y, by = "Date", all = TRUE), SIZELIST)
SAL.TID[is.na(SAL.TID)] <- 0

T.ABUN <- decostand(SAL.TID[,11:26], method = "hellinger")
SAL.TID[, 11:26] <- T.ABUN


F1 <- as.Date("2024-02-08")
F2 <- as.Date("2024-02-18")
M1 <- as.Date("2024-03-08")
M2 <- as.Date("2024-03-18")
A1 <- as.Date("2024-04-06")
A2 <- as.Date("2024-04-16")
Ma1 <- as.Date("2024-05-06")
Ma2 <- as.Date("2024-05-16")
J1 <- as.Date("2024-07-18")
J2 <- as.Date("2024-07-28")
Au1 <- as.Date("2024-08-18")
Au2 <- as.Date("2024-08-28")
S1 <- as.Date("2024-09-18")
S2 <- as.Date("2024-09-28")

FEB <- subset(SAL.TID, Date >= F1 & Date <= F2)
MARCH <- subset(SAL.TID, Date >= M1 & Date <= M2)
APRIL <- subset(SAL.TID, Date >= A1 & Date <= A2)
MAY <- subset(SAL.TID, Date >= Ma1 & Date <= Ma2)
JULY <- subset(SAL.TID, Date >= J1 & Date <= J2)
AUG <- subset(SAL.TID, Date >= Au1 & Date <= Au2)
SEPT <- subset(SAL.TID, Date >= S1 & Date <= S2)


FEBMAT <- FEB[,11:26]
FEBMAT.T <-  t(FEBMAT)
MARCHMAT <- MARCH[,11:26]
MARCHMAT.T <-  t(MARCHMAT)
APRILMAT <- APRIL[,11:26]
APRILMAT.T <-  t(APRILMAT)
MAYMAT <- MAY[,11:26]
MAYMAT.T <-  t(MAYMAT)
JULYMAT <- JULY[,11:26]
JULYMAT.T <-  t(JULYMAT)
AUGMAT <- AUG[,11:26]
AUGMAT.T <-  t(AUGMAT)
SEPTMAT <- SEPT[,11:26]
SEPTMAT.T <-  t(SEPTMAT)


###

# LABLES AND COLOURS
spec.codes <- colnames(FEBMAT)

spec.names <- c("Goby spp.", "Grey Mullet spp.", "Shrimp spp.", "Amphipod spp.", 
                "Sole", "Flatfish spp.", "European flounder", "Cranefly larvae", 
                "Three spined stickleback", "European green crab", "European eel", "European mud scud", 
                "Cockle spp", "blood worm", "polychaete spp.", "Herring")

specco <- c("#660000","#FF9900","#CCCC00","#009966","#333399","#FFCCFF","#990033","#FF0000","#FFFF33",
            "#669900","#336699","#330066","#CC99FF","#993366","#CC3300","#336600")

###

# TWO SEPERATE GRAPHS CLOSE TOGETHER

par(mfrow = c(1,7), oma = c(8,5,0,5), mar = c(2,0,1,0))

plot(FEB$Date, FEB$AM_HighTide, type = "l", col = "grey", ylim = c(0, 9), ylab = " ",
    xlab = " ", xaxt = "s", bty = "n")
lines(FEB$Date, FEB$PM._HighTide, type = "l", col = "darkgrey")
lines(FEB$Date, FEB$Floodheight, type = "l", col = "black")
par(new = TRUE)
barplot(FEBMAT.T, beside = FALSE, col = specco, ylim = c(0,2.2), ylab = "", xlab = "", 
        axes = FALSE, xaxt = "n")

mtext("AM and PM high tides (m)", side = 2, line = 3, cex = 1)
mtext("Lagoon flooding height", side = 3, line = -2.6, cex = 0.7)

plot(MARCH$Date, MARCH$AM_HighTide, type = "l", col = "grey", ylim = c(0, 9), 
     ylab = "",  xlab = " ", xaxt = "s", yaxt = "n", bty = "n")
lines(MARCH$Date, MARCH$PM._HighTide, type = "l", col = "darkgrey")
lines(MARCH$Date, MARCH$Floodheight, type = "l", col = "black")
par(new = TRUE)
barplot(MARCHMAT.T, beside = FALSE, col = specco, ylim = c(0,2.2), ylab = "", xlab = "", 
        axes = FALSE, xaxt = "n")

plot(APRIL$Date, APRIL$AM_HighTide, type = "l", col = "grey", ylim = c(0, 9), 
     ylab = "",  xlab = " ", xaxt = "s", yaxt = "n", bty = "n")
lines(APRIL$Date, APRIL$PM._HighTide, type = "l", col = "darkgrey")
lines(APRIL$Date, APRIL$Floodheight, type = "l", col = "black")
par(new = TRUE)
barplot(APRILMAT.T, beside = FALSE, col = specco, ylim = c(0,2.2), ylab = "", xlab = "", 
        axes = FALSE, xaxt = "n")

plot(MAY$Date, MAY$AM_HighTide, type = "l", col = "grey", ylim = c(0, 9), 
     ylab = "",  xlab = "Time", xaxt = "s", yaxt = "n", bty = "n")
lines(MAY$Date, MAY$PM._HighTide, type = "l", col = "darkgrey")
lines(MAY$Date, MAY$Floodheight, type = "l", col = "black")
par(new = TRUE)
barplot(MAYMAT.T, beside = FALSE, col = specco, ylim = c(0,2.2), ylab = "", xlab = "", 
        axes = FALSE, xaxt = "n")

par(xpd = NA)
legend("bottom",inset = -0.34, legend = spec.names, fill = specco, title = "Species", cex = 1.2, ncol = 4,
       x.intersp = 0.3, y.intersp = 1)

plot(JULY$Date, JULY$AM_HighTide, type = "l", col = "grey", ylim = c(0, 9), 
     ylab = "",  xlab = " ", xaxt = "s", yaxt = "n", bty = "n")
lines(JULY$Date, JULY$PM._HighTide, type = "l", col = "darkgrey")
lines(JULY$Date, JULY$Floodheight, type = "l", col = "black")
par(new = TRUE)
barplot(JULYMAT.T, beside = FALSE, col = specco, ylim = c(0,2.2), ylab = "", xlab = "", 
        axes = FALSE, xaxt = "n")

plot(AUG$Date, AUG$AM_HighTide, type = "l", col = "grey", ylim = c(0, 9), 
     ylab = "",  xlab = " ", xaxt = "s", yaxt = "n", bty = "n")
lines(AUG$Date, AUG$PM._HighTide, type = "l", col = "darkgrey")
lines(AUG$Date, AUG$Floodheight, type = "l", col = "black")
par(new = TRUE)
barplot(AUGMAT.T, beside = FALSE, col = specco, ylim = c(0,2.2), ylab = "", xlab = "", 
        axes = FALSE, xaxt = "n")

plot(SEPT$Date, SEPT$AM_HighTide, type = "l", col = "grey", ylim = c(0, 9), 
     ylab = "",  xlab = " ", xaxt = "s", yaxt = "n", bty = "n")
lines(SEPT$Date, SEPT$PM._HighTide, type = "l", col = "darkgrey")
lines(SEPT$Date, SEPT$Floodheight, type = "l", col = "black")
par(new = TRUE)
barplot(SEPTMAT.T, beside = FALSE, col = specco, ylim = c(0,2.2), ylab = "", xlab = "", 
        axes = FALSE, xaxt = "n")

axis(side = 4, las = 1)
mtext("Species abundance (Hellinger transformed)", side = 4, line = 3, cex = 1)
