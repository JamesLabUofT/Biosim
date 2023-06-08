##############################################################
# An example of script for spruce budworm development stages
# @author Remi Saint-Amant - Canadian Forest Service
##############################################################
cat("\014")
rm(list=ls())
graphics.off()
Sys.setlocale("LC_ALL", "English")

library(BioSIM)

#Simulate spruce budworm phenology for the current year
today = Sys.Date()
year <- as.numeric( as.numeric(format(today, "%Y")) );

#define location
#loc <- data.frame(KeyID="Qc-01",Name="Quebec City",Latitude=46.87,Longitude=-71.25,Elevation=114) ### Quebec city
#Call BioSIM API with SBW phenology model for current year with 15 weather replications and 5 model replicates
#output <- generateWeather("Spruce_Budworm_Biology", year, year, loc$KeyID, loc$Latitude, loc$Longitude, loc$Elevation, rep = 15, repModel = 5);
locations <- read.csv("SpongyMoth_updated_Summer2023.csv")
#output <- generateWeather("Gypsy_Moth_Seasonality", year, year, loc$KeyID, loc$Latitude, loc$Longitude, rep = 2, repModel = 5);

output <- generateWeather("Gypsy_Moth_Seasonality", year, year, locations$KeyID, locations$Latitude, locations$Longitude, rep = 2, repModel = 5);

#Clsoe Java client
shutdownClient()

#simplify name
SBW = output$Spruce_Budworm_Biology;

#convert Year, Month, Day into Date
SBW$Date <- as.Date(sprintf("%d-%02d-%02d", SBW$Year,SBW$Month,SBW$Day));

#define varaible to output
variables <- c("L2","L3","L4","L5","L6","Pupae","Adults");

#compute xlim for all variables
test1 <- SBW[,variables]>0.01
test2 <- apply(test1, 1, function(x) {any(x)})
xLim = range(SBW[test2,]$Date)
SBW = SBW[SBW$Date>xLim[1]&SBW$Date<xLim[2],];
#compute ylim for all variables
yLim = range(SBW[,variables])
yLim[2] = yLim[2]*1.1


#define color
color1 <- rainbow(length(variables)+1, start=0.25, end=0.2);#Generate color
color2 <- adjustcolor(color1, 0.15);

includeTitle <- T   ### Set to true to export with title

require(Cairo)
if (includeTitle) {
  CairoSVG(file = "spruceBudwormExampleWithTitle.svg", width = 6, height = 4)
} else {
  CairoSVG(file = "spruceBudwormExample.svg", width = 6, height = 4)
}
# graph

if (includeTitle) {
  par(mfrow=c(1,1), mar=c(2.5, 2.5, 0.5, 0.5), oma = c(0.5, 2.5, 3.5, 0.5), cex=1.0, cex.main = 1.5, cex.lab=1.3, cex.axis=1.2, lend=1)
} else {
  par(mfrow=c(1,1), mar=c(2.5, 2.5, 0.5, 0.5), oma = c(0.5, 2.5, 2.5, 0.5), cex=1.0, cex.main = 1.5, cex.lab=1.3, cex.axis=1.2, lend=1)
}

#draw frame
	plot(NA, xlim=xLim, ylim=yLim, xlab="", ylab="", xaxt = "n", yaxt = "n", frame=FALSE );
	dates_pos <- seq(min(xLim), max(xLim), by="weeks");
	axis(1, dates_pos, format(dates_pos, "%b %d"));
	axis(2, at = pretty(yLim), las=1);
	mtext(side=2, text='Stage density (%)', outer=TRUE, line=0.5, cex = par()$cex.lab)
if (includeTitle) {
  mtext(side=3, text=paste0('Spruce budworm phenology at ', loc$Name,'\non date of ', today), outer=TRUE, line=0.5, cex = par()$cex.lab)
}

#add [10%-90%] variability
	for(v in c(1:length(variables)))
	{
		Lo <- aggregate(SBW[,variables[v]]~SBW$Date,FUN=function(x)quantile(x, 0.1));
		Hi <- aggregate(SBW[,variables[v]]~SBW$Date,FUN=function(x)quantile(x, 0.9));
		polygon(c(Hi[,1], rev(Lo[,1])), c(Hi[,2], rev(Lo[,2])), col=color2[v], border=NA, lwd=1, lty = 1);
	}

#Add mean
	for(v in c(1:length(variables)))
	{
		M <- aggregate(SBW[,variables[v]]~SBW$Date,FUN=mean);
		lines(M[,1],M[,2], col=color1[v],lwd=3, lty = (v-1)%%2+1);
	}

#Add Legend
legend('right', legend=variables, lty=1, lwd=20, col=color2, bty = "n", seg.len=3, cex = par()$cex.lab)
legend('right', legend=variables, lty=(c(1:length(variables))-1)%%2+1, lwd=3, col=color1, bty = "n", seg.len=3, cex = par()$cex.lab)

# Turn off device driver (to flush output to SVG)
dev.off()




