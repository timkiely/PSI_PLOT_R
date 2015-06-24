


#-----------------------------------------------------------
# ------------------ DEMO_PSI_PLOT.R -----------------------
# This script demonstrates the plotting techniques which 
# can be used to create the automated population
# stability index reports
#
# For each technique, dummy data is generated and used for plotting
#
# For the live reports, the data will need to be shaped into roughly equivalent
# shape in order for the plotting to work
#
# Each plot, with the exception of the final (model tracking report)
# is stored as a Grob object, then arranged using grid.arange at the end
#
# Note: this should run end-to-end with no prior-loaded data.
# if you run it all the way through, you'll see the page 2 
# report generated. To see the page 1 report, run the grid.arrange
# function around line 153
#
# -Tim Kiely
# 6/17/2015
#
#
#-----------------------------------------------------------


#If not already installed, install the necessary packages 
if(!"gridExtra"%in%installed.packages())
  install.packages("gridExtra")
if(!"ggplot2"%in%installed.packages())
  install.packages("ggplot2")
if(!"dplyr"%in%installed.packages())
  install.packages("dplyr")
if(!"gmodels"%in%installed.packages())
  install.packages("gmodels")
if(!"plotrix"%in%installed.packages())
  install.packages("plotrix")
if(!"tidyr"%in%installed.packages())
  install.packages("tidyr")

library(gridExtra)
library(ggplot2)
library(dplyr)
library(gmodels)
library(plotrix)
library(tidyr)


# Create fake variables "Var1" "Var2" etc
Variables<-paste("Var",c(1:52))



# 1) Create test data for the transition matix

# Create dummy dataframe with the above fake variables plus Statues, "green" "yellow" "red", etc
test.data <- data.frame("Variables"=Variables
                        ,"Prev_Status"=sample(c("High","Low"),size=length(Variables),replace=T)
                        ,"Cur_Status"= sample(c("High","Low"),size=length(Variables),replace=T)
                        ,"Color_Status"=sample(c("Green","Yellow","Red"),replace=T,size=length(Variables))
                        ,stringsAsFactors = F)

# Create transition matrix, using above dummy data

# TRANSITION MATRIX
transition.matrix<-round(prop.table(table(test.data$Prev_Status,test.data$Cur_Status)),2)



# Store transition matrix as "polt.TM"

plot.TM<-
  tableGrob(transition.matrix
            ,main="Transition Matrix")


## OUTPUT STABILITY

# Two vairables here: the text, and the color

output.stability <- "0.1 Green"
colr<-"darkgreen"

plot.OS<-
  textGrob(output.stability,
          gp = gpar(col=colr))



## 1 Year AUC

# Creates fake AUC data for the line chart
test.data.auc<-data.frame("month"=seq.Date(from=as.Date("2014-01-01"), to=as.Date("2014-12-31"),by="month")
                          ,"AUC"=sample(seq(from=0,to=1,by=0.01),12))

# Store the line chart as plot.AUC
plot.AUC<-
  ggplot(data=test.data.auc,aes(x=month,y=AUC))+
  geom_point(fill="lightblue",size=3)+
  geom_line(colour="lightblue",size=1)+
  ggtitle("1 Yr AUC")+
  theme_bw()


## INPUT STABILITY (pie chart)
# First, who uses pie charts??? LOL
# Create a bar chart, then wrap the bar chart into a pie chart using coord_polar


# Group the dummary data by the color status (red, yellow, green)
a<-test.data %>% group_by(Color_Status) %>% summarise(count=n()) %>% mutate(pos=cumsum(count)-count/2)


#Store the pie chart as plot.IS
plot.IS<-
  ggplot(data=a,aes(x=factor(1),y=count,fill=Color_Status))+
  geom_bar(width=1,stat="identity")+
  geom_text(aes(x= factor(1), y=pos, label = count), size=5) + 
  coord_polar(theta="y")+
  scale_fill_manual(values=c("green","red","yellow"))+
  xlab(NULL)+
  ylab(NULL)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  ggtitle("Input Stability")



## Now we have all of our plot objects:
# plot.TM
# plot.OS
# plot.AUC
# plot.IS


# so we can arrange them in a single column using grid.arrange...

grb<-arrangeGrob(plot.TM
             ,plot.OS
             ,plot.AUC
             ,plot.IS
             ,ncol=1)


# Then, for illustration purposes, we just plot "grb" 4 times, simulating the different regions 
# grid.arrange will print the final plot. If we want to write it to a file (like a .pdf),
# we should use arrangeGrob (like above) and store as an object. Printing to .pdf is easy after that, just ask



## RUN THIS IF YOU WANT TO SEE THE MAIN MODEL TRACKING REPORT
grid.arrange(grb,grb,grb,grb,ncol=4
             
             # Add a little extra text for the main title and footnotes
             
             ,main=textGrob(paste0("\n\nModel Tracking Report - GWP Model - ",Sys.Date()),gp=gpar(fontsize=20,font=3))
             ,sub=textGrob(paste0("GREEN <= 0.1 little change [no action required]"
                                  ,"\nYELLOW 0.1 - 0.25 small change [no action required]"
                                  ,"\nRED  > 0.25 significant shift [further investigation reqd.]"
                                  ,"\n"
                                  )
                           )
             )




#-------------------------------------------------------------------

## PAGE 2: MODEL TRACKING REPORT
# For this part, my mock data isn't quite up to snuff but it gets the point across, I think
# Basically, creating a matrix of both values and corresponding colors, then mapping the values into 
# a table, and overlaying the colors


# modifying the original test.data, adding in dummy countries, IPS scores and corresponding colors
test.data$country<-sample(c("Americas","EMEA","LAC","APAC"),replace=T,size=length(Variables))
test.data$IPS<-sample(seq(from=0.0,to=0.3,by=0.02),replace=T,size=length(Variables))
test.data$IPS_Color<-ifelse(test.data$IPS<=0.1,"Green"
                            ,ifelse(test.data$IPS>0.1 & test.data$IPS<=0.25,"Yellow"
                                    ,ifelse(test.data$IPS>0.25,"Red","Red")
                                    )
                            )


# selecting out the other, non-essential dummy variables and 
# creating both a value dataset and a color datset
test.data.value <- dplyr::select(test.data,Variables,country,IPS)
test.data.color <- dplyr::select(test.data,Variables,country,IPS_Color)


# converting both to matricies
valuematrix<-as.matrix(spread(test.data.value,country,IPS))
colormatrix<-as.matrix(spread(test.data.color,country,IPS_Color))

# using color2D.matplot to make the table. Plenty of other methods 
# to do this, might be worth investigating...

color2D.matplot(matrix(as.numeric(valuematrix[,2:5]),nrow=nrow(valuematrix)) , # <----note: using the value matrix as a base
                show.values = 2,
                axes = FALSE,
                xlab = "",
                ylab = "",
                vcex = 0.5,
                vcol = "black",
                #extremes = c("red","yellow", "green")
                ,cellcolors=colormatrix[,2:5]) # <--------- note: overlaying the colors here 

# This last bit adds axis titles
axis(3, at = seq_len(ncol(colormatrix[,2:5])) - 0.5,
     labels = colnames(colormatrix)[2:5], tick = FALSE, cex.axis = 2)
axis(2, at = seq_len(nrow(colormatrix)) -0.5,
     labels = colormatrix[,1], tick = FALSE, las = 1, cex.axis = 1)

# it's a little tricker printing this method to pdf, but not that bad. we'll get there later...



