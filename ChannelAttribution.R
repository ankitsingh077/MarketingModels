#Install the libraries
# install.packages("ChannelAttribution")
# install.packages("ggplot2")
# install.packages("reshape")
# install.packages("dplyr")
# install.packages("plyr")
# install.packages("reshape2")
# install.packages("markovchain")
# install.packages("plotly")

#Load the libraries
library("ChannelAttribution")
library("ggplot2")
library("reshape")
library("dplyr")
library("plyr")
library("reshape2")
library("markovchain")
library("plotly")

#Read the data into R

channel = read.csv("Channel_attribution.csv",header = TRUE)

for (row in 1:nrow(channel))
 { 
  if (21 %in% channel[row,]){channel$convert[row]=1}
  
 }

column =colnames(channel)
channel$path = do.call(paste,c(channel[column],sep=">"))


for (row in 1:nrow(channel))
{
  channel$path[row] = strsplit(channel$path[row],">21")[[1]][1]
  
}

channel_fin = channel[,c(23,22)]
channel_fin = ddply(channel_fin,~path,summarise, conversion= sum(convert))
head(channel_fin)

data=channel_fin
head(data)

#heuristic model of attribution

h=heuristic_models(data,'path','conversion',var_value = 'conversion')
head(h)

#Markov model of attribution

m=markov_model_mp(data,'path','conversion',var_value = 'conversion',order=1)
head(m)

# merge the dataframe generates by the Markov and Heuristic model on the channel name

r= merge(h,m, by="channel_name" )

#select the relevant channels
r1 = r[, (colnames(r) %in%c('channel_name', 'first_touch_conversions', 
                             'last_touch_conversions', 'linear_touch_conversions', 
                             'total_conversion'))]

head(r1)
#change the data to long format to use it in the ggplot function
r1=melt(r1,id='channel_name')
head(r1)

# Plot the total conversions
ggplot(r1, aes(channel_name, value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('TOTAL CONVERSIONS') +
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(axis.title.y = element_text(vjust = +2)) +
  theme(title = element_text(size = 16)) +
  theme(plot.title=element_text(size = 20)) +
  ylab("")


