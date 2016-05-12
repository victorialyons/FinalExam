url="scorpion_reaction_paired.csv"
scorp_data=read.csv(url,header=T)
names(scorp_data)
summary(scorp_data)
pairs(scorp_data)
attach(scorp_data)
plot(Reaction_Time_control,Reaction_Time_predator)
t.test(Reaction_Time_control,Reaction_Time_predator)
t.test(Reaction_Time_control,Reaction_Time_predator,paired=T)
mod_PtimeVsCtime=lm(Reaction_Time_predator~Reaction_Time_control)
abline(mod_PtimeVsCtime,col="red")
summary.lm(mod_PtimeVsCtime)
require(ggplot2)
?qplot
qplot(Reaction_Time_control,Reaction_Time_predator,data=scorp_data,facets=.~size,geom=c("point","smooth"),method="lm")
qplot(Reaction_Time_control,Reaction_Time_predator,data=scorp_data,facets=.~size,geom=c("point","smooth"),method="loess")
#now consider type of responses by control vs predator
x=table(Behavior_control,Behavior_predator)
mosaicplot(x)
?mosaicplot
mosaicplot(x,color=c(1:9))
#the table shows that resposes are differnt but too many categorys suppose we
#count up wet sting vs not wet sting
wetC=Behavior_control=="Dry sting"
wetP=Behavior_predator=="Dry sting"
sum(wetC)
sum(wetP)
x=table(wetP,wetC)
x
chisq.test(x)
require(tigerstats)
chisqtestGC(x,graph = T)
plot(Probes_control,Probes_predator)
xx=table(Probes_control,Probes_predator)
mosaicplot(xx)
summary(Probes_control)
summary(Probes_predator)
t.test(Probes_control,Probes_predator,paired=T)
#so groups differ in number of probs to produce a reponse
#and time it takes to get a response
#reaction time is sooner and number of probes is less to
#get a response, now factor in size difference.
url2="scorpion_reaction_rolls.csv"
scorp_data2=read.csv(url2,header=T)
names(scorp_data2)
attach(scorp_data2)
rol=table(roll,Behavior)
rol
chisqtestGC(rol)
sze=table(size,Behavior)
chisqtestGC(sze)
#both roll and size are significant in the large
#need to determine if roll conditioned on size is significant