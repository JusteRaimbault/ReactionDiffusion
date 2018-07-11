
setwd(paste0(Sys.getenv('CS_HOME'),'/ReactionDiffusion/Models/Density'))

library(dplyr)
library(ggplot2)


res <- as.tbl(read.csv('explo/20180529_1027_OSE/population87871.csv'))

indics = c("moran","distance","entropy","slope")
plot(res[,indics])

res$rate = res$population / res$growthrate
params = c("diffusion","diffusionsteps","alphalocalization","growthrate","population","rate")


plot(res[,params])

# -> cf (\alpha,\beta) param space : two regimes superposing ? how precisely define a regime ?
# connected area in param / indicator space ? f(target_area) ?
#  clustering = f(target_area) ? test with density-based and distance based clusterings.

sres=res[,indics];for(j in 1:ncol(sres)){sres[,j]=(sres[,j]-min(sres[,j]))/(max(sres[,j])-min(sres[,j]))}
pca = prcomp(sres)
summary(pca)

plot(pca$x[,1:2])


