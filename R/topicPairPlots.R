setwd('~/Datasets/Battelle Data/Analyses/')
library(stringr)
getwd()
load(paste0(getwd(), "/","LDA.Rdata"))
dim(docs)
names(docs)

doctopics <- read.csv('~/Datasets/Battelle Data/Analyses/Topic models/doctopic.csv', header = FALSE, sep = '\t')
names = c('doc.id', 'year', paste0('topic.',1:200))
names(doctopics) <- names
topic.names <- names[3:length(names)]
dim(doctopics)

topics <- read.csv('~/Datasets/Battelle Data/Analyses/Topic models/topics.csv', stringsAsFactors = FALSE, header = TRUE, sep = ',')
dim(topics)
names(topics)
topics$words <- str_trim(topics$words)


par(mar = c(2,2,2,2))
topic_pair_plot <- function(i,j, pos = 1, xquantile = .95, yquantile = .95, cexMult = 40, linechars = 50, pallette = "Reds"){
    proportions1 = doctopics[[topic.names[i]]]
    proportions2 = doctopics[[topic.names[j]]]
    significance <- proportions1*proportions2
    index = order(significance, decreasing = TRUE)[pos]
    abstract = docs$text[index]
    abstract = str_extract_all(abstract, pattern = paste0('.{1,',linechars,'}[^ ]*[\\. ]'),simplify = TRUE)
    main = paste0("Topic proportions for a topic pair:")
    sub = paste0("(horizontal and vertical lines correspond to ", round(100*xquantile,1),"th percentile for each topic.)")
    main = paste0(main,'\n',sub)
    par(mar = c(4,4,3,3))
    #cols = colorRamp(brewer.pal(8,pallette),interpolate = "linear", space = "rgb")
    #pal <- choose_palette()
    cols = rep("blue",length(significance))
    cols[proportions1 > quantile(proportions1,probs = xquantile) & proportions2 > quantile(proportions2,probs = yquantile)] = "red"
    
    plot(x = proportions1,y = proportions2, xlab = topics$words[i], ylab = topics$words[j], 
         cex = cexMult*significance + .2, main = main, #col = cols(3*significance), pch = 16)
         #col = pal(round(length(pal)*4*significance)), pch = 16)
         col = cols, pch = 16)
    
    abline(v = quantile(proportions1,probs = xquantile), col = 'red')
    abline(h = quantile(proportions2,probs = yquantile), col = 'red')
    #abline(1,-1,col = 'blue')
    text(paste(abstract[1:length(abstract)], collapse = '\n'), x = proportions1[index], y = proportions2[index],
         pos = 4, offset = 3)
    arrows(x0 = proportions1[index]+.05,y0 = proportions2[index],x1 = proportions1[index]+.01,y1 = proportions2[index])
}

topic_pair_plot(180,190,2,.97,.97, cexMult = 50, linechars = 70, pallette = "Blues")
plot(x = 1:20, y = rep(1,20), pch = 1:20, col = cols((1:20)/20))

library(colorspace)
pal <- choose_palette()
pal(10)[c(1,2,2)]
