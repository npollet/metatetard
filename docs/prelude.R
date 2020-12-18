###########################################
## Folders, Themes, colors, functions    ##
###########################################
library(ggplot2)
# Setting path to directories
dir_path<-"/Users/pollet/home1/METATETARD/New_metatetard_paper/Papier_metatetard/Clean_data_scripts/"
data_dir_path <- paste0(dir_path,"data/")
output_dir_path <- paste0(dir_path,"test_Figures/")

#Personalized graphical ggplot2 theme
theme_set(theme_bw())
custom_themes<-c("theme_npgray.R","theme_nb.R")
path2custom_themes<-(paste0(data_dir_path,custom_themes))
for (my_theme in path2custom_themes) {
    source(my_theme)
}

# To keep track of color concordance
phylumcolors<-read.csv(paste0(data_dir_path,"Phylumcolors.txt"), sep=";",as.is = 2)
jcolors<-phylumcolors$Color
names(jcolors)<-phylumcolors$Phylum
head(jcolors)

# Function used for labeling plots with significance labels
# Converting a p-value triangular matrix in a rectangular matrix
# From https://fabiomarroni.wordpress.com/2017/03/25/perform-pairwise-wilcoxon-test-classify-groups-by-significance-and-plot-results/
tri.to.squ<-function(x)
{
    rn <- row.names(x)
    cn <- colnames(x)
    an <- unique(c(cn,rn))
    myval <-  x[!is.na(x)]
    mymat <-  matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
    for(ext in 1:length(cn))
    {
        for(int in 1:length(rn))
        {
            if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
            mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
            mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
        }
        
    }
    return(mymat)
}
