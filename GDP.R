# ------------------------------------------------------------------------
# Import
library(quantmod)
library(reshape2)
library(stringr)
library(directlabels)
library(factoextra)
library(gridExtra)

library(ggplot2)
theme <- theme_grey()
theme_set(theme)

# ------------------------------------------------------------------------
# Define functions
setFred <- function(){
    setDefaults(getSymbols, src = 'FRED')
}

getLast <- function(string){
    str_sub(string, start = -3)
}

removeNA <- function(dataframe){
    dataframe <- dataframe[complete.cases(dataframe), ]
}

# ------------------------------------------------------------------------
# Parameters
countries = c(
    'ARG', 'BRA', 'CHL', 'COL',
    'PER', 'URY', 'ECU', 'BOL',
    'PRY', 'VEN'
)

# ------------------------------------------------------------------------
# Start
l <- c(1:length(countries))

e <- new.env()

setFred()

# Run
for(i in 1:length(countries)){
    # Get country list
    l[i] <- paste('NYGDPPCAPKD', countries[i], sep = "")
    # Download data
    getSymbols(l[i], env = e)
}

# Merge all xts in environment
t <- do.call(merge, as.list(e))

# Make df
Date <- index(t)
df <- data.frame(t)

colnames(df) <- getLast(colnames(df))

df$Date <- Date

df <- removeNA(df)

#Melt
df_melt <- melt(df, id="Date")

df <- subset(df, select = -c(Date))

# ------------------------------------------------------------------------
# Plot
GDP <- ggplot(data = df_melt, aes(x = Date, y = value, colour = variable)) +
    geom_line() +
    ylim(0, NA) +
    ggtitle('Per capita GDP (in 2010 $)') +
    xlab(NULL) + ylab(NULL) + 
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_dl(
        aes(label = variable),
        method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)
    ) + 
    theme(legend.position = "none")

# ------------------------------------------------------------------------
# df_growth
df_growth <- df

for(i in 1:length(countries)){
    df_growth[i] <- Delt(df[i])
}

df_growth <- removeNA(df_growth)

# Principal components
P <- prcomp(df_growth)

pr <- fviz_eig(
    P,
    ncp = length(df),
    ylim=c(0, 100),
    title = "Proportion of variance in principal component",
    ylab = "", xlab = "",
    ggtheme = theme,
)

arrows <- fviz_pca_var(
    P,
    geom = "text",
    col.circle = "grey70",
    title = "Plot of the first two principal components",
    ggtheme = theme,
    repel = TRUE
)

# Grobs and plot
# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html

lay = rbind(
    c(1, 1),
    c(2, 3)
)

gr <- list(GDP, pr, arrows)

grid.arrange(grobs = gr, layout_matrix = lay)
