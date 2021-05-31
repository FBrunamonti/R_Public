# ----------------------------------------------
# Import
library(rpredictit)
library(tibble)
"%>%" <- magrittr::"%>%"

library(ggplot2)
theme_set(theme_light())

# -----------------------------------------------
# Define functions
contractsList <- function(){
    h <- all_markets()
    h <- subset(h, select = c(id, name))
    h <- unique(h)
    print(h, n=Inf)
}

plotOdds <- function(market){
    # Import & Clean
    df <- single_market(market)
    df2 <- subset(df, select = c(contract_id, lastTradePrice))
    df2$contract_id <- df$contract_name
    colnames(df2) <- c('names', 'odds')
    
    # Prices
    if(length(df2$names) == 1){
        df2[1] <- "Yes"
        df2 <- df2 %>% add_row(names = "No", odds = 1-df$lastTradePrice)
    } else {
        df2$odds <- df2$odds/sum(df2$odds)
    }

    df2$odds <- df2$odds * 100
    
    # ----------------------------------------------
    # Plots
    ggplot(data = df2, aes(x = reorder(names, odds), y = odds, group = 1)) + 
        geom_bar(stat = 'identity', color = 'black', fill = '#F6EBBD', width = 0.5, alpha = 0.7) + 
        geom_text(aes(label = round(odds)), vjust = 0.3, hjust = -0.5, size = 3) + 
        geom_hline(yintercept = seq(0, 100, 25), linetype = 'dashed') + 
        labs(title = df$name[1], x = NULL, y = 'Odds from PredictIt.org') + 
        coord_flip()
}

# -----------------------------------------------
# Run
plotOdds(6867)
plotOdds(7057)
plotOdds(7013)
plotOdds(7053)
plotOdds(7136)

h <- contractsList()
