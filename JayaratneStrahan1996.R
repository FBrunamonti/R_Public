# Libraries ---------------------------------------------------------------
library(tibble)
library(dplyr)
library(rio)

library(lfe)
library(fwildclusterboot)
library(did)
library(bacondecomp)
library(conleyreg)

library(gridExtra)
library(ggplot2)
theme_set(theme_bw())

# Parameters ---------------------------------------------------------------
address <- "https://raw.githubusercontent.com/FBrunamonti/Data_Public/main/JayaratneStrahan1996.csv"

# Get data ---------------------------------------------------------------
df <- rio::import(address) %>% as_tibble #%>% filter(DeregulateYear == 0)

# Panel regression ---------------------------------------------------------------
# Main specification
r1 <- felm(Growth ~ Deregulated | State + Year | 0 | State + Year, df)

# # Fast and wild bootstrap
# r2 <- felm(Growth ~ Deregulated | State, df)
# bootYear <- boottest(r2, clustid = "Year", param = "Deregulated", B = 10^4 - 1)
# bootState <- boottest(r2, clustid = "State", param = "Deregulated", B = 10^4 - 1)
# bootTwoWay <- boottest(r2, clustid = c("State", "Year"), param = "Deregulated", B = 10^4 - 1)
# summary(bootTwoWay)
# 
# # Conley standard errors
# r3 <- conleyreg(
#     Growth ~ Deregulated | State + Year,
#     unit = "StateIndex", time = "Year",
#     10000,
#     lag_cutoff = 3,
#     lat = "Lat", lon = "Long",
#     dist_comp = "fast",
#     verbose = FALSE,
#     data = df
# )

# Regression in log-levels
r4 <- felm(log(Income) ~ Deregulated | State + Year | 0 | State + Year, df)

# Event study and treatment effect ---------------------------------------------------------------
attgt <- att_gt(
    yname = "Growth",
    tname = "Year",
    idname = "StateIndex",
    gname = "YearOfTreatment",
    data = df,
    cluster = "State"
)

es <- aggte(
    attgt,
    type = "dynamic", # For event study
    min_e = -4, max_e = +6,
    bstrap = TRUE,
    clustervars = "State"
)

ggdid(es) +
    xlim(-4, 6) + ylim(-0.03, 0.03) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(legend.position = "none")

# Goodman-Bacon decomposition ---------------------------------------------------------------
df_bacon <- bacon(
    Growth ~ Deregulated,
    data = df,
    id = "State",
    time_var = "Year"
) %>% as_tibble

p1 <- ggplot(df_bacon) +
    aes(x = weight, y = estimate) + 
    labs(x = "Weight", y = "Estimate", shape = "Type") + 
    geom_point() + 
    facet_wrap(~ type) + 
    geom_smooth(method = "gam", se = FALSE)

p2 <- ggplot(df_bacon, aes(x = weight, fill = type, color = type)) + 
    geom_density(alpha = 0.15, adjust = 1/5, outline.type = "full") + 
    theme(
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.80, 0.80)
    ) + 
    labs(x = "Weight", y = "Density", title = "Density of weights from Goodman-Bacon decomposition")

weights <- df_bacon %>%
    group_by(type) %>%
    summarize(weight = sum(weight))

p3 <- ggplot(data = weights, aes(x = reorder(type, weight), y = weight, group = 1)) + 
    geom_bar(stat = 'identity', color = 'black', fill = '#F6EBBD', width = 0.5, alpha = 0.7) + 
    geom_hline(yintercept = 0, linetype = 'dashed') + 
    labs(y = "Weight", x = NULL) + 
    ylim(0, 1) + 
    coord_flip()

grid.arrange(p1, p3, nrow = 1)
