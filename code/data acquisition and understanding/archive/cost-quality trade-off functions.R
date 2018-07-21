################################################################################
## Cost-Quality tradoff functions
################################################################################
## Initialization
cat("\014")
rm(list = ls())
source("functions/labelingCostQualityTradeoff.R")
x_ticks <- c(0.02, 0.08, 0.14, 0.19, 0.25)
price_per_label_values <- seq(range(x_ticks)[1], range(x_ticks)[2], length.out = 1e3)
trade_offs <- c("Fix", "Concave", "Asymptotic") # ,"F1","F2","F3")


##############
# basic plot #
##############
par(mfrow = c(1, 3), cex = 1.2, mai = c(1, 0.5, 0.1, 0.1), pty = "s")

for (t in trade_offs) {
    ## Calculate probabilities
    probabilities <- labelingCostQualityTradeoff(t, price_per_label_values, fixProbability = 0.85)
    # probabilities = round(probabilities,4)
    ## Create blank canvas
    plot(0, 0,
        type = "n", col = "red",
        ylim = c(0.5, 1), xlim = c(0.02, 0.25), axes = F,
        # main=paste(t,"trade-off\nfunction"),
        xlab = "Price Per Label", ylab = "Correct Label Probability"
    )
    ## Set axies
    axis(1, at = x_ticks)
    axis(2, at = seq(0.5, 1, by = 0.1), las = 2)
    ## Add grid
    abline(h = seq(0.5, 1, 0.1), lty = 1, col = "cornsilk2")
    abline(v = x_ticks, lty = 1, col = "cornsilk2")
    box()
    ## Plot the trade-off function
    lines(price_per_label_values, probabilities, col = "red", lwd = 3)
}


##########
# ggplot #
##########
# library(ggplot2)
# plot_data = data.frame()
# for(t in trade_offs){
#     probabilities = labelingCostQualityTradeoff(t, price_per_label_values, fixProbability = 0.85)
#     #probabilities = round(probabilities,2)
#     plot_data = rbind(plot_data,data.frame(price=price_per_label_values,
#                                            probabilities,
#                                            trade_offs=t))
# }
# fig1 <- ggplot(plot_data, aes(x=price, y=probabilities, group=trade_offs)) +
#     # Add lines to the plot
#     geom_line(aes(colour = trade_offs), size=2) +
#     # Add scatter points to the plot
#     #geom_point(aes(colour = trade_offs), size=4) +
#     # Y axis attributes
#     ylab("Probability") +
#     # X axis attributes
#     scale_x_continuous(breaks = x_ticks, limits = range(x_ticks)) +
#     xlab("Price Per Label") +
#     # Theme attributes
#     theme_bw() + theme(legend.position="none", text=element_text(size=20)) +
#     facet_grid(. ~ trade_offs)
# plot(fig1)
# ggsave(file=file.path(getwd(),"Cost-Quality tradeoff functions.png"), fig1, width=12, height=4)
