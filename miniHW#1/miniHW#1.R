Time <- c(0, 0.6, 1.1, 1.7, 1.9, 2.4, 2.8, 3.3, 3.7)
DO <- c(0.39, 0.37, 0.31, 0.28, 0.27, 0.25, 0.20, 0.17, 0.16)
model.lm <- lm(DO ~ Time) # regression model
model.lm0 <- lm(DO ~ 0 + Time) # regression model intercepting (0,0)

r <- cor(Time, DO) # correlation value
R2.1 <- summary(model.lm)$r.square # R2 value for first regression model
R2.2 <- summary(model.lm0)$r.square # R2 for (0,0) intercepting regression model

# legend build part

rp1 = vector('expression', 2)
rp1[1] = substitute(expression(italic(r) == MYVALUE), list(MYVALUE = format(r, dig = 4)))[2]
rp1[2] = substitute(expression(R ^ 2 == MYOTHERVALUE), list(MYOTHERVALUE = format(R2.1, digits = 4)))[2]

rp2 = vector('expression', 1)
rp2[1] = substitute(expression(R ^ 2 == MYOTHERVALUE), list(MYOTHERVALUE = format(R2.2, digits = 4)))[2]

# legend build part

# plotting and saving part

if (!file.exists("./miniHW#1/")) {
    dir.create(dirname("./miniHW#1/"), showWarnings = FALSE)
}

png(
    "./miniHW#1/rplot1.png",
    width = 1200,
    height = 1200,
    res = 300
)
plot(
    Time,
    DO,
    pch = 16,
    frame = F,
    xlim = c(0, 4),
    ylim = c(0, 0.5),
    col = "#2E9FDF"
)
legend('topright', legend = rp1, bty = 'n')
abline(model.lm, lty = 3)
title("DO Change w.r.t. Time")
dev.off()

png(
    "./miniHW#1/rplot2.png",
    width = 1200,
    height = 1200,
    res = 300
)
plot(
    Time,
    DO,
    pch = 16,
    frame = F,
    xlim = c(0, 4),
    ylim = c(0, 0.5),
    col = "#2E9FDF"
)
legend('topright', legend = rp2, bty = 'n')
abline(model.lm0, lty = 2)
title("DO Change w.r.t. Time")
dev.off()

# plotting and saving part

rm(list = ls(all = T))
gc()
cat("\014")