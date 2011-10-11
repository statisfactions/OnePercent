library(ggplot2)

tenthtime <- read.csv("data/BakijaTable3.csv")
tenthtime <- tenthtime[1:5, ]
tenthtime$Occupation <- factor(tenthtime$Occupation)
tenthtimem <- melt(tenthtime, id.vars = "Occupation", variable_name = "Year")

## Dealing with years again
tenthtimem$Year <- as.character(tenthtimem$Year)
tenthtimem$Year <- sapply(strsplit(tenthtimem$Year, split = "X"), function(x)
               as.numeric(x[2]))

## Generate dataframe and placement for text labels in plot
textvals <- tenthtime[c("Occupation", "X1993")]
bots <- c(0, cumsum(textvals$X1993[-nrow(textvals)]))
tops <- cumsum(textvals$X1993)
textvals$mids  <- (bots + tops)/2 

## Plot
png("pics/tenthtimeplot.png")
tenthtimepl <- ggplot(tenthtimem, aes(Year, value, fill = Occupation)) +
  geom_area() +
  geom_text(data = textvals,
            aes(label = Occupation, y = mids), x = 1993, size = 4.5) +
  scale_fill_brewer(pal = "Paired", legend = F) +
  opts(title = "Prominent professions of the top 0.1 percent
(by income excluding capital gains)") +
scale_y_continuous("Percentage of primary taxpayers")
print(tenthtimepl)
dev.off()
