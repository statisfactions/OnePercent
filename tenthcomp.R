library(ggplot2)

tenthcomp <- read.csv("data/BakijaT1vT3.csv", stringsAsFactors = F)
tenthcomp <- tenthcomp[1:5,]
tenthcomp$Occupation <- factor(tenthcomp$Occupation)
tenthcompm <- melt(tenthcomp, id.vars = "Occupation")
tenthcompm$variable <- factor(tenthcompm$variable, labels =
                              c("With capital gains",
                                "Without capital gains"))

## Generate dataframe and placement for text labels in plot
textvals <- data.frame(Occupation = as.character(tenthcomp$Occupation))
bots <- rbind(c(0,0), cumsum(tenthcomp[-nrow(tenthcomp), 2:3]))
tops <- cumsum(tenthcomp[2:3])
textvals[c("gains", "nogains")]  <- (bots + tops)/2
textvals$Occupation <- factor(c("Executives, managers, supervisors\n(non-finance)",
                         "Finance professions,\nincluding management",
                         "Lawyers",
                         "Medical workers",
                         "Real estate"))
textvalsm <- melt(textvals, id.vars = "Occupation")
textvalsm$variable <- factor(tenthcompm$variable, labels =
                              c("With capital gains",
                                "Without capital gains"))

## Plot
png("pics/tenthcomp.png")
tenthcomppl <- ggplot(tenthcompm, aes(variable, value, fill = Occupation)) +
  geom_bar() +
  scale_fill_brewer(pal = "Paired", legend = F) +
  opts(title = "Prominent professions of the top 0.1 percent, 2005
(by income with and without capital gains)") +
  geom_text(data = textvalsm,
             aes(variable, value, label = Occupation),  size = 4) +
 scale_x_discrete("") + 
 scale_y_continuous("Percentage of primary taxpayers")
print(tenthcomppl)
dev.off()
