library(ggplot2)

onepraw <- read.csv("data/BakijaTable2.csv", stringsAsFactors = F)
onep <- onepraw[c(1:5),] # only first five rows used
onep$Occupation <- factor(onep$Occupation, levels = onep$Occupation)
onepm <- melt(onep, id.vars = "Occupation", variable_name = "Year")

## Some cleanup of years needed
onepm$Year <- as.character(onepm$Year)
onepm$Year <- sapply(strsplit(onepm$Year, split = "X"), function(x)
               as.numeric(x[2]))

## Generate dataframe and placement for text labels in plot
textvals <- onep[c("Occupation", "X1993")]
mids <- c(0, cumsum(textvals$X1993))
textvals$mids <- (mids[-1] + mids[-length(mids)]) / 2 - 0.3
textvals$Occupation <- as.character(textvals$Occupation)
textvals$Occupation <- c("Executives, managers, supervisors\n(non-finance)",
                         "Medical workers",
                         "Finance professions,\nincluding management",
                         "Lawyers",
                         "Tech")
                        
png("pics/oneplottime.png")
oneplottime <- ggplot(onepm, aes(Year, value, fill = Occupation)) +
  geom_area() +
  geom_text(data = textvals,
            aes(label = Occupation, y = mids), x = 1993, size = 4.5) +
  scale_fill_brewer(pal = "Paired", legend = F) +
  opts(title = "Prominent professions of the top one percent
(by income excluding capital gains)") +
scale_y_continuous("Percentage of primary taxpayers")
print(oneplottime)
dev.off()
