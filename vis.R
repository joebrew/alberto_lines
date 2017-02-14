library(readxl)
library(tidyverse)

# Readin 
df <- read_excel('stupid graph.xlsx')

# Define categories
categories <- c(rep('All TB cases', 3),
                rep('Lab-confirmed cases', 3),
                rep('Clinically-diagnosed cases', 3))

unique_categories <- unique(categories)

counter <- 1
for (i in 1:length(unique_categories)){
  this_category <- unique_categories[i]
  x <- read_excel('stupid graph.xlsx',
                  skip = 1)[, which(categories == this_category)]
  x$category <- this_category
  x$id <- counter:(nrow(x) + counter - 1)
  if(i == 1){
    df <- x
  } else {
    df <- bind_rows(x,
                    df)
  }
}

# Gather
df <- gather(df,
             day, 
             value,
             `Day 0` : `Day 60`)
df$day <- as.numeric(gsub('Day ', '', df$day))

# Plot
ggplot(data = df,
       aes(x = day,
           y = value,
           group = id)) +
  geom_line(lwd = 0.1,
            alpha = 0.5) +
  facet_wrap(~category, ncol = 1) +
  theme_bw() +
  xlab('Day') +
  ylab('')
ggsave('plot.pdf')
