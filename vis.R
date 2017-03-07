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
df <- df %>%
  filter(!is.na(`Day 0`))

# Gather
df <- gather(df,
             day, 
             value,
             `Day 0` : `Day 60`)
df$day <- as.numeric(gsub('Day ', '', df$day))

# Plot
ggplot(data = df %>%
         filter(day <= 7),
       aes(x = day,
           y = value,
           group = id)) +
  geom_line(lwd = 0.1,
            alpha = 0.3) +
  facet_wrap(~category, ncol = 3) +
  theme_bw() +
  xlab('Day') +
  ylab('') +
  theme(strip.text.x = element_text(size = 6, 
                                    colour = "black", 
                                    angle = 0)) +
  scale_x_continuous(name = 'Day',
                     breaks = c(0,7)) +
  ylab('Serum IP-10 (pg/ml)')

ggsave('plot.pdf')

# Plot
ggplot(data = df,
       aes(x = day,
           y = value,
           group = id)) +
  geom_line(lwd = 0.1,
            alpha = 0.3) +
  facet_wrap(~category, ncol = 3) +
  theme_bw() +
  xlab('Day') +
  ylab('') +
  theme(strip.text.x = element_text(size = 6, 
                                    colour = "black", 
                                    angle = 0)) +
  scale_x_continuous(name = 'Day',
                     breaks = c(0,7, 60)) +
  ylab('Serum IP-10 (pg/ml)')

ggsave('plot60.pdf')

# Statistical test for lab vs clinic
model_data <- df %>%
  filter(day <= 7,
         category != 'All TB cases') %>%
  arrange(day) %>%
  mutate(cat_id = paste0(category, '_', id)) %>%
  group_by(cat_id) %>%
  summarise(reduction = value[day == 0] - value[day == 7],
            category = first(category))

ggplot(data = model_data,
       aes(x= reduction,
           group = category,
           fill = category)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_fill_manual(name = '',
                    values = c('darkorange', 'darkgreen')) +
  labs(x = 'Reduction',
       y = 'Density',
       title = 'Reduction from day 0 to 7')

ggplot(data = model_data,
       aes(x= reduction,
           group = category,
           fill = category)) +
  geom_histogram(alpha = 0.5) +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_fill_manual(name = '',
                    values = c('darkorange', 'darkgreen')) +
  labs(x = 'Reduction',
       y = 'Cases',
       title = 'Reduction from day 0 to 7')

t.test(x = model_data$reduction[model_data$category == 'Clinically-diagnosed cases'],
       y = model_data$reduction[model_data$category == 'Lab-confirmed cases'])

fit <- lm(reduction ~  category,
          data = model_data)
summary(fit)
