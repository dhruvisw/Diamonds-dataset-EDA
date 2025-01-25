#3501_Dhruvi
setwd("C:/Users/DELL/Desktop/DHRUVI/DHRUVI Data Science/Sem 4/DV")
print(getwd())
library(tidyverse)

df=read_csv("diamonds.csv")

print(sum(is.na(df)))

print(sum(is.na(df$srno)))
print(sum(is.na(df$carat)))
print(sum(is.na(df$cut)))
print(sum(is.na(df$color)))
print(sum(is.na(df$clarity)))
print(sum(is.na(df$depth)))
print(sum(is.na(df$table)))
print(sum(is.na(df$price)))
print(sum(is.na(df$x)))
print(sum(is.na(df$y)))
print(sum(is.na(df$z)))


filtered_data=filter(df,!is.na(df$color))
new_df = filtered_data %>% select(!c(srno))

group_data = new_df %>% group_by(cut)
summary_data=summarise(group_data)
print(summary_data)

print(summary(new_df))


plot(x = df$carat,
     y = df$price,
     xlab = "Carat",
     ylab = "Price",
     col = "red",
     pch = 4,
     main = "Price vs Carat")



library(plotly)
require(ggplot2)

p <- ggplot(data=diamonds, aes(x = df$carat, y = df$depth)) 
p <- p + geom_point(aes(color= df$clarity)) +
  xlab("Carat") +  ylab("Depth") +
  ggtitle("Diamond carat-Depth")

ggplotly(p)


p = ggplot(diamonds, aes(x = cut, y = price, fill = cut)) + geom_boxplot() + theme_bw()
print(p)


# We can plot histograms for each level of the cut factor variable using facet_grid

options(repr.plot.width = 10, repr.plot.height = 8)
p = ggplot(diamonds, aes(x = price, fill = cut)) + 
  geom_histogram(bins=20) + 
  facet_grid(cut ~ ., scales = "free") + 
  theme_bw() 
p  


diamonds %>%
  ggplot(aes(clarity, group = clarity, fill = clarity))+
  geom_bar(stat = 'count', alpha = 0.77)+
  ggtitle("Clarity Distribution")

hist(diamonds$price)
hist(diamonds$carat)

diamonds %>%
  ggplot(aes(price, ..density.., fill = cut))+
  geom_histogram(alpha = 0.4, binwidth = 1000)+
  geom_density(alpha = 0.42)+
  facet_wrap(~ cut, scales = 'free')+
  ggtitle('Cut/Price Distribution')


IQR(subset(diamonds, cut == 'Fair')$price)
IQR(subset(diamonds, cut == 'Good')$price)
IQR(subset(diamonds, cut == 'Very Good')$price)
IQR(subset(diamonds, cut == 'Premium')$price)
IQR(subset(diamonds, cut == 'Ideal')$price)

df_cut <- data.frame(cut_level = c("Fair", "Good", "Very Good", "Premium", "Ideal"), iqr_cut = c(3155.25, 3883, 4460.75, 5250, 3800.5))
print(df_cut)

df_cut$cut_level <- factor(df_cut$cut_level, levels = c("Fair", "Good", "Very Good", "Premium", "Ideal"))
ggplot(data = df_cut) + geom_col(mapping = aes(x = cut_level, y = iqr_cut, fill = cut_level)) + xlab("Quality of Cut") + ylab("Interquartile Range") + ggtitle("INTERQUARTILE RANGE BY CUT") + scale_fill_brewer(palette = "Reds")



diamonds %>%
  ggplot(aes(carat, log(price), color = clarity))+
  geom_point()+
  ggtitle("Normalize price")



diamonds %>%
  filter(clarity == 'IF') %>%
  ggplot(aes(carat, price, color = cut)) +
  geom_point()+
  scale_y_continuous(labels = scales::dollar_format())+
  geom_smooth(method = 'lm', formula = 'y ~ x', se = FALSE)+
  ggtitle("Clarity Flawless: Price by Carat, color = cut")+
  labs(x = "ct", y = '', fill = "Cut")