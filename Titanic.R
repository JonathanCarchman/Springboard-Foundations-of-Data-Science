# Excercise completed by J. Carchman
# Foundations of Data Science
# Homework 4.2
# 11/26/17


# Loads ggplot
library(ggplot2)

# Loads titanic data 
titanic = read.csv("C:/Users/Jonathan/Desktop/Titanic dataset 714.csv", header = TRUE)


# 1 - Check the structure of titanic
str(titanic)

# 2 - Use ggplot() for the first instruction
# This creates a simple bar graph showing passengers by cabin class and sex
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")

# 3 - Plot 2, add facet_grid() layer
# This creates a simple bar graph showing passengers by cabin class and sex with and additional facet, survival
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")+
  facet_grid(. ~ Survived)

# 4 - Define an object for position jitterdodge, to use below
posn_jd <- position_jitterdodge(0.5, 0, 0.6)

# 5 - Plot 3, but use the position object from instruction 4
# This creates a simple scatter diagram, showing passegers as points and arranging by age and cabin class, 
# coloring by sex and a survival facet
ggplot(titanic, aes(x = Pclass, y=Age, color = Sex)) +
  geom_point(size=3, alpha=.5, position = posn_jd)+
  facet_grid(. ~ Survived)
