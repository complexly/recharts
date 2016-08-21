## ------------------------------------------------------------------------
library(recharts)
# you can also write: iris %>% echart(~Sepal.Length, ~Sepal.Width)
echart(iris, ~Sepal.Length, ~Sepal.Width)
echart(iris, ~Sepal.Length, ~Sepal.Width, series = ~Species)

