data = data.frame(
  asihckhdoydkhxiydfgfTgdsx = sample(LETTERS[1:3], 100, replace=TRUE),
  a30mvxigxkghc5cdsvxvyv0ja = sample(LETTERS[1:3], 100, replace=TRUE),
  value = rnorm(100)
)
colu = names(data)[-3]
library(dplyr)
df2 <- data %>%
  group_by_at(vars(one_of(colu))) %>%
  mutate(percent=value/sum(value))
