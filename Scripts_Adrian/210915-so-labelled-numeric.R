sample <- df %>% select(P04W91) %>% filter(!is.na(.)) %>% slice_head(n = 5) %>% transmute(var = P04W91)

sample <- structure(list(
  var = structure(c(1, 2, 2, 3, 2),
                  value.labels = c(
                    no = 3, `yes, opinion` = 2, `yes, decision` = 1))),
  row.names = c(NA, -5L),
  class = "data.frame")


attributes(sample$var)

sample$var
as.factor(sample$var)
as.character(sample$var)

. <- attr(sample$var, "value.labels")
sample$var <- factor(sample$var, ., names(.))

# https://stackoverflow.com/questions/69190801/replace-the-values-in-a-named-numeric-by-its-attribute-value-label
