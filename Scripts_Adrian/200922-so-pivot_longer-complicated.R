df <- tibble(
  id = c(1,2,3),
  x02val_a = c(0,1,0),
  x03val_a = c(1,0,0),
  x04val_a = c(0,1,1),
  x02val_b = c(0,2,0),
  x03val_b = c(1,3,0),
  x04val_b = c(0,1,2),
  age02 = c(1,2,3),
  age03 = c(2,3,4),
  age04 = c(3,4,5)
)

df %>%
  pivot_longer(-id,names_to = c('.value', 'year'),names_pattern = '([a-z]+(\\d+)[a-z]+_[a-z])')
