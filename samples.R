

g1_100 = data.frame(vot = rnorm(100, 60, 25), participant = 1:100) %>% 
  mutate(group = "g1") %>% 
  mutate(sample = "n = 100")

g2_100 = data.frame(vot = rnorm(100, 60, 25), participant = 1:100) %>% 
  mutate(group = "g2") %>% 
  mutate(sample = "n = 100")

g1_10 = data.frame(vot = rnorm(20, 60, 25), participant = 1:100) %>% 
  mutate(group = "g1") %>% 
  mutate(sample = "n = 20")
g2_10 = data.frame(vot = rnorm(20, 60, 25), participant = 1:100) %>% 
  mutate(group = "g2") %>% 
  mutate(sample = "n = 20")

full_df = rbind(g1_10, g2_10, g1_100, g2_100) 

mean(g1_100$vot)
mean(g2_100$vot)
sd(g1_100$vot)
sd(g2_100$vot)


mean(g1_10$vot)
mean(g2_10$vot)
sd(g1_10$vot)
sd(g2_10$vot)

full_df %>% 
  ggplot(aes(x = vot, y = group)) + geom_boxplot() + facet_grid(~ sample)




