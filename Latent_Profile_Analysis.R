# 6. Latent Profile Analysis ----
# check optimal number of classes and the optimal model variant
BIC <- mclustBIC(food_pre_raw5)

png("BIC.png", width = 4600, height = 4200, res = 600)  # 像素×2，物理尺寸翻倍
par(mar = c(3, 3, 2, 1), cex.axis = 0.8, cex.lab = 0.9)
plot(BIC)
dev.off()

## EEV with 3 class model is the optimal fit
fpm1p3 <- Mclust(food_pre_raw5, modelNames = "EEV", G = 3, x = BIC)
summary(fpm1p3)

modelEEV3 = data.frame(fpm1p3$classification)
modelEEV3$eid = food_pre_raw3$eid
colnames(modelEEV3) <- c("Profile", "eid")


EEV <- data.frame(fpm1p3$parameters$mean) %>%
  rownames_to_column() %>%
  rename(Liking = rowname) %>%
  pivot_longer(cols = c(X1, X2, X3), names_to = "Profile", values_to = "Mean") %>%
  mutate(Mean = round(Mean, 2),
         Mean = ifelse(Mean > 1, 1, Mean)) %>%
  left_join(food_info, by = c("Liking" = "Field_ID")) %>% 
  mutate(
    Factor_Dimension = paste(Factor, " (", Dimension, ")", sep = "")
  )

## Data visualisation ----
Liking = ggplot(EEV, aes(x = Description, y = Mean, fill = Profile)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('Health' = '#00B936', 'Omnivore' = '#619CFF', 'Sweet-tooth' = '#F8766D')) +
  labs(x = "Description",
       y = "Mean",
       fill = "Profile") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"), 
    legend.text = element_text(size = 12),
    text = element_text(family = "Arial")
  )

ggsave("Liking2.png", plot = Liking, width = 19,height = 7,dpi = 600)