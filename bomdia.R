
library(dplyr)
library(ggplot2)
library(hrbrthemes)

# CONTANDO OCORRÊNCIAS DO BANCO INTEGRAL
bomdia_confianca <- bomdia %>% 
  count(Confiança.Entrega.Proteção, sort= TRUE)

bomdia_coragem <- bomdia %>% 
  count(Coragem, sort= TRUE)

bomdia_bondade <- bomdia %>% 
  count(Bondade.Caridade, sort= TRUE)

bomdia_justica <- bomdia %>% 
  count(Justiça.Honestidade, sort= TRUE)

bomdia_humildade <- bomdia %>% 
  count(Humildade, sort= TRUE)

bomdia_gratidao <- bomdia %>% 
  count(Gratidão, sort= TRUE)

bomdia_familia <- bomdia %>% 
  count(Família, sort= TRUE)

bomdia_religiao <- bomdia %>% 
  count(Religião, sort= TRUE)

bomdia_nacionalismo <- bomdia %>% 
  count(Nacionalismo, sort= TRUE)

bomdia_meritocracia <- bomdia %>% 
  count(Meritocracia, sort= TRUE)

bomdia_corrupcao <- bomdia %>% 
  count(Corrupção, sort= TRUE)

bomdia_lider <- bomdia %>% 
  count(Líder.pol, sort= TRUE)

bomdia_justica <- bomdia %>% 
  count(Justiça.Honestidade, sort= TRUE)

bomdia_negro <- bomdia %>% 
  count(Negro, sort= TRUE)

bomdia_mulher <- bomdia %>% 
  count(Mulher, sort= TRUE)

#PLOT1: Série temporal DIA

bomdia_history_day <- bomdia_history_day %>% 
  mutate(bomdia = as.Date(bomdia))

Sys.setlocale("LC_TIME", "pt_BR")

ggplot(bomdia_history_day, aes(x=bomdia, y=n)) +
  #geom_segment( aes(x=bomdia, xend=bomdia, y=0, yend=n), color="grey") +
  geom_line( color="#440154FF", size=.9) +
  #geom_line() +
  #scale_y_log10() +
  theme_ipsum() +
  scale_colour_viridis_d(direction = -1) +
  #geom_vline(xintercept="2018-10-10", color="red", size=.5) +
  geom_text(data=subset(bomdia_history_day, n>5), aes(family = "Arial", label=bomdia), angle=90, vjust=-0.5, size = 2.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Gráfico 1: Dias com maior incidência de memes de Bom Dia",
       subtitle = "",
       x = "Data",
       y = "Frequência",
       caption = "Fonte: coLAB/UFF + Margem/UFMG")

Sys.setlocale("LC_TIME", "en_US") 


#PLOT2: Série temporal MÊS

bomdia_history_month$mes <- factor(bomdia_history_month$mes,levels = c("março","abril","maio","junho","julho","agosto","setembro","outubro","novembro"))
#bomdia_history_month <- as.data.frame(bomdia_history_month)

bomdia_history_month <- bomdia_history_month %>% 
  mutate(perc = round((n / 569)*100, digits = 1))

plot2A <- ggplot(bomdia_history_month, aes(x=mes, y = perc, fill = mes)) + 
  geom_bar(stat = "identity", alpha=.8, width=.9) +
  #geom_segment( aes(x=mes, xend=mes, y=0, yend=perc), color="grey") +
  #geom_point( color="#440154FF", size=2) +
  #geom_line() +
  #scale_y_log10() +
  theme_ipsum() +
  scale_fill_viridis_d(direction = -1) +
  #geom_vline(xintercept="2018-10-10", color="red", size=.5) +
  geom_text(aes(family = "Arial", label=perc), angle=0, vjust=-0.8, size = 2.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Gráfico 2: Meses com maior incidência de memes de Bom Dia",
       subtitle = "Quantidade de memes enviados no mês (percentual)",
       x = "Data",
       y = "Frequência (%)",
       caption = "")

plot2B <- ggplot(bomdia_history_month, aes(x=mes, y = media, fill = mes)) + 
  geom_bar(stat = "identity", alpha=.8, width=.9) +
  #geom_segment( aes(x=mes, xend=mes, y=0, yend=perc), color="grey") +
  #geom_point( color="#440154FF", size=2) +
  #geom_line() +
  #scale_y_log10() +
  #theme(legend.position="none") +
  theme_ipsum() +
  scale_fill_viridis_d(direction = -1) +
  #geom_vline(xintercept="2018-10-10", color="red", size=.5) +
  geom_text(aes(family = "Arial", label=media), angle=0, vjust=-0.8, size = 2.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "",
       subtitle = "Média de memes enviados por dia",
       x = "Data",
       y = "Média",
       caption = "Fonte: coLAB/UFF + Margem/UFMG")

library(cowplot)
library(patchwork)
plot2A + plot2B


#PLOT3: Representações

bomdia_representacoes <- bomdia_representacoes %>% 
  mutate(perc = round((n / 569)*100, digits = 1))

ggplot(bomdia_representacoes, aes(x=representacoes, y = perc, fill = representacoes)) + 
  geom_bar(stat = "identity", alpha=.8, width=.9) +
  #coord_flip() +
  scale_fill_viridis_d(direction = -1) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  #theme(legend.position="none") +
  geom_text(aes(family = "Arial", label=perc), vjust=-0.5, size = 3.5) +
  labs(title = "Gráfico 3: Representações iconográficas em memes de Bom Dia", 
       subtitle = "N = 569",
       x = "Representações",
       y = "Frequência (%)",
       caption = "Fonte: coLAB/UFF + Margem/UFMG") +
  theme_ipsum()
  
# PLOT4: Valores

bomdia_valores <- bomdia_valores %>% 
  mutate(perc = round((n / 569)*100, digits = 1))

ggplot(bomdia_valores, aes(x=valores, y = perc, fill = valores)) + 
  geom_bar(stat = "identity", alpha=.8, width=.9) +
  #coord_flip() +
  scale_fill_viridis_d(direction = -1) +
  theme_ipsum() + 
  geom_text(aes(family = "Arial", label=perc), vjust=-0.5, size = 3.5) +
  labs(title = "Gráfico 4: Valores expressos em memes de Bom Dia", 
       subtitle = "N = 569",
       x = "Valores",
       y = "Frequência (%)",
       caption = "Fonte: coLAB/UFF + Margem/UFMG")

# PLOT5: Virtudes

bomdia_virtudes <- bomdia_virtudes %>% 
  mutate(perc = round((n / 569)*100, digits = 1))

ggplot(bomdia_virtudes, aes(x=virtudes, y = perc, fill = virtudes)) + 
  geom_bar(stat = "identity", alpha=.8, width=.9) +
  #coord_flip() +
  scale_fill_viridis_d(direction = 1) +
  theme_ipsum() + 
  geom_text(aes(family = "Arial", label=perc), vjust=-0.5, size = 3.5) +
  labs(title = "Gráfico 5: Virtudes expressas em memes de Bom Dia", 
       subtitle = "N = 569",
       x = "Virtudes",
       y = "Frequência (%)",
       caption = "Fonte: coLAB/UFF + Margem/UFMG")


# PLOT 6: Virtudes por mês

bomdia_corr_bondade <- bomdia_corr %>% 
  count(Mes, Bondade) %>% 
  filter(Bondade == 1) %>% 
  mutate(Bondade = n) %>% 
  select(Mes, Bondade)
bomdia_corr_confianca <- bomdia_corr %>% 
  count(Mes, Confiança) %>% 
  filter(Confiança == 1) %>% 
  mutate(Confiança = n) %>% 
  select(Mes, Confiança)
bomdia_corr_coragem <- bomdia_corr %>% 
  count(Mes, Coragem) %>% 
  filter(Coragem == 1) %>% 
  mutate(Coragem = n) %>% 
  select(Mes, Coragem)
bomdia_corr_gratidao <- bomdia_corr %>% 
  count(Mes, Gratidão) %>% 
  filter(Gratidão == 1) %>% 
  mutate(Gratidão = n) %>% 
  select(Mes, Gratidão)
bomdia_corr_humildade <- bomdia_corr %>% 
  count(Mes, Humildade) %>% 
  filter(Humildade == 1) %>% 
  mutate(Humildade = n) %>% 
  select(Mes, Humildade)
bomdia_corr_justica <- bomdia_corr %>% 
  count(Mes, Justiça) %>% 
  filter(Justiça == 1) %>% 
  mutate(Justiça = n) %>% 
  select(Mes, Justiça)

bomdia_corr_primario <- bomdia_corr_bondade
bomdia_corr_total <- bomdia_corr_primario %>%
  full_join(bomdia_corr_confianca) %>%
  full_join(bomdia_corr_coragem) %>%
  full_join(bomdia_corr_gratidao) %>%
  full_join(bomdia_corr_humildade) %>%
  full_join(bomdia_corr_justica)
rm(bomdia_corr_primario)
rm(bomdia_corr_bondade)
rm(bomdia_corr_confianca)
rm(bomdia_corr_coragem)
rm(bomdia_corr_gratidao)
rm(bomdia_corr_humildade)
rm(bomdia_corr_justica)

#bomdia_corr_total_2 <- bomdia_corr_total[,-1]
#rownames(bomdia_corr_total_2) <- bomdia_corr_total[,1]

bomdia_corr_total_2 <- tidyr::gather(bomdia_corr_total, "Bondade", "Confiança", "Coragem", "Gratidão", "Humildade", "Justiça", 
              key = Virtude, value = n)

bomdia_corr_total_2$Mes <- factor(bomdia_corr_total_2$Mes,levels = c("março","abril","maio","junho","julho","agosto","setembro","outubro","novembro"))

ggplot(bomdia_corr_total_2, aes(fill=Virtude, y=n, x=Mes)) + 
  geom_bar(position="dodge", stat = "identity", alpha=.8, width=.9) +
  #coord_flip() +
  scale_fill_viridis_d() +
  #geom_text(aes(family = "Arial", label=n), angle=0, vjust=-0, size = 2.5) +
  theme_ipsum() + 
  labs(title = "Gráfico 6: Virtudes vs Mês", 
       subtitle = "N = 569",
       x = "Virtudes",
       y = "Frequência",
       caption = "Fonte: coLAB/UFF + Margem/UFMG")

# TABELA RESÍDUOS

write.csv(bomdia_corr_total, "~/Downloads/bomdia_corr_residuos.csv")

bomdia_res$Mes <- factor(bomdia_res$Mes,levels = c("março","abril","maio","junho","julho","agosto","setembro","outubro","novembro"))

library(ggpubr)
main.title <- paste0("Tabela 1: Virtudes vs Mês")
subtitle <- paste0(
  "Resíduos padronizados"
) %>%
  strwrap(width = 80) %>%
  paste(collapse = "\n")

tab <- ggtexttable(bomdia_res, theme = ttheme("light"), , rows = NULL)

#Células realçadas
tab <- table_cell_bg(tab, row = 6, column = 4, linewidth = 5,
                     fill="#1F968BFF", color = "#1F968BFF", alpha=.8)
tab <- table_cell_bg(tab, row = 8, column = 6, linewidth = 5,
                     fill="#1F968BFF", color = "#1F968BFF", alpha=.8)
#Plot
tab %>%
  tab_add_title(text = subtitle, face = "plain", size = 10) %>%
  tab_add_title(text = main.title, face = "bold", padding = unit(0.1, "line")) %>%
  tab_add_footnote(text = "Fonte: coLAB/UFF + Margem/UFMG", size = 10, face = "italic")


#PLOT 8: Regressões?

#Calculando R^2

bomdia_lm2 <- lm(coragem~confiança,data=bomdia_lm) 
summary(bomdia_lm2)
# Call:
#   lm(formula = coragem ~ confiança, data = bomdia_lm)
#
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.5079 -1.3565 -0.8278  0.0400  7.3599 
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  -2.7201     4.2451  -0.641   0.5421  
# confiança     0.3774     0.1409   2.678   0.0316 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 4.577 on 7 degrees of freedom
# Multiple R-squared:  0.5061,	Adjusted R-squared:  0.4355 
# F-statistic: 7.172 on 1 and 7 DF,  p-value: 0.03163

bomdia_lm3 <- lm(formula = coragem ~ bondade + confiança + gratidão + humildade + justiça, data = bomdia_lm)
summary(bomdia_lm3)

#Plotando gráfico
ggplot(bomdia_lm, aes(x=coragem, y=confiança)) + 
  geom_point(size=2, color="#1F968BFF", shape=21) + 
  #xlim(0, 25)+ylim(0, 50) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color="#440154FF", alpha=.8) + 
  theme_ipsum() + labs(title = "Gráfico 7: Coragem vs. Confiança",
                       subtitle = "R² = 0.5061",
                       x = "Confiança (resíduos padronizados)",
                       y = "Coragem (resíduos padronizados)",
                       caption = "Fonte: coLAB/UFF + Margem/UFMG")

# IMG1:

# Plotar quatro imagens lado a lado com um único título
#Bondade

imagem1 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/dataset_bomdia_final/IMG-20201028-WA0069.jpg", scale = 0.9)
imagem1 <- ggdraw(add_sub(imagem1, "Bondade", vpadding=grid::unit(0, "lines"),
               y = 0.7, x = 0.15, size = 9, hjust = 0))
#Confiança
imagem2 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/dataset_bomdia_final/IMG-20200511-WA0252.jpg", scale = 0.9)
imagem2 <- ggdraw(add_sub(imagem2, "Confiança", vpadding=grid::unit(0, "lines"),
                          y = 0.7, x = 0.15, size = 9, hjust = 0))
#Coragem
imagem3 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/dataset_bomdia_final/IMG-20201002-WA0102.jpg", scale = 0.9)
imagem3 <- ggdraw(add_sub(imagem3, "Coragem", vpadding=grid::unit(0, "lines"),
                          y = 0.7, x = 0.15, size = 9, hjust = 0))
#Gratidão
imagem4 <- ggdraw() + 
  #draw_label("Draft", colour = "#80404080", size = 40, angle = 45) + #Escrever sobre a imagem
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/dataset_bomdia_final/IMG-20200418-WA0098.jpg", scale = 0.9)
imagem4 <- ggdraw(add_sub(imagem4, "Gratidão", vpadding=grid::unit(0, "lines"),
                          y = 0.7, x = 0.15, size = 9, hjust = 0))
#Humildade
imagem5 <- ggdraw() + 
  #draw_label("Draft", colour = "#80404080", size = 40, angle = 45) + #Escrever sobre a imagem
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/dataset_bomdia_final/IMG-20200801-WA0057.jpg", scale = 0.9)
imagem5 <- ggdraw(add_sub(imagem5, "Humildade", vpadding=grid::unit(0, "lines"),
                          y = 0.7, x = 0.15, size = 9, hjust = 0))
#Justiça
imagem6 <- ggdraw() + 
  #draw_label("Draft", colour = "#80404080", size = 40, angle = 45) + #Escrever sobre a imagem
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/dataset_bomdia_final/IMG-20200522-WA0045.jpg", scale = 0.9)
imagem6 <- ggdraw(add_sub(imagem6, "Justiça", vpadding=grid::unit(0, "lines"),
                          y = 0.7, x = 0.15, size = 9, hjust = 0))

# Criar lista para imagens a serem plotadas
plot_row <- plot_grid(imagem1, imagem2, imagem3, imagem4, imagem5, imagem6, ncol = 3)

# Adicionar título
title <- ggdraw() +
  draw_label(
    "Quadro 1: Virtudes em memes de Bom Dia",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme_ipsum()

# Adicionar legenda
caption <- ggdraw() +  
  theme_ipsum() +
  labs(caption = "Fonte: coLAB/UFF + Margem/UFMG")

# Plotar imagens com título e legenda
plot_grid(
  title, plot_row, caption,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

# IMG2:

# Plotar quatro imagens lado a lado com um único título
#Família
imagem7 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/dataset_bomdia_final/IMG-20200423-WA0073.jpg", scale = 0.9)
imagem7 <- ggdraw(add_sub(imagem7, "Família", vpadding=grid::unit(0, "lines"),
                          y = 0.7, x = 0.3, size = 9, hjust = 0))
#Meritocracia
imagem8 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/dataset_bomdia_final/IMG-20200625-WA0077.jpg", scale = 0.9)
imagem8 <- ggdraw(add_sub(imagem8, "Meritocracia", vpadding=grid::unit(0, "lines"),
                          y = 0.7, x = 0.3, size = 9, hjust = 0))
#Nacionalismo
imagem9 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/dataset_bomdia_final/IMG-20200612-WA0001.jpg", scale = 0.9)
imagem9 <- ggdraw(add_sub(imagem9, "Nacionalismo", vpadding=grid::unit(0, "lines"),
                          y = 0.7, x = 0.3, size = 9, hjust = 0))
#Religiosidade
imagem0 <- ggdraw() + 
  #draw_label("Draft", colour = "#80404080", size = 40, angle = 45) + #Escrever sobre a imagem
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/dataset_bomdia_final/IMG-20200426-WA0119.jpg", scale = 0.9)
imagem0 <- ggdraw(add_sub(imagem0, "Religiosidade", vpadding=grid::unit(0, "lines"),
                          y = 0.7, x = 0.3, size = 9, hjust = 0))
# Criar lista para imagens a serem plotadas
plot_row <- plot_grid(imagem7, imagem8, imagem9, imagem0, ncol = 2)

# Adicionar título
title <- ggdraw() +
  draw_label(
    "Quadro 2: Valores em memes de Bom Dia",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme_ipsum()

# Adicionar legenda
caption <- ggdraw() +  
  theme_ipsum() +
  labs(caption = "Fonte: coLAB/UFF + Margem/UFMG")

# Plotar imagens com título e legenda
plot_grid(
  title, plot_row, caption,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)


