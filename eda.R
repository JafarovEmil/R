# remotes::install_github("jbkunst/highcharter")
# devtools::install_github("laresbernardo/lares")

library(tidyverse)
library(data.table)
library(glue)
library(rlang)
 

# fread() funksiyası daha böyük həcimli dataları sürətli oxuması üçün read.csv()-nin alternatividir.
# Datanı daxil edərkən Working directory-i dəyişmək əvəzinə faylın yerləşdiyi qovluğun adını daxil edə bilərik.
# Note: R backslash (\) işarəsini oxuya bilmədiyindən onu slash (/) ilə əvəzləməliyik.
dataset <- fread('C:/Users/Admin/Desktop/Bootcamp-R/week 3/african_names.csv')

dataset %>% glimpse()


# Datanı skan etməsi üçün skim() funksiyası istifadə olunur.
library(skimr)
dataset %>% skim()


library(inspectdf)
dataset %>% inspect_na()
# inspect_cor() funksiyası sadəcə numeric dəyişənlər arasındakı əlaqəni ölçmək üçün istifadə olunur.
dataset %>% inspect_cor() %>% show_plot()
# Sadəcə bir sütun üzrə korelyasiyanın ölçülməsi.
dataset %>% inspect_cor(with_col = "age") %>% show_plot()


# Sütunların NA-lərə görə kəsişmələrinin vizuallaşdırılması
library(naniar)
dataset %>%
  #select(gender, age, height, year_arrival) %>%
  gg_miss_upset()


library(DataExplorer)
dataset %>% plot_intro()
dataset %>% profile_missing()
dataset %>% plot_missing()
dataset %>% plot_missing(missing_only = T)
dataset %>% plot_bar()
dataset %>% plot_correlation(type = "continuous")	
dataset %>% plot_histogram()
# dataset %>% create_report()


dataset %>% 
  mutate_if(is.character, as.factor) %>% # Əgər sütun character-sə, faktor data tipinə çevir.
  mutate_if(is.factor, as.numeric) %>% # Əgər sütun faktor-sa, numeric data tipinə çevir.
  gather() %>%
  ggplot(aes(x = value, group = key)) +
  geom_histogram(fill = "green", color = "darkgreen",bins = 30) +
  facet_wrap(~ key, ncol = 4, scale = "free")
options(scipen = 999) # disabling scientific notation


library(lares)
dataset %>% 
  corr_cross(rm.na = T,
             method = "spearman")

dataset %>% 
  corr_var(age,
           method = "spearman")


# cor() funksiyası NA dəyişənləri ilə işləyə bilmir. Əvvəlcədən ya NA dəyişənləri datadan çıxarılmalı 
# (na.omit()), ya da funksiya daxilində NA-ləri nəzərə almaması bildirilməlidir. 
# funksiya daxilində naşrm = T işlədilə bilmədiyindən use="complete.obs" daxil edilir.
library(highcharter)
dataset %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate_if(is.factor,as.numeric) %>% # na.omit() %>% 
  cor(use="complete.obs") %>% round(2) %>% # use="complete.obs"
  hchart(label = T)


library(dlookr)
dataset %>% 
  diagnose_report(output_format = "html", browse = T)


library(esquisse)
dataset %>% esquisser()

