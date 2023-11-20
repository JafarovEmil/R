# instal and activate package
library(tidyverse)

# "::" - hər hansı bir faylın və ya funksiyanın həmin package-dən götürülməsini əmr edir.
newdata <- nycflights13::flights

str(newdata)
newdata %>% str()


newdata %>% glimpse()


# newdata - datanın özü, filter isə sütünlarla işləmək üçün nəzərdə tutulub. 
# Bu nümünədə biz month sütünunun 11 və 12-yə bərabər olmağını istəyirik.
newdata %>% filter(month %in% c(11, 12)) %>% View()


# Filter zamanı nida işarəsi həmin ədədləri datadan çıxarmaq üçündür. 
newdata %>% 
  filter( !(arr_delay > 120), !( dep_delay > 120))


# datanı hər hansı bir sütuna görə azdan çoxa doğru düzmək üçün arrange-dən istifadə edirik, çoxdan aza edəndə isə
# arrange-dən sonra desc istifadə etməliyik.
newdata %>% arrange(arr_delay)

#coxdan aza
newdata %>% arrange(desc(arr_delay))


# select isə yalnız istədiyimiz sütunları seçməkdə köməklik edir.
newdata %>% select(year, month, day)

# ":" işarəsi diapazonu qeyd edir. Yəni year sütünu ilə day sütünun arasında yerləşən bütün sütünları daxil edir.
newdata %>% select(year:day)

# "-" işarəsi isə yenə bütün diapazonu qeyd edir və bu sütünları daxil etməyin əvəzinə, onları datadan çıxarır / silir.
newdata %>% select(-(year:day))

# rename() isə hər hansı sütunun adını dəyişməyə icazə verir. İlk növbədə yeni adı qeyd edirik, sonra isə dəyişmək
# istədiyimiz sütunu.
newdata %>% rename(ay = month)


# Bəzən sütnuların yerini dəyişmək lazım olur, bunun üçün istədiyiniz sütunları qeyd edirsiniz, sonra is everything, çünki
# everything bütün yerdə qalmış sütünlarıda daxil edir.
newdata %>% select(time_hour, air_time, everything())

# Bəzən datada 500-1000 sütün olur və onların arasında time sözü tərkibində olan sütünların sayı 100-lərlə ola bilər,
# bu zaman siz time yazaraq yalnız time olan sütünları seçirsiniz.
newdata %>% select(contains('time'))
newdata$carrier %>% unique() %>% length()

# "ends_with"  "contains" sözünə çox oxşayır, amma demək istəyir ki yalnız delay sözü ilə bitən sütünları seçsin.
newdata %>% select(year:day, 
                   ends_with("delay"), 
                   distance, 
                   air_time)


# bəzən əldə olan sütünları redaktə etmək lazım olur, bunun üçün mutate funksiyasından istifadə edirik,
# bu nümünədə yeni sütun yaradırıq gain və həmin sütunda dep_delay sütünunda olan ədələrdən arr_play sütununda olan
# ədədləri çıxmaq istəyirik.
newdata %>% mutate(
  gain = dep_delay - arr_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours) %>% 
  select(c(20:22))


# transmute isə datanı redaktə edəndən sonra yalnız redaktə olunmuş sütunları saxlayır.
newdata %>% transmute(
  gain = dep_delay - arr_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours)


# "summarise" "mutate" kimidir. Yeni sütun yaradır və "na.rm" isə orta ədəd hesablananda boş olan xanaları
# silir / nəzərə almır.
newdata %>% summarise(delay = mean(dep_delay, na.rm = TRUE))

newdata %>% 
  group_by(carrier) %>%
  summarise(flight = mean(flight,na.rm = T)) %>% 
  arrange(desc(flight))

newdata %>% count(carrier,dest) 

# Count sadəcə sütunun adıdır, n() isə bütün datda olan sətrləri toplayır, distance və delay üzrə ortalamanı hesablayır.
newdata %>% summarise(
  count = n(),
  dist = mean(distance, na.rm = TRUE),
  delay = mean(arr_delay, na.rm = TRUE))

# Yalnız boş olmayan xanaları hesablayır. (%)
newdata %>% complete.cases() %>% sum()/nrow(newdata)                 
nrow(newdata) ; ncol(newdata)


# Bütün sütunlarda olan boş xanaları datadan çıxarır. 
# Əgər yalnız istədiyiniz sütunda olan boş xanaları çıxarmaq istəyirsinizsə, bu zaman həmin sütunun(ların) adlarını qeyd etməlisiz. Məs drop_na(distance)
newdata %>% drop_na() %>% View()


# 1:2 yəni 1-ci hissə sətrlərin 2-ci hissə 1:3 isə sütünları qeyd edir. gather datanı yığır sütünlar üzrə.
# "-" işarəsi həmin ədədi yığmaqdan çıxarır.
newdata[1:3,1:3] %>% 
  gather(key = sutunlar, 
         value = verilenler,-year) %>% 
  as.data.frame()

# Sütun və sətrlərin yerlərini dəyişmək üçün t() istifadə etməliyik.
newdata[1:7,1:3] %>% t()

# Yeni sütunu add_column ilə də etmək olar.
newdata[1:5,1:6] %>% 
  mutate(countries = c('Turkey_France','Baku_Turkey','USA_France','Canada_Baku','Turkey_Baku'))

# Bəzən sütunu bir neçə sütuna bölmək lazım olur. bu zaman separate-dən istifadə edirik.
newdata[1:5,1:6] %>% 
  mutate(countries = c('Turkey_France','Baku_Turkey','USA_France','Canada_Baku','Turkey_Baku')) %>% 
  separate(countries,c('origin','destination'),sep = '_')

# Bəzən sütun(lar) üzrə unikal ədədləri seçərək onların digər sütun(lar) üzrə ortalamasını tapmalıyıq.
newdata %>% 
  group_by(dest) %>% 
  summarise(count = n(),
            dist = mean(distance, na.rm = TRUE),
            delay = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(count)) #%>% View()


# Datanı 'csv' formatında export etmək
newdata %>% write_csv('flights.csv')

# Datanı 'excel' formatında export etmək
library(writexl)
newdata %>% write_xlsx('flights.xlsx')


# 'csv' formatındakı faylı R a import etmək
df <- read_csv("flights.csv")

# 'excel' formatındakı faylı R a import etmək
library(readxl)
df <- read_xlsx("flights.xlsx")
#or
my_data <- read_excel("flights.xlsx")

#You can choose file on your computer :
my_data <- read_excel(file.choose())

# Specify sheet by its name
my_data <- read_excel("my_file.xlsx", sheet = "data")

# Specify sheet by its index
my_data <- read_excel("my_file.xlsx", sheet = 2)



library(dslabs)
gapminder %>% head() %>% as_tibble()
gapminder$region %>% unique()

gapminder %>%
  mutate(group = case_when(
    .$region %in% c("Southern Europe","Western Europe","Eastern Europe","Northern Europe") ~ "Europe",
    .$region %in% c("Western Asia","Eastern Asia","South-Eastern Asia","Southern Asia","Central Asia") ~ "Asia",
    .$region %in% c("Middle Africa","Western Africa","Southern Africa","Eastern Africa") ~ "Africa",
    .$region != "Northern Africa" & .$continent == "Africa" ~ "Sub-Saharan Africa",
    .$region %in% "Northern America" ~ "Northern America",
    .$region %in% c("Central America","South America","Caribbean") ~ "Latin America",
    .$region %in% "Australia and New Zealand"  ~ "Australia and New Zealand",
    .$region %in% c("Melanesia","Micronesia","Polynesia") ~ "Pacific Islands"))



#===============================================================================
#===============================================================================



# ADDITIONAL RESOURCES

install.packages('tidyr')
library(tidyr)

# Ozumuzden fake bir data yaradaq
comp <- c(1,1,1,2,2,2,3,3,3) 
yr <- c(1998,1999,2000,1998,1999,2000,1998,1999,2000) 
q1 <- runif(9, min=0, max=100) 
q2 <- runif(9, min=0, max=100) 
q3 <- runif(9, min=0, max=100) 
q4 <- runif(9, min=0, max=100) 

# Daha sonra onu data frame-e chevirek
df <- data.frame(comp=comp, 
                 year=yr, 
                 Qtr1 = q1, 
                 Qtr2 = q2, 
                 Qtr3 = q3, 
                 Qtr4 = q4)

# Datamiza baxaq
df


# Sutunlar setirlere chevrilir ve uyghun deyerler onlarin qarshisinda yazilir
new_df  <- df %>% gather(Quarter,Revenue,Qtr1:Qtr4)
new_df %>% head(4)

# Spread() funksiyasi ile evvelki veziyyetine qaytaririq
sp_new_df <- new_df %>% spread(Quarter,Revenue)
sp_new_df %>% head(4)


# Yeni bir data frame yaradiriq
data1 <- data.frame(x = c(NA, "a.x", "b.y", "c.z"))

# Baxaq
data1

# Daha sonra Separate() funksiyasi ile her bir setiri iki hisseye ayiririq 
# ve onlar yaratdigimiz muvafiq iki sutunlar arasinda bolushdurulur
data1 %>% separate(x, c("ABC", "XYZ"))


# Iki ayri sutundaki setirleri bir sutunda birleshdiririk
head(mtcars, 3)
mtcars %>%  unite("vs.am", c("vs","am"),sep = '.') %>% head(3)


