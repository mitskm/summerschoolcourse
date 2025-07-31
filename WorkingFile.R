#Загрузить датасет (массив данных) можно несколькими способами 
#Первый способ: установить working directory (путь в папку, откуда R возьмет файл) и при помощи read.csv загрузить
#
setwd("/Users/mitskm/Downloads") #Такой вариант подойдет для macos. Для Windows пишите с C и обратными слешами
flats <- read.csv('flats.csv', header = TRUE) #Загружаем датасет

#Второй способ: загрузить с GitHub. Гитхаб позволяет получть raw-ссылку, где кроме данных больше нет ничего. С нее можно загрузить данные
url_1 <-  "https://raw.githubusercontent.com/mitskm/summerschoolcourse/refs/heads/main/letters.csv"
letters <- read.csv(url_1, header = TRUE) #Обратим внимание, что read.csv принимает также URL на вход, а не только название файла
url_2 <- "https://raw.githubusercontent.com/mitskm/summerschoolcourse/refs/heads/main/Data_Causal.csv"

experiment <- read.csv(url_2, header = TRUE)
#Чтобы считать файл формата xlsx используйте read_excel()
#Третий способ (самый оптимальный): Загрузить файл через Import Dataset. Проинспектировать его (корректно ли считывается) и выбрать имя переменной. 
View(flats) #Смотрим файл 

#Для R Существует множество дополнений.
install.packages(c('estimatr', 'car')) #Установка нескольких дополнений
library(estimatr) #Подключение дополнения. Если вы этого не сделаете, то R не сможет работать с командами внутри этого дополнения
library(car)



#Подготовка данных
control     <- experiment$Response[experiment$Treatment == 0] #Созданием дамми-переменной для контрольной группы
treatment_1 <- experiment$Response[experiment$Treatment == 1]
treatment_2 <- experiment$Response[experiment$Treatment == 2]
experiment$Treatment <- as.factor(experiment$Treatment)#Факторизация переменной, чтобы R воспринимал ее как категориальную 
experiment$Bill <- as.factor(experiment$Bill) 
experiment$Position <- as.factor(experiment$Position)

#t-tests
t.test(control,treatment_1 ) #По умолчанию R делает Welch's t-test для двух независимых выборок с разной дисперсией. 
#Чтобы сделать Student's t-test используйте t.test(control,treatment_1, equal.var = TRUE )
t.test(control,treatment_2)
View(experiment)

#Регрессии с классическими ошибками (то есть ошибки имеют распределение N(0, sigma^2) )
model_1 <-  lm(Response ~ Treatment, data = experiment)
summary(model_1)
model_2 <-  lm(Response ~ Treatment + Bill + Position + tseen, data = experiment)
summary(model_2)

#Регрессия с кластеризованными ошибками (Поскольку у нас повторные наблюдения)
model_3 <- lm_robust(Response ~ Treatment + Bill + Position + tseen, data = experiment, clusters = experiment$ID)
round(summary(model_3)$coefficients, 4)


View(letters)

letters$Status = as.factor(letters$Status)
letters$First_Reviewer = as.factor(letters$First_Reviewer)
letters$Second_Reviewer = as.factor(letters$Second_Reviewer)

letters$Status <-  relevel(letters$Status, ref = "Иное") #Изменить референсную группу у фактора

#Модель с зависимой переменной в виде среднего балла по двумя проверяющим за мотивационку 
model_4 <- lm(Target ~ gender + Status + First_Reviewer + Second_Reviewer, data = letters)
summary(model_4)

#Зависимая переменная: балл первого проверяющего
model_5<- lm(First_Grade ~ gender + Status + First_Reviewer, data = letters)
summary(model_5)

#Зависмая переменная: балл второго проверяющего
model_6 <- lm(Second_Grade ~ gender + Status, data = letters)
summary(model_6)

#Безумная модель
model_7 <- lm(Target ~ First_Grade + Second_Grade, data = letters)
summary(model_7)