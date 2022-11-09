library(readr) # Carregar arquivos csv
library(tidyverse) # Pacotes do Tidyverse
library(useful) # Imputação de média/moda/valor mais frequente
library(reshape2) # Múltiplos Boxplots
library(corrplot) # Gráfico de correlação
library(caret) # Train test split

spaceship_train <- read_csv("G:/Meu Drive/Aulas-Ciencia-de-Dados-com-R/exercicio_aula/space_ship/train.csv")

# ANÁLISE EXPLORATÓRIA:

# Tipo de cada coluna
str(spaceship_train)

# Tamanho do conjunto de dados
dim(spaceship_train) # 8693 x 14

# Remoção de colunas inúteis:
spaceship_train <- spaceship_train %>% select(-PassengerId, -Name)

# Extração de informação da coluna Cabin:
spaceship_train <- spaceship_train %>%
                   separate(Cabin, sep = "/", remove = TRUE, 
                            into = c('Deck','Num','Side'))

# Transformação de dados categóricos em fatores:
cols <- c('HomePlanet', 'Deck', 'Side', 'Destination')
spaceship_train[cols] <- lapply(spaceship_train[cols], factor)

# Análise dos fatores:
spaceship_train %>% 
  select_if(is.factor) 

# Quantidade de dados em cada fator:
table(spaceship_train$HomePlanet)
table(spaceship_train$Deck)
table(spaceship_train$Side)
table(spaceship_train$Destination)

# Remoção das colunas Deck e Num:
spaceship_train <- spaceship_train %>% select(-Deck, -Num)

# QUANTOS DADOS FALTANTES EXISTEM?

# Quantidade de valores NA
colSums(is.na(spaceship_train))

# Retirada de todos os valores NA: Retirada de 2000 linhas
spaceship_train %>% 
  drop_na() %>%
  dim()

spaceship_train <- spaceship_train %>% drop_na()

# ANÁLISE DE CORRELAÇÃO ----

M <- spaceship_train %>%
  select_if(is.numeric) %>% cor()

corrplot(M, type = 'lower', 
         method = 'number', tl.col = 'black',
         tl.srt = 45, # Rotaciona labels de cima
         tl.cex = 0.8,
         number.cex = 0.65 , # Diminui o tamanho dos números
         col = COL2('PuOr', 10))

# AJUSTE DO MODELO:
sample <- createDataPartition(spaceship_train$Transported, p = .7, 
                              list = FALSE)
train <- spaceship_train[sample, ]
test <- spaceship_train[-sample, ]

# GENERALIZED LINEAR MODELS
spaceship_rg <- glm(Transported ~ ., data = train,
                    family = binomial)
# Análise do Modelo:
summary(spaceship_rg)
# Realização de previsões:
y_pred <- predict(spaceship_rg, test, type = 'response')
y_pred <- factor(ifelse(y_pred > 0.5, TRUE, FALSE))
y_true <- factor(test$Transported)

confusionMatrix(data = y_pred, reference = y_true)
# ACURÁCIA: Porcentagem de valores corretamente previstos
# PRECISÃO: Dentre as classificações positivas, quantas estão corretas?
860/(860 + 235)
# RECALL: Dentre TODOS os eventos positivos, quantos estão corretos?
860/(860 + 160)


# REALIZANDO PREVISÕES NO CONJUNTO test:
spaceship_test <- read_csv("G:/Meu Drive/Aulas-Ciencia-de-Dados-com-R/exercicio_aula/space_ship/test.csv")
test_id <- spaceship_test %>% select(PassengerId)
spaceship_test <- spaceship_test %>% select(-Name, -PassengerId)
spaceship_test <- spaceship_test %>%
  separate(Cabin, sep = "/", remove = TRUE, 
           into = c('Deck','Num','Side'))
# Transformação de dados categóricos em fatores
cols <- c('HomePlanet', 'Deck', 'Side', 'Destination')
spaceship_test[cols] <- lapply(spaceship_test[cols], factor)
spaceship_test <- spaceship_test %>% select(-Deck, -Num)
# Existem valores faltantes?
colSums(is.na(spaceship_test))
# O ajuste dos numéricos será feito com base na média ou mediana?
spaceship_test %>%
  select_if(is.numeric) %>%
  drop_na() %>%
  melt() %>%
  drop_na() %>%
  ggplot(aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 0.5,
                                   vjust = 0.5))

# Média das colunas numéricas:
spaceship_test %>%
  select_if(is.numeric) %>%
  colMeans(na.rm = TRUE)

numeric_cols <- c('Age', 'RoomService', 'FoodCourt', 'ShoppingMall',
                  'Spa', 'VRDeck')
categorical_cols <- c('HomePlanet', 'CryoSleep', 'Side', 'Destination', 'VIP')

# Função auxiliar:
most_frequent <- function(x){
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

spaceship_test[numeric_cols] <- simple.impute(spaceship_test[numeric_cols], median)
spaceship_test[categorical_cols] <- simple.impute(spaceship_test[categorical_cols], most_frequent)

# A imputação de fato aconteceu?
colSums(is.na(spaceship_test))

# Realiza a previsão
y_pred <- predict(spaceship_rg, spaceship_test, type = 'response')
y_pred <- ifelse(y_pred > 0.5, TRUE, FALSE)
# Vamos criar o arquivo csv para submissão no Kaggle:
submission = data.frame('PassengerId' = test_id,
                        'Transported' = str_to_title(y_pred))

write.csv(submission, file = "submission.csv", 
          row.names = FALSE,
          quote = FALSE) # Remove quote no csv






