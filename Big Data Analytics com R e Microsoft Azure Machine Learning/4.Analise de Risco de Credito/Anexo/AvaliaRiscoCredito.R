# Risk analysis

# Set Directory
setwd("/home/oracy/Documents/DSA_Projetos/DSA_Projetos/Big Data Analytics com R e Microsoft Azure Machine Learning/4.Analise de Risco de Credito")
getwd()

# Loading my library
source('utils.R')

# Loading the dataset into a dataframe
df <- read.csv('credit_dataset.csv', header = TRUE, sep = ',')
head(df)
str(df)
# credit.rating - Credito aprovado ou nao
# account.balance - balanco conta bancaria 
# credit.duration.months - duracao em meses do credito
# previous.credit.payment. - Status do pagamento do credito anterior
# credit.purpose - Tipo de Credito proposto
# credit.amount - Quantidade de credito
# savings - Poupanca
# employment.duration - Duracao no emprego atual
# installment.rate - Taxa de parcelamento
# marital.status - Estato civil
# guarantor - Fiador
# residence.duration - Quanto tempo esta naquela residencia
# current.assets - Ativos correntes
# age - Idade do solicitante
# other.credits - Outros creditos
# apartment.type - Tipo do apartamento
# bank.credits - Outros Creditos bancarios
# occupation - Cargo que ele ocupa na empresa
# dependents - Quantos dependentes possue
# telephone - Possue telefone
# foreign.worker - Trabalhador estrangeiro

# Normalizing the variables
numeric.vars <- c("credit.duration.months", "age", "credit.amount")
df <- scale.features(df, numeric.vars)

# Factor type variables
factor.vars <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
                 'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                 'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                 'other.credits', 'apartment.type', 'bank.credits', 'occupation', 
                 'dependents', 'telephone', 'foreign.worker')
df <- to.factors(df, factor.vars)

head(df)
str(df)

# Dividing data in training and testing - 60:40 ratio
# Font: https://cran.r-project.org/web/packages/dataPreparation/vignettes/train_test_prep.html
nrow(df)
index <- sample(1:nrow(df), 0.6 * nrow(df))
df_train <- df[index,]
df_test <- df[-index,]
length(df_train)
length(df_test)

# Feature Selection
# Font: https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
# Font: https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
#install.packages('caret')
#install.packages('e1071', dependencies = TRUE)
#install.packages('randomForest')
library(caret)
library(randomForest)

# Running the function
rfe.results <- feature.selection(feature.vars = df_train[,-1],
                                 class.var = df_train[,1])

# Viewing Results
rfe.results
varImp(rfe.results)
predictors(rfe.results)
plot(rfe.results, type = c("g","o"))

# Creating and Evaluating the Model
#install.packages('ROCR')
library(ROCR)
source('plot_utils.r')

## separate feature and class variables
test.feature.var <- df_test[,-1]
test.class.var <- df_test[,1]

# Building a Logistic Regression Model
lm.init <- 'credit.rating ~ .'
lm.init <- as.formula(lm.init)
lr.model <- glm(formula = lm.init, data = df_train, family = "binomial")
  
# Viewing the template
summary(lr.model)
any(is.na(df))

# Testing the Model in Test Data
lr.predictions <- predict(lr.model, df_test, type = "response")
lr.predictions <- round(lr.predictions)

# Evaluating the model
confusionMatrix(table(data = lr.predictions, reference = test.class.var), positive = '1')

## Feature selection
formula <- 'credit.rating ~ .'
formula <- as.formula(formula)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula, data = df_train, method = "glm", trControl = control)
importance <- varImp(model, scale = FALSE)
plot(importance)

# Building the model with the selected variables
newFormula <- "credit.rating ~ account.balance + credit.purpose + previous.credit.payment.status + savings + credit.duration.months"
newFormula <- as.formula(newFormula)
lrNewModel <- glm(formula = newFormula, data = df_train, family = "binomial")

# Viewing the template
summary(lrNewModel)

# Testing the Model in Test Data
lrNewPrediction <- predict(lrNewModel, df_test, type = "response")
lrNewPrediction <- round(lrNewPrediction)

# Evaluating the model
confusionMatrix(table(data = lrNewPrediction, reference = test.class.var), positive = '1')

# Evaluating model performance

# Creating ROC Curves
lrModelBest <- lr.model
lrPredictionValue <- predict(lrModelBest, test.feature.var, type = "response")
predictions <- prediction(lrPredictionValue, test.class.var)
par(mfrow = c(1, 2))
plot.roc.curve(predictions, title.text = "ROC Curve")
plot.pr.curve(predictions, title.text = "Precision/Recall Curve")