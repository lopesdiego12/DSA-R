# Preventing Hospital Expenses

# Set Directory
setwd("C:\\Users\\Oracy\\Desktop\\DSA_Projetos\\DSA_Projetos\\Big Data Analytics com R e Microsoft Azure Machine Learning\\3.Prevendo Despesas Hospitalares")
getwd()

# For this analysis, we will use a data set simulating hypothetical medical expenses
# for a set of patients spread across 4 regions of Brazil.
# This dataset has 1,338 observations and 7 variables.

# Step 1 - Data gathering
df <- read.csv('despesas.csv', header = TRUE,sep = ',')
head(df)

# Step 2: Exploring and Preparing the Data
# Viewing variables
str(df)

# Central Trend Averages of the variable spending
summary(df[c("gastos")])

# Building a Histogram
hist(df$gastos, main = 'Histogram', xlab = 'Spending')

# Regions contingency table
table(df$regiao)

# Exploring relationships among variables: Correlation Matrix
cor(df[c("idade","bmi", "filhos", "gastos")])

# None of the correlations in the matrix are considered strong, but there are some interesting associations.
# For example, age and bmi (BMI) appear to have a weak positive correlation, which means that
# As age increases, body mass tends to increase. There is also a positive correlation
# Moderate between age and expenditure, in addition to the number of children and expenses. These associations
# that as the average age, body mass and number of children increases, the expected cost of health insurance goes up.

# Viewing relationship between variables: Scatterplot
# Note that there is no clear relationship between the variables
pairs(df[c("idade", "bmi", "filhos", "gastos")])

# Scatterplot Matrix
# Font: http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs#use-the-r-package-psych
#install.packages ("psych")
library(psych)
pairs.panels(df[c("idade", "bmi", "filhos", "gastos")], method = "pearson", # correlation method
             density = TRUE, # show density plots
             ellipses = TRUE # show correlation ellipses
             )

# This graphic provides more information about the relationship between variables

## Step 3: Training the Model
# Font: https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/lm
str(df)
model <- lm(gastos ~ idade + filhos + bmi + sexo + fumante + regiao, df)
  
# Similar to the previous item
model_2 <- lm(gastos ~ ., df) # "." is the same as type all variables

# Viewing the coefficients
# Font: https://stackoverflow.com/questions/6577058/extract-regression-coefficient-values
model_summary <- summary(model)
model_summary
model

# Preventing medical expenses
# Font: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/predict.lm.html
predicting <- predict(model)
class(predicting)
head(predicting)

# Step 4: Evaluating Model Performance
# More details about the model
# Font: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html
summary(model)

# Step 5: Optimizing Model Performance

# Adding a variable with twice the age value
df$idade2 <- df$idade ^ 2
df$idade2
df$idade

# Adding a Bookmark to BMI> = 30
# Font: https://www.datamentor.io/r-programming/ifelse-function/
df$bmi30 <- ifelse(df$bmi >= 30, 1, 0)
df$bmi30

# Creating the final template
str(df)
model_3 <- lm(gastos ~ idade + idade2 + sexo + filhos + bmi30 * fumante + regiao, df)

summary(model_3)