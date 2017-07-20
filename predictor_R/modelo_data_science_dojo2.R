setwd("C:/Users/Marcos/Downloads/titanic-competition")

library(randomForest)

"Le falta hacer parameter shooting, cross validation y 70-30 split a este modelo (se debería hacer justo cuando creas el randomForest).
Este código está en github.com/datasciencedojo/bootcamp en homework answers"

#Pones stringasfactors en FALSE porque quieres combinar estos dataframes y para combinarlos
#tienes que alinear sus factores. Pon header=TRUE a pesar de que es redundante porque es buena
#costumbre
titanic.train = read.csv("train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test = read.csv("test.csv", stringsAsFactors = FALSE, header = TRUE)

#Antes de unirlos debes crear una feature que diga si pertenece al trainset o no
titanic.train$isTrainSet = TRUE
titanic.test$isTrainSet = FALSE

#Antes de combinarlos, debes hacer que ambos dataframes tengan los mismos features
#y en el test set no hay feature de survived. Puedes darte cuenta de si llevan los
#mismos features haciendo names(titanic.test) y names(titanic.train), no importa el
#orden en el que te salgan los features, lo único que importa es que tengan EXACTAMENTE
#el mismo nombre
titanic.test$Survived = NA

#Es ahora cuando unimos los dos dataframes. Tenemos que hacerlo con un rbind (rowbind), no con un cbind (columnbind).
#Puedes ver que se ha hecho bien porque si haces un table(titanic.full$isTrainSet) te van a salir que se corresponde
#con los valores de los dataframes unidos
titanic.full = rbind(titanic.test , titanic.train)

#Hay ciertos features que tienen valores sin asignar. Tenemos que llenar esos valores de alguna manera (en este caso
#se va a hacer poniendo la mediana de los valores, pero en casos mejores debes crear un estimador más preciso)
#En el caso de la feature embarked se va a poner el valor más habitual. Es lo más fácil
titanic.full[titanic.full$Embarked == '', "Embarked"] = 'S'

titanic.full$Pclass = as.factor(titanic.full$Pclass)
titanic.full$Sex = as.factor(titanic.full$Sex)
titanic.full$Embarked = as.factor(titanic.full$Embarked)

#Hay dos tipos de linear regression models. Esta el mínimos cuadrados y está otro.
#Se elige hacerlo con el mínimos cuadrados. Este método tiene el problema de que es sensible a los outliers. Por eso, se retiran
#En la otra sección se hizo la limpieza poniendo los valores de la mediana en los valores no asignados. Esto está fatal
#por las cuestiones mencionadas anteriormente (asignar valores muy bajos a gente rica etc, quizás las mujeres de tercera
#clase pagan más, quizás las de primera pagan menos y los hombres de primera pagan más. Quizás los que llevan niños pagan
#menos por algún tipo de oferta, lo que sea).
#Crearemos un modelo predictor que prediga los valores de forma parecida a como hicimos con Survived

#Como queremos quitar los outliers, quitamos los que van más allá del cuarto cuartil.También habría que quitar outliers por debajo
#pero en este caso no hay outliers por debajo porque es cero.
upper.whisker.Age = boxplot.stats(titanic.full$Age)$stats[5]
outlier.filter.Age = titanic.full$Age < upper.whisker.Age

#Ahora haremos un modelo predictor por regresión para deducir edad en lugar de supervivencia
age.equation = "Age ~ Pclass + Sex + SibSp + Parch + Embarked"
age.formula = as.formula(age.equation)

age.model = lm(formula = age.formula, data = titanic.full[outlier.filter.Age,])
age.row = titanic.full[is.na(titanic.full$Age), c("Pclass", "Sex", "SibSp", "Parch", "Embarked")]
age.predictions = predict(age.model, newdata = age.row)

#Ahora se hace el predictor para la tasa de billete
upper.whisker.Fare= boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter.Fare = titanic.full$Fare < upper.whisker.Fare

fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.formula = as.formula(fare.equation)

fare.model = lm(formula = fare.formula, data = titanic.full[outlier.filter.Fare,])
fare.row = titanic.full[is.na(titanic.full$Fare),c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")]
fare.predictions = predict(fare.model, newdata = fare.row)

#Ahora toca volcar estas predicciones de vuelta en el full
titanic.full[is.na(titanic.full$Age),"Age"] = age.predictions
titanic.full[is.na(titanic.full$Fare),"Fare"] = fare.predictions

#Volcamos otra vez los dataframes en las variables de titanic para asignarle los valores que estaban sin asignar y sus factors
titanic.train = titanic.full[titanic.full$isTrainSet == TRUE,]
titanic.test = titanic.full[titanic.full$isTrainSet == FALSE,]

#se hace una categoría binaria para Survived
titanic.train$Survived = as.factor(titanic.train$Survived)

#Ahora es cuando creamos el modelo predictor. Primero hay que hacer la ecuación que utilizará el randomForest (predice Survived en función
# de Pclass + Sex + Age + SibSp + Parch + Fare + Embarked)
survived.equation = "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula = as.formula(survived.equation)

#Ahora se hace el modelo predictor. Conviene mirarse la teoría de forests para entender lo que son los nodos y todoo eso.
titanic.model = randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.train))

#Ahora es cuando aplicamos la predicción
Survived = predict(titanic.model, newdata = titanic.test)

#Las siguientes líneas son simplemente crear el dataframe que se entrega en Kaggle. Hay que entregarlo como un csv de dos columnas,
# PassengerId y si sobrevivió o no
PassengerId = titanic.test$PassengerId
output.df = as.data.frame(PassengerId)
output.df$Survived = Survived
#Se pone row.names en FALSE porque no queremos que nuestro dataframe tenga la numeración en las filas
write.csv(output.df, file = "kaggle_submission2.csv", row.names = FALSE)

#Ahora hay que hacer la entrega del modelo. Te vas a Kaggle y le das a Make a Submission