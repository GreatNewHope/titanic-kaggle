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

#Otro feature que hay que limpiar es el de la edad. Se va a poner en este caso la mediana (mala elección pero es lo más
#fácil)
age.median = median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), 'Age'] = age.median

#Exactamente igual que con la edad. Fíjate que todo esto es un error brutal. Estamos asignando valores de tarifa igual a
#la mediana sin preguntarnos a quién se lo estamos asignando. Esto quiere decir que le estaremos poniendo tarifas súper
#bajas a los ricos y tarifas ligeramente altas a los pobres. Se debe hacer un estimador, que teniendo en cuenta el resto
#de features de un individuo, deduzca los valores de la feature que está sin asignar. Puede ser un regression model o un
#randomForest
fare.median = median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), 'Fare'] = fare.median

#categorical casting. Quizás debería hacerse además de esto una categoría para hermanos y esposa (SibSp) y otra para padres con
#hijos (Parch)
titanic.full$Pclass = as.factor(titanic.full$Pclass)
titanic.full$Sex = as.factor(titanic.full$Sex)
titanic.full$Embarked = as.factor(titanic.full$Embarked)

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
write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)

#Ahora hay que hacer la entrega del modelo. Te vas a Kaggle y le das a Make a Submission