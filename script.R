library(readr)
library(dplyr)
loans <- read_csv("~/Escritorio/SIGE/practica/LoanStats_2017Q4.csv",
    skip = 1)
View(loans)

#Previews of loans
sapply(loans[1, ], class)

# Normalize class variable

class_variable = sapply(loans$loan_status, function(column_value) {

    if(column_value %in% c("Current")){
        "Paid"
    }else if(column_value %in% c("Late (31-120 days)", "Late (16-30 days)", "In Grace Period", "Charged Off")){
        "Not_paid"
    }else{
        "To be removed"
    }
})

loans$loan_status <- class_variable

## 1. Eliminación de instancias pertenecientes a clases no relevantes- Now we remove the row per column value "To be removed"
loans <- loans[loans$loan_status != "To be removed", ]


# Attributes / Column names
names(loans)

# Dimensions
dim(loans)

# Check missing values
column_names = names(loans)

for (i in 1:length(column_names)) {
    col = column_names[i]
    null_values = sum((is.na(loans[col])))
    msg = sprintf("column %s has %d missing values", col, null_values)
    print(msg)
}


# Remove columns that have lots of missing values
total_rows = dim(loans)[1]
quarter = total_rows/4.0
half = total_rows/2.0
threeQuarters = (total_rows*3.0)/4

columns_to_remove <- c()

for (i in 1:length(column_names)) {
    col = column_names[i]
    null_values = sum((is.na(loans[col])))

    if(null_values >= threeQuarters){
      msg = sprintf("column %s has at least 75%% missing values", col)
      print(msg)

      columns_to_remove <- c(i, columns_to_remove)

    }else if(null_values >= half){
      msg = sprintf("column %s has at least 50%% missing values", col)
      print(msg)

      columns_to_remove <- c(i, columns_to_remove)
    }else if(null_values >= quarter){
      msg = sprintf("column %s has at least 25%% missing values", col)
      print(msg)
    }
}

loans = loans[,-c(columns_to_remove)]
dim(loans)
View(loans)



# 3. Eliminación de variables sin información: Finding not relevant values

Vemos que las 3 primeras columnas parecen iguales pero al aplicar el identical vemos que las dos que son iguales son las dos primeras:
identical(loans$loan_amnt, loans$funded_amnt)
Returns -> True
identical(loans$funded_amnt, loans$funded_amnt_inv)
Returns -> False

Asi que sabiendo que las dos primeras son iguales pasamos a eliminar una de ellas:
loans <- subset(loans, select= -funded_amnt)

Ahora pasamos a explorar el contenido de las siguientes columnas y procedemos a eliminar las siguientes ya que no aportan informacion
Valores muy desiqlibrados: eg: S->114600 N->3 ó  0->114690, 4->1, 13->2, 26->1
- table(loans$hardship_flag): Donde sale que todas las filas no tienen este plan en su prestamos y solo un caso si N=114804      Y=1
  loans$hardship_flag <- NULL

- table(loans$debt_settlement_flag): Ocurre lo mismo que con el anterior y recibimos el mismo resultado
  ELiminamos haciendo:
  loans$debt_settlement_flag <- NULL

- num_tl_30dpd: 0->114756     1->46      2->3 :Número de cuentas actualmente con 30 días de retraso (actualizado en los últimos 2 meses)
  loans$num_tl_30dpd <- NULL

- num_tl_120dpd_2m
  loans$num_tl_120dpd_2m <- NULL

- delinq_amnt
  loans$delinq_amnt <- NULL

- chargeoff_within_12_mths: table(loans$chargeoff_within_12_mths)
             0      1      2      3      4      5
        113972    780     40      5      7      1
  loans$chargeoff_within_12_mths <- NULL

- acc_now_delinq:
            0      1      2      3
       114727     74      3      1
  loans$acc_now_delinq <- NULL

- table(loans$next_pymnt_d)
  Apr-2018 Feb-2018 Mar-2018
      21       22   114734
  loans$next_pymnt_d <- NULL

- table(loans$recoveries)
       0   2970
  114804      1
  loans$recoveries <- NULL

- table(loans$total_rec_late_fee)
       0     15  15.04  15.06
  114350    166      1     2
  loans$total_rec_late_fee <- NULL

- table(loans$pymnt_plan)
       n      y
  114804      1
  loans$pymnt_plan <- NULL



Encontramos que el siguiente valor solo tiene un único valor en todas sus columnas:
- table(loans$policy_code)
           1
      114805
  loans$policy_code <- NULL
- table(loans$collection_recovery_fee)
           0
      114805
  loans$collection_recovery_fee <- NULL


dim(loans)
[1] 114805     88


2. Transformacion
Pasamos a transformar los valores de la columna "title" para compararla con los de la columna "purpose".
- table(loans$title)
            Business             Car financing          Credit card refinancing
                1455                      1311                            24677
  Debt consolidation                Green loan                      Home buying
               60567                        72                             1345
    Home improvement     Learning and training                   Major purchase
                8341                         1                             3094
    Medical expenses      Moving and relocation                           Other
                1872                        862                           10387
            Vacation
                 821
- table(loans$purpose)
             car        credit_card  debt_consolidation        educational
            1311              24676               60568                  1
home_improvement              house      major_purchase            medical
            8341               1345                3093               1873
          moving              other    renewable_energy     small_business
             862              10387                  72               1455
        vacation
             821

- Comparamos las columnas "title" y "purpose":
Seleccionamos las dos columnas:
title_purpose <- subset(loans, select=c("title", "purpose"))
Ahora mostramos el numero de elementos distintos que tienen ambas columnas:
length(unique(title_purpose$title))
[1] 13
length(unique(title_purpose$purpose))
[1] 13
A continuacion mostramos las graficas de sectores de ambos para compararlos
with(title_purpose, pie(table(title), labels=levels(title), xlab="", ylab="",  main="title", col=rainbow_hcl(13)))
with(title_purpose, pie(table(purpose), labels=levels(purpose), xlab="",
  ylab="", main="purpose", col=rainbow_hcl(13)))

Tambien mostramos la tabla donde tambien podemos apreciar claramente que se corresponden los valores:
purpose
title                       car credit_card debt_consolidation educational
Business                    0           0                  0           0
Car financing            1311           0                  0           0
Credit card refinancing     0       24676                  1           0
Debt consolidation          0           0              60567           0
Green loan                  0           0                  0           0
Home buying                 0           0                  0           0
Home improvement            0           0                  0           0
Learning and training       0           0                  0           1
Major purchase              0           0                  0           0
Medical expenses            0           0                  0           0
Moving and relocation       0           0                  0           0
Other                       0           0                  0           0
Vacation                    0           0                  0           0
 purpose
title                     home_improvement house major_purchase medical moving
Business                               0     0              0       0      0
Car financing                          0     0              0       0      0
Credit card refinancing                0     0              0       0      0
Debt consolidation                     0     0              0       0      0
Green loan                             0     0              0       0      0
Home buying                            0  1345              0       0      0
Home improvement                    8341     0              0       0      0
Learning and training                  0     0              0       0      0
Major purchase                         0     0           3093       1      0
Medical expenses                       0     0              0    1872      0
Moving and relocation                  0     0              0       0    862
Other                                  0     0              0       0      0
Vacation                               0     0              0       0      0
 purpose
title                     other renewable_energy small_business vacation
Business                    0                0           1455        0
Car financing               0                0              0        0
Credit card refinancing     0                0              0        0
Debt consolidation          0                0              0        0
Green loan                  0               72              0        0
Home buying                 0                0              0        0
Home improvement            0                0              0        0
Learning and training       0                0              0        0
Major purchase              0                0              0        0
Medical expenses            0                0              0        0
Moving and relocation       0                0              0        0
Other                   10387                0              0        0
Vacation                    0                0              0      821



Por lo que comparandolas podemos ver que se corresponden bastante bien.
Aun asi, sabiendo ya que son la misma columna para estar mas seguros vamos a transformar los nombres de la columna tittle a los correspondientes nombres de la columna title_purpose.



for (i in 1:length(loans$title)) {
   if(loans$title[i]=="Debt consolidation"){
      loans$title[i] <- "debt_consolidation"
   }else if(loans$title[i]=="Business"){
      loans$title[i] <- "small_business"
  }else if(loans$title[i]=="Car financing"){
      loans$title[i] <- "car"
  }else if(loans$title[i]=="Credit card refinancing"){
      loans$title[i] <- "credit_card"
  }else if(loans$title[i]=="Green loan"){
      loans$title[i] <- "renewable_energy"
  }else if(loans$title[i]=="Home buying"){
      loans$title[i] <- "house"
  }else if(loans$title[i]=="Home improvement"){
      loans$title[i] <- "home_improvement"
  }else if(loans$title[i]=="Learning and training"){
      loans$title[i] <- "educational"
  }else if(loans$title[i]=="Major purchase"){
      loans$title[i] <- "major_purchase"
  }else if(loans$title[i]=="Medical expenses"){
      loans$title[i] <- "medical"
  }else if(loans$title[i]=="Moving and relocation"){
      loans$title[i] <- "moving"
  }else if(loans$title[i]=="Other"){
      loans$title[i] <- "other"
  }else if(loans$title[i]=="Vacation"){
      loans$title[i] <- "vacation"
  }
}

Ahora eliminamos la columna title ya que solo se diferencia de en 2 valores con la columna purpose y no estan escritos bien los nombres para su tratamiento.
loans$title <- NULL
> dim(loans)
[1] 114805     87


### Correlacion por cada par de variables
# Almacenamos las columnas cuyos valores son continuos
numeric_columns = cols[lapply(loans, typeof) != "character"]

# Almacenamos las columnas cuyos valores son categoricos
character_columns = cols[lapply(loans, typeof) == "character"]

# Recorremos por cada par de valores para ver su correlacion
correlation_array <- c("i","j","cor_coef")
total_columns = length(numeric_columns)
li = c()
col1 = c()
col2 = c()
coefi = c()
for(i in 1:total_columns){
    j <- i+1
    while(j <= total_columns){
        #print(sprintf("i = %s, j = %s", numeric_columns[i], numeric_columns[j]))
        coef <- cor(loans[numeric_columns[i]],loans[numeric_columns[j]],method="pearson")
        each_column <- c(numeric_columns[i],numeric_columns[j], as.numeric(coef))
        li = c(li,each_column)
        j <- j+1
        #print(test)

        col1 <- c(col1, numeric_columns[i])
        col2 <- c(col2, numeric_columns[j])

        if(is.na(coef)){
          coefi <- c(coefi, 0)
        }else{
          coefi <- c(coefi, coef)
        }

    }
}

pairs <- data.frame(col1, col2, coefi)

pairs = pairs[pairs$coefi != 0,]

library(data.table)
pairs = setorder(setDT(pairs), -"coefi")
View(pairs)

# Procedemos a eliminar las variables segun el coeficiente de correlacion
pairs_to_remove <- c()

for (i in pairs$coefi){
  if(i >= 0.95){
    pairs_to_remove <- c(i, pairs_to_remove)
  }
}
pairs_to_remove

#The pairs to remove are: installment, annual_inc, dti, delinq_2yrs, inq_last_6mths, open_acc, pub_rec, revol_bal, total_acc

dim(loans)
loans3$installment <- NULL
loans3$annual_inc <- NULL
loans3$dti <- NULL
loans3$delinq_2yrs <- NULL
loans3$inq_last_6mths <- NULL
loans3$open_acc <- NULL
loans3$pub_rec <- NULL
loans3$revol_bal <- NULL
loans3$total_acc <- NULL
#Should be 78

###### OTHER
# Creamos ds 2 copiando los elementos de loans
loans2 <- loans

for(i in names(loans2)){
  for(j in character_columns){
    if(i==j){
      loans2[i] <- NULL
    }
  }
}

# Obtenemos los coeficientes de correlacion entre todas las variables
val_cor <- cor(loans2)
#####


# Decision tree
library(rpart)
fit <- rpart(loans$loan_status ~ ., data = loans, method = "class", control = list(maxdepth = 5))
plot(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Printing the tree
fancyRpartPlot(fit)

summary(fit)

#Variable importance 6
#       emp_title     last_pymnt_d  last_pymnt_amnt    total_rec_int      total_pymnt  total_pymnt_inv
#              31               15               10               10                9                9
# total_rec_prncp       revol_util         zip_code earliest_cr_line
#               9                3                1                1

#Variable importance 10
#       emp_title       revol_util     last_pymnt_d         zip_code earliest_cr_line  last_pymnt_amnt
#              27               11               11                9                9                6
#   total_rec_int      total_pymnt  total_pymnt_inv  total_rec_prncp         int_rate       addr_state
#               5                5                5                5                2                1
#       sub_grade       emp_length
#               1                1


# Random Forest
library(randomForest);

# Imputacion con rfImpute del package randomForest
loans3.na <- loans
loans3.imputed <- rfImpute(loans3$loan_status ~ ., loans3.na)

# Eliminamos filas de casos con NA
loans5 <- loans4
loans5 <- loans4[complete.cases(loans4), ]

dim(loans4)
#[1] 114805     78

dim(loans5)
#[1] 73002    78



library(dplyr)
loans6=loans5 %>% mutate_if(is.character, as.factor)

# Nos quedamos con los atributos numericos
nums <- unlist(lapply(loans6, is.numeric))
loans6.numeric = loans6[, nums]
tmp <- cor(loans6.numeric)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0

#Alta Correlacion
loans6.important.numeric <- loans6.numeric[,!apply(tmp,2,function(x) any(x > 0.99))]
head(loans6.important.numeric)
names(loans6.important.numeric)

# Añado la columna loan_status a loans6.important.numeric antes de aplicar random Forest
loans6.important.numeric$loan_status <- loans6$loan_status

# Random fores para quedarnos con las variables numericas mas importantes
rf.model <- randomForest(loans6.important.numeric$loan_status ~.,
            data = loans6.important.numeric,
            ntree = 35,
            type="classification",
            importance=TRUE,
            na.action=na.omit

# Visualizamos las variables mas importantes
varImpPlot(rf.model)








#################################################################################################################
########
table(class_variable)
pie(table(class_variable))
barplot(table(loans$loan_amnt), main = "tittle")
########

##Para tener todos los valores de una columna
t <- table(loans$loan_status)

####
coefi <- c()
col1 <- c()
col2 <- c()
col1 <- c(col1, numeric_columns[i])
col2 <- c(col2, numeric_columns[j])
if(is.na(coef)){
  coefi <- c(coefi, 0)
}else{
coefi <- c(coefi, coef)
}
pairs <- data.frame(col1, col2, coefi)
pairs = pairs[pairs$coefi != 0,]
library(data.table)
pairs = setorder(setDT(pairs), -"coefi")
