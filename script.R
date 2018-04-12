library(readr)
library(dplyr)
loans <- read_csv("~/Escritorio/SIGE/practica/LoanStats_2017Q4.csv",
    skip = 1)
View(loans)



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

Vemos que las 3 primeras columnas parecen iguales pero al aplicar el iquals vemos que las dos que son iguales son las dos primeras:
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



Encontramos que el siguiente valor solo tiene un único valor en todas sus columnas:
- table(loans$policy_code)
           1
      114805
  loans$policy_code <- NULL
- table(loans$collection_recovery_fee)
           0
      114805
  loans$collection_recovery_fee <- NULL


pie(table(loans$tax_liens), title="test")
table(table(loans$tax_liens))






#################################################################################################################
########
table(class_variable)
pie(table(class_variable))
barplot(table(loans$loan_amnt), main = "tittle")
########

##Para tener todos los valores de una columna
t <- table(loans$loan_status)
