# Dados em Painel
install.packages(plm)
library(plm)
#------------------------------------------ 
data("EmplUK", package = "plm")
data("Wages", package = "plm")
data("Grunfeld", package = "plm")
data("Produc", package = "plm")
#--------------------------------------------
emp=EmplUK[,4]
wage= EmplUK[,5]
capital =EmplUK[,6]
output=EmplUK[,7]
#---------------------------------------------------------
lag(log(emp), 1) 
lag(log(wage), 3)
diff(log(capital), 2)
#--------------------------------------------------
formula1 = log(emp) ~ lag(log(emp), 1) + lag(log(emp), 2) + lag(log(wage), 2) + lag(log(wage), 3) +
  diff(log(capital), 2) + diff(log(capital), 3)
Emp.mod1 <- plm(formul = formula1, data = EmplUK, model = "within")
summary(Emp.mod1)
#--------------------------------------------------
  formula2 <- log(emp) ~ lag(log(emp), 1) + lag(log(emp), 2) + lag(log(wage), 2) + lag(log(wage), 3) +
  I(log(capital) - lag(log(capital), 2)) + I(log(capital) - lag(log(capital), 3))
Emp.mod2 <- plm(formul = formula2, data = EmplUK, model = "within")
summary(Emp.mod2)
#-------------------------------------------------------------------------------
#---------------------------regress�o tipo pooling----------------------------
Emp.pooled1= plm(formula = formula1, data = EmplUK, model = "pooling")
summary(Emp.pooled1)
#--------------------------------------------------------------------------------
#---- regress�o com estimador de efeitos fixos, do tipo within---------------------

Emp.within1 <- plm(formul = formula1, data = EmplUK, model = "within")
summary(Emp.within1)

fixef(Emp.within1) #--- extraindo os efeitos fixos de cada unidade

#---- incluindo efeitos dos per�odos

Emp.within1 <- plm(log(emp) ~ lag(log(emp), 1) + lag(log(emp), 2) + lag(log(wage), 2) + lag(log(wage), 3) +
                     diff(log(capital), 2) + diff(log(capital), 3) + as.factor(year), data = EmplUK, model = "within")
summary(Emp.within1)
#-----------------------------------------------------------------------------------------
#---- regress�o com estimador de efeitos fixos, do tipo first diferences-------------------

Emp.fd1 <- plm(formul = formula1, data = EmplUK, model = "fd")
summary(Emp.fd1)
#-------------------
fixef(Emp.fd1) #extraindoi os efeitos fixos de cada unidade (vai dar errado)
#--------------------------------------------------------------------
#---- regress�o com estimador de efeitos fixos, do tipo between-----------------

Emp.between1 <- plm(formul = formula1, data = EmplUK, model = "between")
summary(Emp.between1)
#------------------
fixef(Emp.between1) #extraindo os efeitos fixos de cada unidade (vai dar erro!)
#----------------------------------------------------------------------------------
#------------------------O teste comumente empregado � o teste de Hausman___________
#--> phtest(fixed, random)
# teste de Hausmann (Hausman 1978) efetua a especifica��o dos modelos de Efeito Fixo 
#e de Efeitos Aleat�rios, sendo que se o teste rejeitar a hip�tese nula,
#o modelo de Efeitos Fixos � o mais adequado.
#Se o valor p for superior a 0,05 o modelo de Efeitos Aleat�rios � considerado superior
#ao modelo de Efeitos Fixos.
#Se o valor p for inferior a 0,05 o modelo de Efeitos Fixos  � considerado superior
#ao modelo de Efeitos Aleat�rio (o teste rejeita a hip�tese nula)


phtest(Emp.fd1,Emp.between1)



  