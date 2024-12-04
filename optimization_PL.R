# Optimization - minimize production loss


# install.packages("lpSolve")
library(lpSolve)
library(plyr)
cs = read.csv("0_data_prep/optimization_input.csv")
cs=cs[,-1]
cs$crop_area_pct = cs$Area / cs$totalHA
cs[is.na(cs)] <- 0



# 1. Set objective ####
# minimize production loss
# x is the percent of HA for a crop, ranging from 0 to 1.
Objective.in = cs$Yield*cs$totalHA*(-cs$CSensitivity)/100

# 2. Format constraint matrix #### 
# a. format constraints for total area in each district = current HA
dstr = unique(cs$District)
df_dstr = data.frame(matrix(nrow = nrow(cs), ncol = length(dstr)))

n = 1
a1 = 0
for (j in dstr){
  cs_dstr = cs[cs$District==j,]
  
  a2 = nrow(cs_dstr)
  a3 = (nrow(cs)-a1-a2)
  df_dstr[,n] = c(rep(0,a1),rep(1,a2),rep(0,a3))
  a1 = a1+a2
  n=n+1
}

one_col_dstr = data.frame(x=unlist(df_dstr))

# b. national total calorie does not decrease
df_dstr_calo = data.frame(matrix(nrow = nrow(cs), ncol = 1))
df_dstr_calo[,1] = cs$Yield * cs$totalHA * cs$Kcal * 10000
one_col_dstr_calo = data.frame(x=unlist(df_dstr_calo))

# b.1 national total calorie except maize does not decrease
cs$"Kcal_noma" = ifelse(cs$Crop == "mze", 0, cs$Kcal)
df_dstr_calo_noma = data.frame(matrix(nrow = nrow(cs), ncol = 1))
df_dstr_calo_noma[,1] = cs$Yield * cs$totalHA * cs$Kcal_noma * 10000
one_col_dstr_calo_noma = data.frame(x=unlist(df_dstr_calo_noma))

# b.2 national calorie of maize does not decrease to less than 1/2 of current
cs$"Kcal_mze" = ifelse(cs$Crop == "mze", cs$Kcal, 0)
df_dstr_calo_mze = data.frame(matrix(nrow = nrow(cs), ncol = 1))
df_dstr_calo_mze[,1] = cs$Yield * cs$totalHA * cs$Kcal_mze * 10000
one_col_dstr_calo_mze = data.frame(x=unlist(df_dstr_calo_mze))


# c. crops that could not be substitution should not increase 
# --> those crop in each District <= current area
df_vrb = data.frame(matrix(nrow = nrow(cs)))

for (vrb in 1:nrow(cs)){
  df_vrb[vrb,vrb] = unlist(cs$subs)[vrb]
} 
df_vrb[is.na(df_vrb)] <- 0

one_col_vrb = data.frame(x=unlist(df_vrb))

# c1. maize in each District 
df_vrb_mze = data.frame(matrix(nrow = nrow(cs)))
cs$mze = ifelse(cs$Crop == "mze", 1, 0)
for (vrb in 1:nrow(cs)){
  df_vrb_mze[vrb,vrb] = unlist(cs$mze)[vrb]
} 
df_vrb_mze[is.na(df_vrb_mze)] <- 0

one_col_vrb_mze = data.frame(x=unlist(df_vrb_mze))


# d. each crop in each District does not change >X%
df_vrb_each = data.frame(matrix(nrow = nrow(cs)))

for (vrb in 1:nrow(cs)){
  df_vrb_each[vrb,vrb] = 1
} 
df_vrb_each[is.na(df_vrb_each)] <- 0
one_col_vrb_each = data.frame(x=unlist(df_vrb_each))


# e. net profit (rs/100kg) in each District does not decrease
cs$totalProfit = cs$totalHA *  cs$Profit * cs$Yield * 10
df_dstr_pro = data.frame(matrix(nrow = nrow(cs), ncol = length(dstr)))

n = 1
a1 = 0
for (j in dstr){
  cs_dstr_pro = cs[cs$District==j,]
  
  a2 = nrow(cs_dstr_pro)
  a3 = (nrow(cs)-a1-a2)
  df_dstr_pro[,n] = c(rep(0,a1),unlist(cs_dstr_pro$totalProfit),rep(0,a3))
  a1 = a1+a2
  n=n+1
}
one_col_dstr_pro = data.frame(x=unlist(df_dstr_pro))

# 3. Calculate constraints for each matrix ####
# a. sum of area pct = 100%
area_const = c(rep(1,length(dstr)))

# b. sum of calorie in all District >= current total calorie
# tonne/ha * ha * kcal/100g * 10000 = kcal
cur_calo_const = sum(cs$Yield * cs$Area * cs$Kcal * 10000, na.rm=T)

# b.1 national total calorie except maize does not decrease
cur_calo_const_noma = sum(cs$Yield * cs$Area * cs$Kcal_noma * 10000, na.rm=T)

# b.2 national calorie of maize does not decrease to less than 1/2 current
cur_calo_const_mze = sum(cs$Yield * cs$Area * cs$Kcal_mze * 10000, na.rm=T)


# c. non-substitute crops should be lower than current.
subst_const = cs$crop_area_pct

# c1.maize in each District does not change/ hold constant !!!!
subst_const_mze = ifelse(cs$Crop == "mze",cs$crop_area_pct,0)

# d. each crop in each District does not change >X%
vrb_const = cs$crop_area_pct 

# e.  net profit (rs/100kg) in each District does not decrease
cur_pro = c()
n=1
for (k in dstr){
  cs_dstr = cs[cs$District == k,]
  # rs/100kg * tonne/ha * ha * 10 = rs
  cs_dstr[,"net"] = cs_dstr$Profit * cs_dstr$Yield * cs_dstr$Area * 10
  cur_pro[n] = sum(cs_dstr$net,na.rm=TRUE) 
  n=n+1
}

net_const = cur_pro

# 4. Optimization ####
sc = cs
for (rg in seq(0.05,0.5,0.05)){
  # rg = 0.05
  Const.mat<-matrix(c(unlist(one_col_dstr), # sum of area pct in each district = 100%
                      unlist(one_col_dstr_pro),  # net profit in each district >= current
                      unlist(one_col_dstr_calo_noma), # sum of calorie in all District, except maize >= current
                      unlist(one_col_dstr_calo_mze), # sum of all maize calorie in all District >= 1/2 current
                      unlist(one_col_vrb), # non-substitute crops should be lower than current
                      unlist(one_col_vrb_each),unlist(one_col_vrb_each)), # each crop in each district doesn't change > x%
                    nrow= 2*length(dstr)+1+1+3*nrow(df_vrb),byrow=TRUE)
  
  Const.rhs = c(area_const,
                net_const,
                cur_calo_const_noma,
                1/2*cur_calo_const_mze,
                subst_const,
                (vrb_const+rg), (vrb_const-rg)) # percent point change
  
  
  Const.dir = c(rep("==",length(dstr)),
                rep(">=",length(dstr)),
                ">=", ">=",
                rep("<=",nrow(df_vrb)),
                rep("<=",nrow(df_vrb_each)),rep(">=",nrow(df_vrb_each)))
  
  Optimum = lp(direction="min", Objective.in,
               Const.mat, Const.dir, Const.rhs)
  
  sc[,paste("opt_pct_",rg*100,"%",sep="")] = Optimum$solution
  
}


