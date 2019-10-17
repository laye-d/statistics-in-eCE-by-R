setwd(getwd())#getwd()
source("process.R")




#************************************
#**********样品数据
#************************************
L0 = 10000#微米
S = 215 * 72#微米
rho = 6500#千克每立方米
# F=seq(0.2, 1, 0.2)#牛
F = c(0.2, 0.4, 0.7, 1)#牛
#结果deltaS单位J/(K*kg)
num.noise.down.0.2_0.4N = 0.0000001
num.noise.down.0.7_1N = 0.000001
num.noise.up = 0.0000002

T.start = 25
T.low = -70
T.high = 75
cut.T = 5



#************读入csv文件***********
data.raw.1 = read.csv("TiNi50.8_longitudinal_0.2N-0.4N_up0.2N,down0.2N.csv")
data.raw.2 = read.csv("TiNi50.8_longitudinal_0.7N-1N_up0.2N,down0.2N.csv")


#************预处理得到每个文件中各段升、降温曲线数据（epsilon~T）***********
data.1 = process(data = data.raw.1, 
                 L0 = L0,
                 T.start = T.start,
                 T.low = T.low,
                 T.high = T.high,
                 cut.T = 5,
                 v = 1,
                 v.up = 2,
                 v.down = 2,
                 circulation = 2,
                 TF.plot = TRUE,
                 TF.additional = TRUE
                 )

#升、降温曲线数据
data.1.down = data.frame(data.1[1])
data.1.up = data.frame(data.1[2])
#额外的升温段、降温段曲线数据
#data.1.extra = data.frame(data.1[3])


data.2 = process(data = data.raw.2, 
                 L0 = L0,
                 T.start = T.start,
                 T.low = T.low,
                 T.high = T.high,
                 cut.T = 5,
                 v = 1,
                 v.up = 2,
                 v.down = 2,
                 circulation = 2,
                 TF.plot = TRUE,
                 TF.additional = TRUE
)

#升、降温曲线数据
data.2.down = data.frame(data.2[1])
data.2.up = data.frame(data.2[2])






#************合并各文件数据***********
data.down.all = cbind(data.1.down, data.2.down)
data.up.all = cbind(data.1.up, data.2.up)








#************拟合相同应力各温度的应变数据（epsilon~T），求导得到相同应力各温度的应变导数数据（d(epsilon)/dT~T）***********
#注：返回文件后半部分附带拟合曲线数据

data.down.derive.0.2_0.4N = derive(data.down.all[,1:4],
                                   F = F[1:2],
                                   S = S,
                                   Temp = data.frame(Ts = seq(T.low + cut.T, 
                                                              T.high - cut.T, 0.01)),
                                   regressor = "gpr",
                                   num.noise = num.noise.down.0.2_0.4N,
                                   num.poly = 5,
                                   TF.plot = TRUE)

data.down.derive.0.7_1N = derive(data.down.all[,5:8],
                                 F = F[3:4],
                                 S = S,
                                 Temp = data.frame(Ts = seq(T.low + cut.T, 
                                                            T.high - cut.T, 0.01)),
                                 regressor = "gpr",
                                 num.noise = num.noise.down.0.7_1N,
                                 num.poly = 5,
                                 TF.plot = TRUE)

#合并各回归曲线数据
data.down.regression = cbind(data.down.derive.0.2_0.4N[,3:4], data.down.derive.0.7_1N[,3:4])
 
#合并各导数数据
data.down.derive = cbind(data.down.derive.0.2_0.4N[,1:2], data.down.derive.0.7_1N[,1:2])


data.up.derive = derive(data.up.all,
                        F = F,
                        S = S,
                        Temp = data.frame(Ts = seq(T.low + cut.T, 
                                                   T.high - cut.T, 0.01)),
                        regressor = "gpr",
                        num.noise =  num.noise.up,
                        num.poly = 5,
                        TF.plot = TRUE)                  

#提取回归曲线数据
data.up.regression = data.up.derive[,5:8]
#提取导数数据
data.up.derive = data.up.derive[,1:4]


#************转置得到相同温度各应力的应变导数数据（d(epsilon)/dT~(sigma)）,拟合并积分得到各温度的变应力熵变（S~T）***********
data.down.deltaS = Integrate(data.down.derive,
                             F  = F,
                             L0 = L0,
                             S = S,
                             rho = rho,
                             Temp = data.frame(Ts = seq(T.low + cut.T, 
                                                        T.high - cut.T, 0.01)),
                             regressor = "poly",
                             num.poly = 2,
                             interval.plot = 2000,
                             TF.plot = TRUE)

data.up.deltaS = Integrate(data.up.derive,
                           F  = F,
                           L0 = L0,
                           S = S,
                           rho = rho,
                           Temp = data.frame(Ts = seq(T.low + cut.T, 
                                                      T.high - cut.T, 0.01)),
                           regressor = "poly",
                           num.poly = 2,
                           interval.plot = 2000,
                           TF.plot = TRUE)

#结果作图
#由于两边的温度求导不准，所以此处求出的deltaS也不准，可忽略
plot(deltaS ~ Temperature, data = data.down.deltaS, type = "l",
     main = c("Down, From 0N to 1N"))######c()) 
axis(1, seq(-150,200,5), seq(-150,200,5))
plot(deltaS ~ Temperature, data = data.up.deltaS, type = "l",
     main = c("Up, From 0N to 1N"))   
axis(1, seq(-150,200,5), seq(-150,200,5))


##############
F = data.frame(F)
#拟合结果作图
for (i in 1:nrow(F)){
  plot(data.down.all[,2*i] ~ data.down.all[,2*i-1], type = "l",
       main = c("F = ",F[i,1],"N"))
  points(data.down.regression[,i] ~ seq(T.low + cut.T, T.high - cut.T, 0.01), type = "l", col = 2)
  axis(1, seq(-150,200,5), seq(-150,200,5))
}

for (i in 1:nrow(F)){
  plot(data.up.all[,2*i] ~ data.up.all[,2*i-1], type = "l",
       main = c("F = ",F[i,1],"N"))
  points(data.up.regression[,i] ~ seq(T.low + cut.T, T.high - cut.T, 0.01), type = "l", col = 2)
  axis(1, seq(0,200,5), seq(0,200,5))
}


#求导结果作图
for (i in 1:nrow(F)){
  plot(data.down.derive[,i] ~ seq(T.low + cut.T, T.high - cut.T, 0.01), type = "l",
       main = c("F = ",F[i,1],"N"))######c()
  axis(1, seq(-150,200,5), seq(-150,200,5))
}
for (i in 1:nrow(F)){
  plot(data.up.derive[,i] ~ seq(T.low + cut.T, T.high - cut.T, 0.01), type = "l",
       main = c("F = ",F[i,1],"N"))
  axis(1, seq(-150,200,5), seq(-150,200,5))
}



#导数数据（down,等间隔取100温度点）
library(ggplotgui)
ggplot_shiny(data.frame(t(rbind(t(F), data.down.derive)
                          [seq(1,length(seq(T.low + cut.T, T.high - cut.T, 0.01)),floor(length(seq(-25,95, 0.01))/100)),])))
library(ggplotgui)
ggplot_shiny(data.frame(t(rbind(t(F), data.up.derive)
                          [seq(1,length(seq(T.low + cut.T, T.high - cut.T, 0.01)),floor(length(seq(-25,95, 0.01))/100)),])))



# save.image("script_TiNi50.8_longitudinal.RData")
# 
# 
# write.csv(data.down.regression,file="D:/data/TiNi50.8_longitudinal_data.down.regression.csv")
# write.csv(data.down.all,file="D:/data/TiNi50.8_longitudinal_data.down.all.csv")
# write.csv(data.down.derive,file="D:/data/TiNi50.8_longitudinal_data.down.derive.csv")
# write.csv(data.down.deltaS,file="D:/data/TiNi50.8_longitudinal_data.down.deltaS.csv")
# 
# 
# write.csv(data.up.regression,file="D:/data/TiNi50.8_longitudinal_data.up.regression.csv")
# write.csv(data.up.all,file="D:/data/TiNi50.8_longitudinal_data.up.all.csv")
# write.csv(data.up.derive,file="D:/data/TiNi50.8_longitudinal_data.up.derive.csv")
# write.csv(data.up.deltaS,file="D:/data/TiNi50.8_longitudinal_data.up.deltaS.csv")



#load("script_TiNi50.8_longitudinal.RData")



