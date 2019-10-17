#************数据预处理函数***********
#得到各段升、降温曲线数据（epsilon~T）
#入参：
#原始数据、样品原始长度、
#原曲线的低温温度、高温温度、每段曲线两端去掉的一段温度范围、测试过程变温速度、升温过程变温速度、
#降温过程变温速度、循环次数、是否对原曲线及分割成段的原曲线作图、是否处理升温和降温的两段曲线、
process = function(data, 
                   L0 = 1000,
                   T.start = 25,
                   T.low = 25,
                   T.high = 180,
                   cut.T = 5,
                   v = 1,
                   v.up = 1,
                   v.down = 2,
                   circulation = 3,
                   TF.plot = TRUE,
                   TF.additional = TRUE
                   )
{
  #************数据预处理***********
  data = data[-1, ]
  data = data[-nrow(data), ]
  data$Ts = as.numeric(as.character(data$Ts))
  data$Length = as.numeric(as.character(data$Length))/L0
  data = na.omit(data)####

  #************计算每段曲线的索引***********
  #plot.idx存储索引，第1、2、3、4列为每个循环的降温起点、终点、升温起点、终点
  #additional.idx存储额外的升温段和降温段索引，第1、2列为两段的起点、终点
  plot.idx = data.frame()####
  plot.idx[1,1] = (T.high - T.start) * 60 / v.up + cut.T * 60 
  plot.idx[1,2] = plot.idx[1,1] + (T.high - T.low - cut.T * 2) *60
  plot.idx[1,3] = plot.idx[1,2] + cut.T * 120
  plot.idx[1,4] = plot.idx[1,3] + (T.high - T.low - cut.T * 2) *60 
  if(circulation>=2){
    for (i in 2:circulation) {
      plot.idx[i,] = plot.idx[i-1,] + (T.high - T.low) * 120 
    }
  }
  additional.idx = data.frame()####
  additional.idx[1,1] = cut.T * 60 / v.up
  additional.idx[1,2] = additional.idx[1,1] + (T.high - T.start - cut.T * 2) * 60 / v.up
  additional.idx[2,1] = circulation * (T.high - T.low) * 120 + (T.high - T.start) * 60 / v.up + cut.T * 60 / v.down
  additional.idx[2,2] = additional.idx[2,1] + (T.high - T.start - cut.T * 2) * 60 / v.down
  
  #************原曲线及分割成段的原曲线作图***********
  if(TF.plot == TRUE){
    plot(Length ~ Ts, data = data, type = "l")
    for (i in 1:circulation) {
      plot(Length ~ Ts, data = data[plot.idx[i,1]:plot.idx[i,2],], type = "l", col = "red",
           ylim = c(min(data$Length[plot.idx[i,1]:plot.idx[i,2]]),max(data$Length[plot.idx[i,3]:plot.idx[i,4]])))
      points(Length ~ Ts, data = data[plot.idx[i,3]:plot.idx[i,4],], type = "l", col = "blue")
    }
    if(TF.additional==TRUE){
      plot(Length ~ Ts, data = data[additional.idx[1,1]:additional.idx[1,2],], type = "l", 
           main = "additional, up")
      plot(Length ~ Ts, data = data[additional.idx[2,1]:additional.idx[2,2],], type = "l",
           main = "additional, down")
    }
  }
  
  #************返回降温数据和升温数据***********
  data.down = list()###直接frame?
  data.up = list()
  # if(return == "down"){
  #   for (i in 1:circulation) {
  #     data.down[(2*i-1)] = data.frame(data$Ts[plot.idx[i,1]:plot.idx[i,2]])
  #     data.down[2*i] = data.frame(data$Length[plot.idx[i,1]:plot.idx[i,2]])
  #   }
  #   data.down = data.frame(data.down)
  #   return(data.down)
  # }
  # if(return == "up"){
  #   for (i in 1:circulation) {
  #     data.up[2*i-1] = data.frame(data$Ts[plot.idx[i,3]:plot.idx[i,4]])
  #     data.up[2*i] = data.frame(data$Length[plot.idx[i,3]:plot.idx[i,4]])
  #   }
  #   data.up = data.frame(data.up)
  #   return(data.up)
  # }
  for (i in 1:circulation) {
    data.down[(2*i-1)] = data.frame(data$Ts[plot.idx[i,1]:plot.idx[i,2]])
    data.down[2*i] = data.frame(data$Length[plot.idx[i,1]:plot.idx[i,2]])
    data.up[2*i-1] = data.frame(data$Ts[plot.idx[i,3]:plot.idx[i,4]])
    data.up[2*i] = data.frame(data$Length[plot.idx[i,3]:plot.idx[i,4]])
  }
  data.down = data.frame(data.down)
  data.up = data.frame(data.up)
  return(list(data.down, data.up))
  
}



#************求导函数***********
#拟合相同应力各温度的应变数据（epsilon~T），求导得到相同应力各温度的应变导数数据（d(epsilon)/dT~T）
#入参：
#不同应力下Ts和对应Length数据、加力范围、横截面面积、数据处理结果使用的温度范围、regressor类型、
#多项式拟合的次数、是否对拟合曲线和导数曲线作图
derive = function(data,
                  F  = seq(0.1, 1, 0.1),
                  S = 100 * 300,
                  Temp = seq(30,175, 0.01),
                  regressor = "gpr",
                  num.noise = 0.00000002,
                  num.poly = 4,
                  TF.plot = TRUE
){
  library(pspline)
  F = data.frame(F = F)
  Temp = data.frame(Ts = Temp)#须保证Temp的列名与拟合model的Ts的列名相同！，否则该gpr算法等不能predict获得y值
  stress = data.frame(F / S * 10^12)#单位Pa
  data.derive = matrix(nrow = nrow(Temp), ncol = nrow(stress), 
                       #须为Temp[,1]，不可Temp，否则字符维数为1维
                       dimnames = list(as.character(Temp[,1]),as.character(stress[,1])))
  data.regression = matrix(nrow = nrow(Temp), ncol = nrow(stress), 
                       #须为Temp[,1]，不可Temp，否则字符维数为1维
                       dimnames = list(as.character(Temp[,1]),as.character(stress[,1])))
  
  if(regressor == "poly"){
    for (i in 1:nrow(stress)){
      #临时数据框，命名列名以拟合????
      data.temp = data.frame(Ts = data[,2*i-1], Length = data[,2*i])
      #data = data.down.all
      #data.temp = data.frame(Ts = data[,1], Length = data[,2])
      model = lm(Length ~ poly(Ts, num.poly, raw = T), data = data.temp)
      L.predict = data.frame(Length = predict(model, Temp))
      # 此处必须新建一个数据框df？？？？，否则sm.spline()报错
      df = data.frame(Temp, L.predict)
      data.derive[,i] = predict(sm.spline(df$Ts, df$Length), df$Ts, 1)
      #data.derive[,i] = predict(sm.spline(Temp, L.predict), Temp, 1)
      if(TF.plot == TRUE){
        plot(data[,2*i] ~ data[,2*i-1], type = "l")
        lines(data[,2*i-1], predict(model), col = 2)#或：predict(model,data.1.1)
        plot(data.derive[,i] ~ as.matrix(Temp), type = "l")
      }
    }
  }
  
  if(regressor == "gpr"){
    library(DiceKriging)
    for (i in 1:nrow(stress)){
      data.temp = data.frame(Ts = data[,2*i-1], Length = data[,2*i])
      data.temp = data.temp[seq(1,nrow(data.temp),8),]#间隔取点
      noise=rep(num.noise,nrow(data.temp))#noise小，拟合更接近原曲线
      Ts = data.frame(Ts = data.temp$Ts)
      Length = data.frame(data.temp$Length)
      model <- km(formula<-~.,design=Ts, response=Length 
              ,noise.var = noise
              ,covtype = "exp",coef.var=0.03,optim.method="BFGS",multistart = 1, gr = TRUE###type= gauss，平滑，但0.9N1N曲线拟合会出现问题？？
      )
      #预测Temp对应的y值，须保证Temp的列名与拟合model的Ts的列名相同！，否则该gpr算法不能predict获得y值
      L.predict = data.frame(Length = predict(model, Temp, "UK")$mean)
      df = data.frame(Temp, L.predict)
      data.derive[,i] = predict(sm.spline(df$Ts, df$Length), df$Ts, 1)
      data.regression[,i] = L.predict$Length
      if(TF.plot == TRUE){
        plot(Length ~ Ts, data.temp, type = "l")
        points(L.predict$Length ~ Temp$Ts, col = 2, type = "l") #须加$，否则为list
        plot(data.derive[,i] ~ as.matrix(Temp), type = "l")
      }
    }
  data.derive = cbind(data.derive, data.regression)
  }
  
  #部分曲线tune报错： ‘cross’ must not exceed sampling size! 
  if(regressor == "svr.radial"){
    library(e1071)
    for (i in 1:nrow(stress)){
      #原数据耗时过长，所以等间隔抽取数据点
      data = data[seq(1,nrow(data),10),]
      data.temp = data.frame(Ts = data[,2*i-1], Length = data[,2*i])
      tuneResult = tune(svm, Length ~ Ts, data = data.temp,kernel = "radial", 
                        ranges = list(gamma = seq(0,1,0.2), cost = seq(20,100,20)))
      print(tuneResult)
      parameter = tuneResult$best.parameters
      gamma = parameter[1]
      cost = parameter[2]
      model = svm(Length ~ Ts, data = data.temp, kernel = "radial",
                  gamma = gamma, cost = cost
      )
      L.predict = data.frame(Length = predict(model, Temp))
      df = data.frame(Temp, L.predict)
      data.derive[,i] = predict(sm.spline(df$Ts, df$Length), df$Ts, 1)
      if(TF.plot == TRUE){
        plot(data[,2*i] ~ data[,2*i-1], type = "l")
        lines(data[,2*i-1], predict(model), col = 2)
        plot(data.derive[,i] ~ as.matrix(Temp), type = "l")
      }
    }
  }
  #若转程序框列名有问题？？？
  #data.derive = data.frame(data.derive)
  #character类型数据？？？
  return(data.derive)
}







#************积分函数***********
#转置得到相同温度各应力的应变导数数据（d(epsilon)/dT~(sigma)）,拟合并积分得到各温度的变应力熵变（S~T）
#入参：
#不同应力下温度对应的导数数据、加力范围、原始长度、横截面面积、导数数据使用的温度范围、regressor类型、
#多项式拟合的次数、每间隔多少点对导数拟合曲线作图、是否对导数拟合曲线和deltaS曲线作图
Integrate = function(data,
                     F  = seq(0.1, 1, 0.1),
                     L0 = 1000,
                     S = 100 * 300,
                     rho = 6500,
                     Temp = seq(30,175, 0.01),
                     regressor = "poly",
                     num.poly = 4,
                     interval.plot = 2000,
                     TF.plot = TRUE
                  ){
  F = data.frame(F = F)
  Temp = data.frame(Ts = Temp)
  stress = data.frame(stress = F[,1] / S * 10^12)#单位Pa
  V0 = L0 * S /10^18
  #data = t(data)
  data.deltaS = matrix(nrow = nrow(Temp), ncol = 2)
  k = 1
  for (i in 1:nrow(Temp)) {
    data.temp = data.frame(stress = stress[,1], derive = data[i,])
    model = lm(derive ~ poly(stress, num.poly, raw = T), data = data.temp)
    ###拟合的多项式函数方程系数
    coef=coef(model)
    ###拟合的多项式函数方程
    func = function(x){
      func = 0
      for (j in 1:num.poly+1) {
        func = coef[j]*x^(j - 1) + func
      }
      return(func + coef[1])
    }
    #若如下会丢失coef[1]？？？？？？？？？
    # func = function(x){
    #   func = 0
    #   for (j in 1:num.poly+1) {
    #     func = coef[j]*x^(j - 1) + func
    #   }
    #   return(func)
    # }
    if (TF.plot == TRUE) {
      i.plot = seq(1, nrow(Temp)+interval.plot, interval.plot)
      if (i == i.plot[k]){
        plot(derive ~ stress, data = data.temp)
        #print(coef)
        #两图重叠
        par(new=TRUE)
        plot(func, from = stress[1,], to = stress[nrow(stress),], col = "2")
        k = k + 1
      }
    }
    data.deltaS[i, 1] = Temp[i,1]
    data.deltaS[i, 2] = V0 * as.numeric(integrate(func, 0, stress[nrow(stress),])[1])/(rho * S * L0 * 10^(-18))
  }
  data.deltaS = data.frame(data.deltaS)
  names(data.deltaS) = c("Temperature", "deltaS")
  return(data.deltaS)
  
}










# 
# 
# #************多项式拟合***********
# 
# regressor.poly = function(data, num.poly){
#   model = lm(Length ~ poly(Ts, num.poly, raw = T), data = data)
#   return(model)
# }
# 
# 
# 
# 
# 
# #************svr***********
# regressor.svr.linear = function(){
#   library(e1071) 
#   data.1.1 = data[plot.idx[1,1]:plot.idx[1,2],]
#   # tuneResult.svr.lin = tune(svm, Length ~ Ts, data = data.1.1,kernel = "linear", ranges = list(gamma = seq(0,1,0.2), cost = seq(20,100,20)))
#   # plot(tuneResult.svr.lin)
#   # print(tuneResult.svr.lin)
#   # parameter.svr.lin = tuneResult.svr.lin$best.parameters
#   # gamma = parameter.svr.lin[1]
#   # cost = parameter.svr.lin[2]
#   model = svm(Length ~ Ts, data = data.1.1, kernel = "linear"
#               #gamma = gamma, cost = cost
#   )
#   plot(Length ~ Ts, data = data.1.1, type = "l")
#   lines(data.1.1$Ts, predict(model), col = 2)
# }
# 
# 
# regressor.svr.polynomial = function(){
#   library(e1071) 
#   data.1.1 = data[plot.idx[1,1]:plot.idx[1,2],]
#   model = svm(Length ~ Ts, data = data.1.1, kernel = "polynomial"
#               #gamma = gamma, cost = cost
#   )
#   plot(Length ~ Ts, data = data.1.1, type = "l")
#   lines(data.1.1$Ts, predict(model), col = 2)
# }
# 
# 
# regressor.svr.sigmoid = function(){
#   library(e1071) 
#   data.1.1 = data[plot.idx[1,1]:plot.idx[1,2],]
#   model = svm(Length ~ Ts, data = data.1.1, kernel = "sigmoid"
#               #gamma = gamma, cost = cost
#   )
#   plot(Length ~ Ts, data = data.1.1, type = "l")
#   lines(data.1.1$Ts, predict(model), col = 2)
# }
# 
# 
# regressor.svr.radial = function(){
#   library(e1071) 
#   data.1.1 = data[plot.idx[1,1]:plot.idx[1,2],]
#   model = svm(Length ~ Ts, data = data.1.1, kernel = "radial"
#               #gamma = gamma, cost = cost
#   )
#   plot(Length ~ Ts, data = data.1.1, type = "l")
#   lines(data.1.1$Ts, predict(model), col = 2)
# }
# 
# 
# 
# 
# #************Gaussian Process Regression（原数据耗时过长，所以等间隔抽取数据点）***********
# regressor.gpr = function(){
#   library(DiceKriging)
#   data.1.1 = data[plot.idx[1,1]:(plot.idx[1,2]),]
#   #每间隔8个点抽取一个数据点
#   data.1.1 = data.1.1[seq(1,nrow(data.1.1),8),]
#   noise=rep(2,nrow(data.1.1))
#   Ts = data.frame(data.1.1$Ts)
#   Length = data.frame(data.1.1$Length)
#   m <- km(formula<-~.,design=Ts, response=Length 
#           ,noise.var = noise
#           ,covtype = "exp",coef.var=3,optim.method="BFGS",multistart = 1, gr = TRUE
#   )
#   p <- predict(m, Ts, "UK")
#   predictions=as.matrix(p$mean)
#   Ts=as.matrix(Ts)
#   plot(Length ~ Ts, data = data.1.1, type = "l")
#   points(predictions ~ Ts, col = 2, type = "l")
# }
# 
# 
# 
