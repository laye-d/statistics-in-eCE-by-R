#python 删除csv文件多余开头(以便R处理).py

rm(list = ls())#清空变量

library(stringr)#通配符提取字符串内容

###################################################参数
num_cycle = 2#循环数
stress_loading = 900#应力
stress_unloading = 5#应力
##################################################




#***************卸载段起始点搜索函数************
find.index.start = function(data, index_current, stress = 500) {
  for (i in index_current+99:nrow(data)){
    if(i+9>nrow(data)){
      stop("错误：搜索结束，未搜索到卸载起始点!")
    }
    #寻找点：处于保载为stress的阶段，且在(stress-0.25, stress+0.25)范围内，且从此处10个数递减，且后面9个至少6个小于stress-2
    if(sum(round(data[(i-99):i,4]) == stress) == 100 &&
       sum(data[i:(i+8),4] - data[(i+1):(i+9),4] > 0) == 9 &&
       stress-0.25 <= data[i,4] && data[i,4] <= stress+0.25 &&
       sum(data[(i+1):(i+9),4] <= stress-2) >= 6){
      break
    }
  }

  print("******unloading_start*******")
  print(data[(i-9):(i-1),4:5])
  print(data[i,4:5])
  print(data[(i+1):(i+9),4:5])
  print("****************************")
  
  return(i)
}

#***************卸载段结束点搜索函数************
find.index.end = function(data, index_current,stress) {
  for (i in index_current+9:nrow(data)){
    if(i+5>nrow(data)){
      stop("错误：搜索结束，未搜索到卸载结束点!")
    }
    if(round(data[i,4]) == stress){
      stop("错误：已到下一循环，未搜索到卸载结束点!")
    }
    #寻找点：到此处10个数至少9个递减，且其前5个点后3个点绝对值均大于该点
    if(sum(data[(i-9):(i-1),4] - data[(i-8):(i),4] > 0) >= 8 &&
       sum(abs(data[(i-5):(i+4),4]) - abs(data[i,4]) > 0) >= 8){
      break
    }
  }
  print("******unloading_end*******")
  print(data[(i-9):(i-1),4:5])
  print(data[i,4:5])
  print(data[(i+1):(i+9),4:5])
  if(data[i,4]>1){
    print("警告：卸载结束点大于1MPa")
  }
  print("****************************")
  return(i)
}

#***************超弹性应变结束点搜索函数************
find.index.strainEnd = function(data, index_current, stress = 500, stress.low = 5) {
  for (i in index_current+99:nrow(data)){
    if(i+9>nrow(data)){
      stop("错误：搜索结束，未搜索到超弹性应变结束点!")
    }
    if(round(data[i,4]) == stress){
      stop("错误：已到下一循环，未搜索到超弹性应变结束点!")
    }
    #寻找点：处于保载为stress.low的阶段，且在(stress-0.1, stress+0.1)范围内，且从此处20个数至少15个递增，且后面第19个数大于stress.low+0.7
    if(sum(round(data[(i-99):i,4]) == stress.low) >= 99 &&
       sum(data[i:(i+18),4] - data[(i+1):(i+19),4] < 0) >= 14 &&
       stress.low-0.1 <= data[i,4] && data[i,4] <= stress.low+0.1 &&
       data[(i+20),4] > stress.low+0.5 ){
      break
    }
  }
  print("******prolonging_end*******")
  print(data[(i-9):(i-1),4:5])
  print(data[i,4:5],)
  print(data[(i+1):(i+15),4:5])
  print("****************************")
  
  return(i)
}

fname = file.choose()
data = read.csv(fname, )
names(data)[4:5] <- c("Stress","Strain")#为特定列命名
data[,4] = as.double(as.character(data[,4]))
data[,5] = as.double(as.character(data[,5]))

data_index = data.frame()
# names(data_index)=c("start","end")
data_integrate = data.frame()
i_current = 1


#*********************搜索卸载段曲线的起始、结束点索引***********************
for(i in 1:num_cycle){
  i_current = data_index[i,1] = find.index.start(data,i_current,stress_loading)
  i_current = data_index[i,2] = find.index.end(data,i_current,stress_loading)
  if(i<num_cycle){#最后一个循环不搜索超弹性应变结束点，认为曲线结束点为超弹性应变结束点
    i_current = data_index[i,3] = find.index.strainEnd(data,i_current,stress_loading,stress_unloading)
  }
  else{
    if(round(data[nrow(data),4]) == stress_unloading){
      i_current = data_index[i,3] = nrow(data)
    }
    else{
      print("警告：曲线结束点应力不等于卸载保载应力，尝试搜索超弹性应变结束点...")
      i_current = data_index[i,3] = find.index.strainEnd(data,i_current,stress_loading,stress_unloading)
      }
  }
  
  #积分计算卸载功、计算超弹性应变(仅卸载过程)、计算超弹性应变
  f <- approxfun(x = data[(data_index[i,1]:data_index[i,2]),5], y = data[(data_index[i,1]:data_index[i,2]),4],
                 method = "linear")#线性插值函数
  data_integrate[i,1] = i
  data_integrate[i,2] = integrate(f, data[data_index[i,1],5], data[data_index[i,2],5])[1] #线性插值函数面积积分
  data_integrate[i,2] = abs(data_integrate[i,2])
  data_integrate[i,3] = data[data_index[i,1],5] - data[data_index[i,2],5]
  data_integrate[i,4] = data[data_index[i,1],5] - data[data_index[i,3],5]
}
names(data_integrate)[1:4] = c("Cycle", "Work per Unit Volume","超弹性应变(仅卸载过程)","超弹性应变(最后一个循环按曲线结束点应变计算)")
#*********************曲线数据***********************
data_unloadingCurve = list()
for (i in 1:num_cycle) {
  data_unloadingCurve[[i]] = data[data_index[i,1]:data_index[i,2],5:4]
}
if(num_cycle>1) {
  data_unloadingProlongingCurve = list()
  for (i in 1:(num_cycle-1)) {
  data_unloadingProlongingCurve[[i]] = data[data_index[i,1]:data_index[i,3],5:4]
  }
}

#*******************原始拉伸曲线图*********************
plot(x = data[data_index[1,1]:data_index[1,2],5], y = data[data_index[1,1]:data_index[1,2],4],type = "l",
     xlab = "Strain (%)", ylab = "Stress (MPa)",
     main = paste(str_extract_all(fname,"[^(\\\\)]+.csv"),"_",num_cycle," cycles",sep = ""))
if (num_cycle>1){
  for (i in 2:num_cycle){
    lines(x = data[data_index[i,1]:data_index[i,2],5], y = data[data_index[i,1]:data_index[i,2],4])
  }
}

#*******************保存csv文件*********************
if (dir.exists(paste(str_extract_all(fname,"[^(\\\\)]+.csv"),"_数据处理//",sep = "")) == F){#查询目录是否存在
  dir.create(paste(str_extract_all(fname,"[^(\\\\)]+.csv"),"_数据处理//",sep = "")) #创建文件夹
}

write.csv(data_integrate, row.names = F,
          paste(str_extract_all(fname,"[^(\\\\)]+.csv"),"_数据处理//",
                str_extract_all(fname,"[^(\\\\)]+.csv"),"_积分结果.csv",sep = ""))#通配符提取文件名
for (i in 1:num_cycle) {
  write.csv(data_unloadingCurve[[i]], row.names = F,
            paste(str_extract_all(fname,"[^(\\\\)]+.csv"),"_数据处理//",
                  str_extract_all(fname,"[^(\\\\)]+.csv"),"_卸载曲线_Cycle",as.character(i),".csv",sep = ""))#通配符提取文件名
  if(num_cycle>1 && i < num_cycle) {
  write.csv(data_unloadingProlongingCurve[[i]], row.names = F,
            paste(str_extract_all(fname,"[^(\\\\)]+.csv"),"_数据处理//",
                  str_extract_all(fname,"[^(\\\\)]+.csv"),"_卸载+保载曲线_Cycle",as.character(i),".csv",sep = ""))
  }
}

