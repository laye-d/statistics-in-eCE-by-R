#python 删除csv文件多余开头(以便R处理).py
rm(list = ls())#清空变量
library(stringr)#通配符提取字符串内容


###################################################参数
num_cycle = 3#循环数
##################################################




#***************降温起始点搜索函数************
find.index.start = function(data, index_current) {
  for (i in index_current:nrow(data)){
    if(i+25>nrow(data)){
      stop("错误：搜索结束，未搜索到降温起始点!")
    }
    ######选出点的大致位置i：点后25个点基本都递减，且25个点温差大于2，且5个点温差大于0.3
    if(sum(data[(i+1):(i+25),2] - data[(i):(i+24),2] < 0) > 20 &&
       data[(i+25),2] - data[(i),2] < - 2 &&
       data[(i+5),2] - data[(i),2] < -0.3
       ){
      break
    }
  }
  
  ######起始点搜索判断条件选出i后，选择点前20个点、点后10个点,计算点前后两拟合线的角度差最大的点作为开始点
  data_angleDifference = data.frame()
  for (ii in (i-20):(i+10) ){
    coef1 = data.matrix(c(1,lm(data[(ii-6):(ii-2),2]~data[(ii-6):(ii-2),1])$coefficients[2]))
    coef2 = data.matrix(c(1,lm(data[(ii+2):(ii+6),2]~data[(ii+2):(ii+6),1])$coefficients[2]))
    #计算前后两段拟合直线的角度差
    data_angleDifference[ii-i+21,1] = acos(abs(sum(coef1 * coef2) / sqrt(sum(coef1^2) * sum(coef2^2))))*180/pi
  }
  i2 = which(data_angleDifference==max(data_angleDifference))
  i = i2 +i-21#选择角度差最大的点，返回之
  
  #作图1
  par(mfrow = c(2,1))#多个图
  plot(data[(i-50):(i+50),1],data[(i-50):(i+50),2])
  points(data[i,1],data[i,2],col="red")
  plot(data[(i-i2+21-20):(i-i2+21+10),1],data_angleDifference[,1])
  points(data[i,1],data_angleDifference[i2 ,1],col="red")
  
  #作图2
  par(mfrow = c(1,1))
  plot(x = data[(i-1000):(i+1000),1], y = data[(i-1000):(i+1000),2], type = "l")
  points(x = data[i,1], y = data[i,2], col="red",pch = 6,lwd = 3)
  
  return(i)
}

#***************降温结束点搜索函数************
find.index.end = function(data, index_current) {
  for (i in (index_current+1):nrow(data)){
    if(i+10>nrow(data)){
      stop("错误：搜索结束，未搜索到降温结束点!")
    }
    
    #寻找点：从index_current到此处的后半部分点至少30%都递减（避免选点远离绝热温变点），且其后10个点基本都大于该点
    if(sum(data[(floor(0.5*(index_current+i))+1):(i),2] - data[floor(0.5*(index_current+i)):(i-1),2] < 0) >= (0.3*(i-index_current)) &&
       sum(data[(i+1):(i+10),2] - data[(i):(i+9),2] > 0) >= 9 
    ){
      break
    }
  }

  points(x = data[i,1], y = data[i,2], col="red",pch = 2,lwd = 3)

  return(i)#which(data[,2] == min(data[(i+1):(i+25),2]))
}

fname = file.choose()
data = read.table(fname, )#打开空格分隔的文本文件
names(data)[1:2] <- c("Time","T")#为特定列命名

data_index = data.frame()
data_deltaT = data.frame()
i_current = 1

#*********************搜索降温段曲线的起始、结束点索引***********************
for(i in 1:num_cycle){
  i_current = data_index[i,1] = find.index.start(data,i_current)
  i_current = data_index[i,2] = find.index.end(data,i_current)
  data_deltaT[i,1] = i
  data_deltaT[i,2] = data[data_index[i,1],2] - data[data_index[i,2],2]
}
names(data_deltaT) = c("Cycle", "-deltaT")


#*******************保存csv文件*********************
temp_dirname = data.frame(str_extract_all(fname,"[^(\\\\)]+")[1])
if (dir.exists("温变数据处理//") == F){#查询目录是否存在
  dir.create("温变数据处理//") #创建文件夹
}
write.csv(data_deltaT, row.names = F,
          paste("温变数据处理//",temp_dirname[nrow(temp_dirname),1],"_温变计算结果.csv",sep = ""))#通配符提取文件名


