
#原始数据文件应含有“dT.dW”，"绝热温变"，"卸载功.相变."，"IF"等数据列

#批量读入文件数据并处理
path = ".\\data_k-IF"
fileName = dir(path)
data = data.frame()
for(i in 1:length(fileName)){
  data.temp = read.csv(file = paste(path,fileName[i],sep = "\\"),
                  header = T,stringsAsFactors = F)[-1,]
  #data.temp$dT.dW = as.double(as.character(data.temp$dT.dW))
  data.temp$dT.dW = as.double(data.temp$dT.dW)
  data.temp$卸载功.相变. = as.double(data.temp$卸载功.相变.)
  data.temp$绝热温变 = as.double(data.temp$绝热温变)
  data.temp = data.temp[which(is.na(data.temp$dT.dW) == F),c("绝热温变","卸载功.相变.","IF")]
  data = rbind(data,data.temp)
}
#新建文件夹
if (dir.exists(".\\output") == F){
  dir.create(".\\output") 
}

#选择”卸载功.相变.“值在特定区间的数据 
#未，线性拟合
data.output = data.frame()

for (i in 1:15) {
  num.min = (i-1)*200
  num.max = i*200
  data.filtered = data[which(num.min<=data$卸载功.相变.),]
  data.filtered = data.filtered[which(data.filtered$卸载功.相变.<=num.max),]
  col.min = matrix(num.min,nrow(data.filtered),1)
  col.max = matrix(num.max,nrow(data.filtered),1)
  data.filtered = cbind(col.min,col.max,data.filtered)
  #计算IF/dW和dT/dW
  data.filtered$"IF/dW" = data.filtered$IF/data.filtered$卸载功.相变.
  data.filtered$"dT/dW" = data.filtered$绝热温变/data.filtered$卸载功.相变.#"dT/dW"
  #按IF列数据大小排序
  data.filtered = data.filtered[order(data.filtered$IF), ]
  data.output = rbind(data.output,data.filtered)
  if(nrow(data.filtered)>0){
    #保存csv
    write.csv(data.filtered, row.names = F,paste(".\\output\\output_",as.character(num.min),"-",as.character(num.max),".csv",sep = ""))
  }
}

#保存csv
write.csv(data.output, row.names = F,".\\output\\output_all.csv")

#作图
library(ggplot2)
data.output$col.max <- as.factor(data.output$col.max)

ggplot(data.output,aes(x=IF, y=绝热温变/卸载功.相变., color=col.max,group=col.max))+
  geom_line()+
  labs(x = "IF_total", y = "deltaT_ad/deltaW_unloading", title = "定deltaW_ul，拟合函数：y=k/deltaW_ul*g(x)+k，IF_ul=g(IF_total)")+
  scale_colour_hue("range(deltaW_ul)")

ggplot(data.output,aes(x=IF/卸载功.相变., y=绝热温变/卸载功.相变., color=col.max,group=col.max))+
  geom_line()+
  labs(x = "IF_total/deltaW_unloading", y = "deltaT_ad/deltaW_unloading", title = "定deltaW_ul，拟合函数：y=k*g(x)+k，IF_ul/deltaW_ul=g(IF_total/deltaW_ul)")+
  scale_colour_hue("range(deltaW_ul)")


