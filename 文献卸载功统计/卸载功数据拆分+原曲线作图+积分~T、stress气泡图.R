library(ggplot2)
library(stringr)#通配符提取字符串内容
library(reshape2)#melt，ggplot画多条曲线

rm(list = ls())#清空变量

fname = file.choose()
data_csv = read.csv(fname)
data_integrate = data.frame()
plot_origin <- ggplot()
if (dir.exists(paste(str_extract_all(fname,"[^(\\\\)]+.csv"),"_数据处理//",sep = "")) == F){#查询目录是否存在
  dir.create(paste(str_extract_all(fname,"[^(\\\\)]+.csv"),"_数据处理//",sep = "")) #创建文件夹
}

for (i in 2:ncol(data_csv)){
  data_temp = data_csv[,c(1,i)]
  data_temp = na.omit(data_temp)
  #保存Engauge Digitizer的导出数据
  write.csv(data_temp, row.names = F,
            paste(str_extract_all(fname,"[^(\\\\)]+.csv"),"_数据处理//",#通配符提取文件名
                  colnames(data_temp)[2],"_",round(data_temp[nrow(data_temp),2]),"MPa",".csv",sep = "")) #paste字符串拼接
  #计算面积积分
  f <- approxfun(x = data_temp[,1], y = data_temp[,2], method = "linear")#线性插值函数
  data_integrate[i-1,1] = as.double(str_extract_all(colnames(data_temp)[2],"[0-9]+"))#通配符提取字符串中数字
  data_integrate[i-1,2] = data_temp[nrow(data_temp),2]
  data_integrate[i-1,3] = integrate(f, data_temp[1,1], data_temp[nrow(data_temp),1])[1] #线性插值函数面积积分
}
names(data_integrate) = c("Temperature","Stress","Integrate")#列名
write.csv(data_integrate, row.names = F,
          paste(str_extract_all(fname,"[^(\\\\)]+.csv"),"_数据处理//",
                                str_extract_all(fname,"[^(\\\\)]+.csv"),"_积分结果.csv",sep = ""))#通配符提取文件名




#*******************原始拉伸曲线图*********************
#ggplot画多条曲线
data_csv_melt <- melt(data_csv, id='x')
data_csv_melt = na.omit(data_csv_melt)
names(data_csv_melt) <- c('x', 'cycle', 'value')
ggplot() +
  geom_line(data = data_csv_melt, aes(x = x, y = value, color = cycle), size = 1)+#
  xlab("Strain (%)") +
  ylab("Stress (MPa)")+
  theme_light()


#**********************气泡图*******************************
# 根据..填充不同颜色；根据..选择不同点大小。
# 在数据量大或有重复点时，更改 alpha 值是不错的选择
ggplot(data_integrate, aes(data_integrate[,1], data_integrate[,2])) +
  geom_point(aes(color = data_integrate[,3], size=data_integrate[,3]), shape=19, alpha=0.5)+
  theme(panel.background = element_rect(fill='transparent', color='black'),
        plot.title = element_text(size = 15,hjust=0.5,margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),), 
        legend.title=element_blank(),
        axis.title.y = element_text(size = 15,hjust = 0.5),
        axis.title.x = element_text(size = 15,hjust = 0.5),
        axis.ticks.length = unit(-0.5,"lines"),
        text=element_text(size=16),
        axis.text.y = element_text(size = 13,margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),),
        axis.text.x = element_text(size = 13,margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),),
        panel.grid = element_line(colour = "grey90"))+
  #scale_x_continuous(limits = c(0,500))+
  #xlim(0,500)+
  scale_color_gradient(low="grey10", high="red")+
  scale_size_continuous(range=c(2,16))+
  #geom_label(nudge_x = 0.2, nudge_y = -0.05, label = round(digits = 2, data_integrate[,3]), size = 4,label.size = 0, family = "sans")+
  geom_text(label = round(digits = 2, data_integrate[,3]), size = 3.5, nudge_y = -0.3,)+ 
  #ggtitle("Unloading work per unit volume")+
  labs(x=colnames(data_integrate)[1],y=colnames(data_integrate)[2],title = "Unloading work per unit volume")    
  #theme_zg(base_size=16, bg='gray90')#source("theme_zg.R")
  
  
  #scale_x_log10()+
  #expand_limits(x=c(1,1000),y = c(398,402)) #expand_limits设置坐标轴范围



  



