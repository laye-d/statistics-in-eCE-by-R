#批量处理：拉伸曲线统计卸载功&超弹性应变&加载功及内耗
rm(list = ls())#清空变量



####################################### 参数 ###########################################
function.name = "批量处理_processfunc：拉伸曲线统计卸载功&超弹性应变&加载功及内耗【v19.11.6】（使用cubintegrate积分）.R"

no.range_1cycle = c(1:4) #1次循环的文件序号范围
no.range_2cycle = c(5:13) #2次循环的文件序号范围
no.range_3cycle = c() #3次循环的文件序号范围

stress_loading = 900#应力
stress_unloading = 5#应力
sample.name = "Ti-50.6Ni_longitudinal" #样品名

no.range = c(1:13) #总的待整理的序号范围
########################################################################################



source(function.name)

#处理1次循环文件
for(i in no.range_1cycle){
  process.func(sample.name = sample.name,
               file.no = i,#待处理文件序号
               num_cycle = 1,#循环数
               stress_loading = stress_loading,
               stress_unloading = stress_unloading)
}
#处理2次循环文件
for(i in no.range_2cycle){
  process.func(sample.name = sample.name,
               file.no = i,#待处理文件序号
               num_cycle = 2,#循环数
               stress_loading = stress_loading,
               stress_unloading = stress_unloading)
}
#处理3次循环文件
for(i in no.range_3cycle){
  process.func(sample.name = sample.name,
               file.no = i,#待处理文件序号
               num_cycle = 3,#循环数
               stress_loading = stress_loading,
               stress_unloading = stress_unloading)
}





data.collect = data.frame()
for(i in no.range){
  fname = paste(sample.name,"_",i,".csv_数据处理/",sample.name,"_",i,".csv_积分结果.csv",sep = "")
  data.temp = read.csv(fname, )
  data.temp = data.frame(rep(i,nrow(data.temp)), data.temp)
  for(ii in 1:nrow(data.temp)){
    data.collect = rbind(data.collect,data.temp[ii,]) 
  }
}
names(data.collect)[1] = "文件序号"

#*************************保存csv文件****************************
write.csv(data.collect, row.names = F,
          paste(sample.name,
                "_批量处理结果汇总.csv",sep = ""))#通配符提取文件名


