import os


fdname_common = input("样品名")
num_files = int(input("文件数"))
fname_common = "Specimen_RawData_1"
path = os.getcwd()
num_del = 8 #int(input('删除行数'))
path_export = path + r"\\" + "R_卸载功统计"

folder = os.path.exists(path_export)
if not folder:  # 判断是否存在文件夹如果不存在则创建为文件夹
    os.makedirs(path_export)

for i in range(1, num_files+1):
    fname = fname_common + ".csv"
    try:
        fnew = open(path_export + r"\\" + fdname_common + "_" + str(i) + ".csv", "w")
        fobj = open(path + r"\\" + fdname_common + "_" + str(i) + ".is_tcyclic_RawData" + r"\\" + fname, 'r')  # 这里的a意思是追加，这样在加了之后就不会覆盖掉源文件中的内容，如果是w则会覆盖。
    except IOError:
        print('*** file open error:')
    else:
        lines = fobj.readlines()
        for ii in range(0, num_del):
            lines[ii] = ""   #删除行
        fnew.writelines(lines)
        fobj.close()  # 特别注意文件操作完毕后要close
        fnew.close()




