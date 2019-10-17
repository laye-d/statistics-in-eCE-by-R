import os
import pandas as pd
import openpyxl


fdname_common = input("样品名:")
num_files = int(input("文件数:"))
fname_common = "Specimen_RawData_1"
fname = fname_common + ".csv"
path = os.getcwd()
path_export = path + r"\\" + "处理后数据"


folder = os.path.exists(path_export)
if not folder:  # 判断是否存在文件夹如果不存在则创建为文件夹
    os.makedirs(path_export)

for i in range(1, num_files+1):
    csv = pd.read_csv(path + r"\\" + fdname_common + "_" + str(i) + ".is_tcyclic_RawData" + r"\\" + fname, sep=',', encoding="gbk", engine='python', skiprows=6)#,error_bad_lines=False
    csv.to_excel(path_export + r"\\" + fdname_common + "_" + str(i) + ".xlsx", sheet_name='data')






