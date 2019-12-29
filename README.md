# ruijin

library(ruijin)

df=rj_labtest.getdata(dir='F:/实验室指标')

rs=rj_labtest.query(data = df,
                 query = c(   
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'白细胞计数',    
                        '红细胞计数',                           
                           '血红蛋白',                           
                           '中性粒细胞%',  
                           '淋巴细胞%',   
                           '单核细胞%',       
                           '血小板计数',   
                           '葡萄糖',   
                           '丙氨酸氨基转移酶',    
                           '天门冬氨酸氨基转移酶',  
                           '碱性磷酸酶', 
                           'γ-谷氨酰基转移酶',   
                           '总胆红素','直接胆红素',    
                           '总蛋白','白蛋白','白球比例',    
                           '胆汁酸','尿素','肌酐','尿酸',    
                           '甘油三酯','总胆固醇',   
                           '高密度脂蛋白胆固醇',   
                           '低密度脂蛋白胆固醇',   
                           '载脂蛋白AI','载脂蛋白B',   
                           '脂蛋白(a)',    
                           '二小时血糖','糖化血红蛋白(HbA1C)',   
                           '胰岛素(INS)(空腹)','胰岛素(INS)(二小时) ',   
                           '乳酸脱氢酶','肌酸激酶','CK-MB质量',    
                           '肌红蛋白定量','肌钙蛋白I',    
                           'C反应蛋白(高敏)(hsCRP)',    
                           '尿液肌酐','尿微量白蛋白','尿白蛋白比肌酐',   
                           '氨基末端B型利钠肽前体',     
                           'APTT','PT','INR','TT','Fg','纤维蛋白降解产物','D-二聚体定量'))
                        
rs
