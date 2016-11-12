表结构设计器(EZDML) V2.13(免费软件)

这是一个数据库建表的小软件，可快速的进行数据库表结构设计，建立数据模型。类似大家常用的数据库建模工具如PowerDesigner、ERWIN、ER-Studio和Rational-Rose等的超级精简版。

包含功能：
1. 表结构设计：创建表、字段、主键、外键、索引和注释；
2. 表描述：可直接编辑文字描述快速生成表结构，爱用键盘的人会喜欢这个功能；
3. 模型图：自动生成模型图；可设计和显示物理／逻辑视图，支持自动布局、平移、缩小放大等操作；
4. 导入数据库：导入数据库中的已有对象，即所谓的逆向工程（支持ORACLE、MYSQL、SQLSERVER和ODBC）；
5. 生成数据库：可生成直接创建数据库的SQL脚本；也可与现有数据库比较，然后生成同步脚本的功能，不至于丢失现有数据。
6. 生成代码：目前可生成C++、Pas、C#和Java的简单类定义代码。


使用说明：
1. 系统启动时会自动加载最后一次使用的文件；
2. 模型图中可按加减号放大缩小，方向键平移，R复原，F居中；
3. 表的描述字中，可以输入简写字母代替数据类型，S,I,F,D,BO,E,BL,O分别代表文本,整数,浮点数,时间,真假,枚举,文件,对象，如果不写，默认为字符串

示例：

测试表
--------
ID  PK
RID FK
标题 S(200)
数量 I
单价 F(10,2)
日期 D
Desc 描述
TpNa 类名
注释

4. 外键关系可以在模型图上创建和显示，也可以在字段的关联表字段里设置；
5. 导入生成功能可支持ORACLE、MYSQL、SQLSERVER和ODBC数据库；
6. 生成功能在未连接数据库时，生成的是创建数据库的初始化SQL；只有连接了数据库，才会与数据库进行比对生成升级SQL；
7. 为避免数据丢失，同步数据库时不会删除字段，但仍然会生成注释形式的相应SQL；
8. 本软件完全免费；如因使用本软件造成损失，作者不承担任何责任。

修改INI文件自定义配置（菜单：工具｜修改INI配置）：
[DefaultFieldTypes]
1=String:VARSTR(2000)
2=Integer:DECIMAL
[CustFieldTypes]
1=BigInt
2=Decimal
3=TestUnk
[CustDataTypeReplaces]
1=VARCHAR2:NVARCHAR2
2=NUMBER(10):DECIMAL
3=%TEXT%:NCLOB
[Options]
AutoSaveMinutes=5
FieldNameMaxDrawSize=64
FieldTypeMaxDrawSize=48
CreateSeqForOracle=0
OCIDLL=D:\oracle\ora112\instantclient\oci.dll

解释一下INI配置：
[DefaultFieldTypes] ——修改缺省数据类型对应的物理类型
1=String:VARSTR(2000) ——修改逻辑类型名为String的物理类型名为VARSTR，默认长度为2000
2=Integer:DECIMAL ——修改逻辑类型名为Integer的物理类型名为DECIMAL，无默认长度
[CustFieldTypes] ——添加额外的数据类型到类型下拉列表
1=BigInt
2=Decimal
3=TestUnk
[CustDataTypeReplaces] ——指定替换字段类型
1=VARCHAR2:NVARCHAR2 ——将VARCHAR2替换为NVARCHAR2（全字匹配）
2=NUMBER(10):DECIMAL ——将NUMBER(10)替换为DECIMAL（全字匹配）
3=%TEXT%:NCLOB ——将TEXT替换为NCLOB（模糊匹配，类似SQL中的like操作）
4=%RAW:BLOB ——将RAW替换为BLOB（左边模糊匹配，右边精确匹配，类似SQL中的like操作）
[Options]
AutoSaveMinutes=5  ——定时每5分钟自动保存，保存为同目录.tmp文件（打开时自动加载tmp，想恢复加载原始文件的话要先删除tmp），为0则不保存
FieldNameMaxDrawSize=64  ——在模型图上允许显示的最大字段名长度
FieldTypeMaxDrawSize=48  ——在模型图上允许显示的最大字段类型长度
CreateSeqForOracle=0  ——禁止为ORACLE表生成序列号
OCIDLL=D:\oracle\ora112\instantclient\oci.dll  ——指定OCI.dll的路径（用于instant client或装了多个ORACLE client时）


huzzz@163.com
http://www.ezdml.com
QQ群: 344282607
论坛：http://www.ezdml.com/bbs

版本历史：
2009年2月18日 V1.21
该工具开始开发的时间是2006年4月，最近改进了一下，因此初始版本号取为1.2。

2009年2月22日 V1.22
解决无法指定整数和浮点数字段精度的问题，可在字段属性中用逗号分隔同时指定长度和精度（如12,4）。

2009年6月5日 V1.23
作了一些改进，如导入导出、外键连线，增加对象、方法、事件类型。

2009年7月15日 V1.24
改正了只能生成最后一个索引SQL的问题，尝试处理索引名称过长的问题，对非法和重复的字段名进行检测。

2009年8月29日 V1.30
增加对SQLSERVER和ODBC的导入和生成功能；支持添加多个模型图；生成C++代码。

2009年9月11日 V1.32
生成DML和DQL SQL，导入和生成表注释，模型图中直接添加外键关联，自动记录最后一次的数据库连接信息，另外解决了其它的一些BUG。

2009年9月25日 V1.34
模型图颜色和选项设置、复制图像、BUG修复。

2009年10月11日 V1.35
使用大程序图标，默认语言改为英语。

2009年11月8日 V1.40
支持MYSQL，修复一些BUG

2009年11月25日 V1.42, 2009年11月29日 V1.50
导出到Excel, 以及其它一些改进和BUG修复.

2009年12月19日 V1.52
BUG修复.

2010年4月20日 V1.55
支持PASCAL脚本的模板生成，BUG修复.

2010年9月6日 V1.59
Pascal脚本参数面板，Bug修复.

2010年12月1日 V1.62
查找并从已有表中导入字段，BUG修复.

2010年12月20日 V1.63
支持MySql和SQLServer的自动增长编号字段类型.

2011年3月15日 V1.64
支持拖放文件打开和最近文件列表.

2011年4月7日 V1.71
支持SQLSERVER的表字段注释，增强了对SQLSERVER2005的支持，修复一些BUG。

2011年4月23日 V1.77
对多个表进行批量添加或导入字段，为外键生成创建索引SQL，修复无法导入ORACLE索引的BUG。

2011年5月1日 V1.79
对主键和连线进行颜色设置，为模型图增加滚动条。

2011年5月15日 V1.80
支持在ORACLE和SQLServer之间备份和恢复数据。

2011年6月8日 V1.81
在目录树的右键菜单中增加批量添加删除字段的功能，解决一些BUG。

2011年7月17日 V1.82
文字描述支持新的格式，如"CustName 顾客名称 S(100)". Bug修复.

2011年7月25日 V1.85
支持NSDOA（BA平台数据服务）连接ORACLE. Bug修复.

2011年9月16日 V1.88
支持文本或SQL块. Bug修复.

2011年10月16日 V1.91
支持缩略图模式.

2011年11月2日 V1.92
增加SQL工具，可执行简单SQL.

2012年10月20日 V1.95
增加HTML表单生成和预览. Bug修复.

2012年10月28日 V1.96
支持多字段复合主键，选表生成数据库. Bug修复.

2014年04月27日 V1.97
退出和启动时自动保存和加载最后一次内容，其它一些功能改进和Bug修复.

2014年09月29日 V1.98
特殊拷贝功能，实体颜色自定义, PASCAL脚本支持页面模板（类似JSP和ASP）.

2014年12月07日 V1.99
定时自动保存，导出DOC时获取是否物理视图等环境参数，自定义新数据类型下拉，自定义数据库输出类型替换，Bug修复.

2015年02月09日 V2.01
自定义缺省数据类型，修改表时同步修改所有同名表，新增同时显示逻辑和物理名的模式，Bug修复。

2015年04月15日 V2.02
支持ORACLE instant-client，可不连数据库生成SQL，快速加载tmp文件时检查文件时间，Bug修复。

2015年9月7日 V2.03
Bug修复.

2015年10月11日 V2.04
支持Sqlite，Bug修复.

2015年10月19日 V2.06
支持PostgreSQL，支持全局脚本接管事件生成自定义SQL，支持加密保存数据连接密码，Bug修复.

2015年12月26日 V2.11
导出带模型图、界面和代码的word或mht格式文档，支持UTF-8字符集的ORACLE数据库，Bug修复.

2016年5月15日 V2.13
重复打开文件时提示，连线接近时自动拉直，ORACLE序列号开关，新建表模板，字段属性从EXCEL粘贴，Bug修复.

