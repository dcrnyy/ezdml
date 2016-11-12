unit ezdmlstrs;

interface
{
升级过程：
1.修改工程的版本号，以及下方的版本号版本日期
2.修改README.TXT中的版本号和版本说明
3.修改README_CHS.TXT中的版本号和版本说明
4.更新语言包
5.编译所有工程
6.修改安装包中的版本号
7.编译并测试安装包（英文系统也要）和绿色版
8.修改中英文HTML中的版本信息
9.上传
}

const
  srEzdmlVersionNum = '2.14';
  srEzdmlVersionDate = '2016-11-12';

resourcestring
  {srEzdmlAppTitle = '表结构设计器';
  srEzdmlExiting = '正在退出..';
  srEzdmlLoading = '正在加载..';
  srEzdmlOpeningFileFmt = '正在加载文件 %s...';
  srEzdmlOpenFile = '打开文件';
  srEzdmlOpening = '正在打开...';
  srEzdmlAbortOpening = '确定要中止打开吗?';
  srEzdmlConfirmClearAll = '新建文件将清除模型中的所有表，确定要继续吗？';
  srEzdmlConfirmClearOnOpen = '打开前是否要清除现有表模型？';
  srEzdmlSaveingFileFmt = '正在保存文件 %s...';
  srEzdmlSaveFile = '保存文件';
  srEzdmlSaving = '正在保存...';
  srEzdmlAbortSaving = '确定要中止保存吗?';
  srEzdmlSaved = '已保存: ';
  srEzdmlNew = '新文件'; }

  srAppTitle = 'EZDML';
  srEzdmlAppTitle = 'EZDML';
  srEzdmlAbortSaving = 'Are you sure to abort saving?';
  srEzdmlSaved = 'Saved: ';
  srEzdmlExiting = 'Exiting..';
  srEzdmlLoading = 'Loading..';
  srEzdmlOpeningFileFmt = 'Opening file %s...';             
  srEzdmlOpenLastFileFmt = 'Open last recent file %s?';
  srEzdmlOpenFile = 'Open file';
  srEzdmlOpening = 'Opening...';
  srEzdmlAbortOpening = 'Are you sure to abort opening file?';
  srEzdmlConfirmClearAll = 'Create new file will clear all tables in the model file. Are you sure to continue?';
  srEzdmlConfirmClearOnOpen = 'Do you want to clear current model before open the file?';
  srEzdmlSaveingFileFmt = 'Saving file %s...';
  srEzdmlSaveFile = 'Save file';
  srEzdmlSaving = 'Saving...';
  srEzdmlNew = 'New File';         
  srEzdmlFileNotFoundFmt = 'File not found %s';
  srEzdmlTmpFileChangedFmt = 'The file date of temp file %s conflicts with the source file, we strongly recommend to backup and check your files. Are you still want to load the temp file?'#13#10'(Yes=Open tmp file, No=Open source file, Cancel=Do not open)';
  srEzdmlLoadTmpFileFailFmt ='Failed to load tmp file: %s'#13#10'Error info: %s'#13#10'This may cause by version error or file damage. Please try old version or delete the tmp file';  
  srEzdmlConfirmOpenUrlFmt = 'Are you sure to open URL %s with your internet explorer?';
  srEzdmlConfirmEditTextFmt = 'Open %s to edit now?';
  srEzdmlConfirmEditedTextFmt = '%s opened, please edit it now, and click OK to apply it when finished.';
  srEzdmlAbout = 'EZDML V%s %s (Freeware)'#13#10'http://www.ezdml.com'#13#10'http://blog.csdn.net/huzgd/'#13#10'huzzz@163.com'#13#10'QQ group: 344282607'#13#10'Forum: http://www.ezdml.com/bbs';
  srEzdmlConfirmExit = 'Do you want to save data before exit?';
  srEzdmlCreateGScriptTipFmt = 'Global Script file (%s) not found, do you want to create it?';
  srEzdmlFileAlreadyOpenedFmt = 'File %s is already open in another EZDML process.';

implementation

end.

