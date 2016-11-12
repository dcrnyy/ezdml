unit ezdmlstrs;

interface
{
�������̣�
1.�޸Ĺ��̵İ汾�ţ��Լ��·��İ汾�Ű汾����
2.�޸�README.TXT�еİ汾�źͰ汾˵��
3.�޸�README_CHS.TXT�еİ汾�źͰ汾˵��
4.�������԰�
5.�������й���
6.�޸İ�װ���еİ汾��
7.���벢���԰�װ����Ӣ��ϵͳҲҪ������ɫ��
8.�޸���Ӣ��HTML�еİ汾��Ϣ
9.�ϴ�
}

const
  srEzdmlVersionNum = '2.14';
  srEzdmlVersionDate = '2016-11-12';

resourcestring
  {srEzdmlAppTitle = '��ṹ�����';
  srEzdmlExiting = '�����˳�..';
  srEzdmlLoading = '���ڼ���..';
  srEzdmlOpeningFileFmt = '���ڼ����ļ� %s...';
  srEzdmlOpenFile = '���ļ�';
  srEzdmlOpening = '���ڴ�...';
  srEzdmlAbortOpening = 'ȷ��Ҫ��ֹ����?';
  srEzdmlConfirmClearAll = '�½��ļ������ģ���е����б�ȷ��Ҫ������';
  srEzdmlConfirmClearOnOpen = '��ǰ�Ƿ�Ҫ������б�ģ�ͣ�';
  srEzdmlSaveingFileFmt = '���ڱ����ļ� %s...';
  srEzdmlSaveFile = '�����ļ�';
  srEzdmlSaving = '���ڱ���...';
  srEzdmlAbortSaving = 'ȷ��Ҫ��ֹ������?';
  srEzdmlSaved = '�ѱ���: ';
  srEzdmlNew = '���ļ�'; }

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

