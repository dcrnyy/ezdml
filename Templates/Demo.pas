<%
//本示例演示如何编写模板脚本
var
  I: Integer;
  S: String;
  F: TCTMetaField;
begin
%>
大家好！本内容由模板脚本生成，模板文件保存在程序目录下的Templates文件夹中。
注：本示例采用了类似于JSP和ASP的页面模板技术

当前模型中的表：数目${AllModels.CurDataModal.Tables.Count}

<%
  for I:=0 to AllModels.CurDataModal.Tables.Count-1 do
  begin
    CurOut.Add('表'+IntToStr(I+1)+' '+AllModels.CurDataModal.Tables[I].Name+'['+AllModels.CurDataModal.Tables[I].Caption+']');
  end;
  
  CurOut.Add('');
  CurOut.Add('当前表名为：'+CurTable.Name);
  if CurTable.Caption='' then
    CurOut.Add('逻辑名称：没有指定')
  else
    CurOut.Add('逻辑名称：'+CurTable.Caption);
  CurOut.Add('');
  CurOut.Add(Format('字段数目：%d',[CurTable.MetaFields.Count]));
  for I:=0 to CurTable.MetaFields.Count-1 do
  begin
    F:=CurTable.MetaFields.Items[I];
    S:=F.Name;
    if F.DisplayName<>'' then
      S:=S+'['+F.DisplayName+']';
%>
  字段${I+1}：${S}，(${F.GetLogicDataTypeName})
<%
  end;
%>

下面是一个长长的SQL：

<%
  S:='select ';
  for I:=0 to CurTable.MetaFields.Count-1 do
  begin
    F:=CurTable.MetaFields.Items[I];
    S:=S+UpperCase(F.Name);
    if I<>CurTable.MetaFields.Count-1 then
    S:=S+', ';
  end;
  S:=S+' from '+UpperCase(CurTable.Name);
  CurOut.Add(S);
%>

示例结束

<%
end.
%>