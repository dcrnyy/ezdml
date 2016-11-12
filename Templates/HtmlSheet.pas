var
  I: Integer;
  S,T,V,U, tbN, Tmp1,Tmp2: String;
  F: TCTMetaField;
  col:Integer;
begin
  Tmp1 := '<html>' + #13#10 +
'' + #13#10 +
'  <head>' + #13#10 +
'    <meta http-equiv="Content-Type" content="text/html; charset=gbk">' + #13#10 +
'    <title>' + #13#10 +
'      %OBJ_NAME%' + #13#10 +
'    </title>' + #13#10 +
'    <meta http-equiv="Content-Type" content="text/html; charset=gbk">' + #13#10 +
'    <style type="text/css">' + #13#10 +
'      TABLE { FONT-SIZE: 12px; COLOR: #000000 } .TitleLine { padding-left: 12px; FONT-WEIGHT:' + #13#10 +
'      bold; FONT-SIZE: 12px; COLOR: #ffffff; background-color: #578b43; height:' + #13#10 +
'      28px; padding-top:6px; } .TitleLine2 { padding-left: 12px; FONT-WEIGHT:' + #13#10 +
'      bold; FONT-SIZE: 12px; COLOR: #ffffff; background-color: #578b43; height:' + #13#10 +
'      20px; padding-top:4px; } .Titleline2_bg{ background-color:#f3f3ec; border-bottom:2px' + #13#10 +
'      solid #578b43; padding:10px; } .Titleline2td{ height:18px; } .vt9_title' + #13#10 +
'      { font-weight: bold; font-size: 12px; color: #2F2F2F; background-color:' + #13#10 +
'      #CCCCCC; height: 20px; line-height: 24px; } .T9_blackB { FONT-WEIGHT: bold;' + #13#10 +
'      FONT-SIZE: 12px; COLOR: #008000; LINE-height: 25px; TEXT-DECORATION: none;' + #13#10 +
'      TEXT-align: left; background-color:#cecece; padding-left:10px; }' + #13#10 +
'    </style>' + #13#10 +
'  </head>' + #13#10 +
'' + #13#10 +
'<body><center>' + #13#10 +
'' + #13#10 +
'<table cellSpacing="1" cellPadding="0" width="660" border="0" id="table7">' + #13#10 +
'  <tr>' + #13#10 +
'  <td class="TitleLine" colSpan="4" >' + #13#10 +
'    %OBJ_NAME%' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10 +
'  <tr>' + #13#10 +
'  <td bgColor="#ffffff" colSpan="4" height="1">' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10 +
'  <tr class="vt9_title">' + #13#10 +
'  <td noWrap width="100%" colSpan="4" >' + #13#10 +
'    &nbsp; %OBJ_NAME%信息管理' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10 +
'  <tr>' + #13#10 +
'  <td colSpan="4" height="5">' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10 +
'  <tr>' + #13#10 +
'  <td class="T9_black" colSpan="4">' + #13#10 +
'    &nbsp; 提示：' + #13#10 +
'      <span id="Cabp_Control_errtip" style="COLOR: red">' + #13#10 +
'      请确保填写的信息真实、正确、有效。' + #13#10 +
'      </span>' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10 +
'  <tr>' + #13#10 +
'  <td colSpan="4" height="5">' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10 +
'  <tr>' + #13#10 +
'  <td class="T9_blackB" colSpan="2">' + #13#10 +
'    %OBJ_NAME%信息' + #13#10 +
'  </td>' + #13#10 +
'  <td class="T9_blackB" colSpan="2">' + #13#10 +
'    请注意：带有*的项目必须填写。' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10 +
'  <tr height="1"><td width="100"></td><td width="230"><td width="100"></td><td width="230"></td></tr>' + #13#10+ #13#10 ;
  Tmp2 :=
'  <tr>' + #13#10 +
'  <td height="12" colSpan="4" >' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10 +
'  <tr>' + #13#10 +
'  <td height="4" class="T9_blackB" colSpan="4" >' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10 +
'  <tr>' + #13#10 +
'  <tr>' + #13#10 +
'  <td colSpan="4">' + #13#10 +
'    <input type="button" value="　添加　">' + #13#10 +
'    <input type="button" value="　修改　">' + #13#10 +
'    <input type="button" value="　查询　">' + #13#10 +
'    <input type="button" value="删除">' + #13#10 +
'    <input type="button" value="刷新">' + #13#10 +
'    <input type="button" value="返回">' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10 +
'</table>' + #13#10 +
'' + #13#10 +
'</center></body>' + #13#10 +
'' + #13#10 +
'</html>';

  tbN:=CurTable.Caption;
  if tbN='' then
    tbN:=CurTable.Name;
  S := StringReplace(Tmp1,'%OBJ_NAME%',tbN,[rfReplaceAll]);
  CurOut.Add(S);
  col := 1;
  for I:=0 to CurTable.MetaFields.Count-1 do
  begin
    F:=CurTable.MetaFields.Items[I];
    S:=F.Name;
    if (F.KeyFieldType=cfktId) or (F.KeyFieldType=cfktRid) then
      Continue;
    if F.DisplayName<>'' then
      S:=F.DisplayName;
    U:=S;
    if (F.KeyFieldType=cfktName) or not F.Nullable then
      U:='* '+S;
    V:=S+'1';
    if F.DefaultValue<>'' then
      V:=F.DefaultValue
    else if F.DataType=cfdtDate then
      V:=FormatDateTime('yyyy-mm-dd hh:nn',Now-20+I+Cos(I+Time))
    else if F.DataType=cfdtInteger then
      V:=IntToStr(Round(Sin(I+Time)*100))
    else if F.DataType=cfdtFloat then
      V:=Format('%f',[Abs(Sin(I+Time)*100)])
    else if F.DataType=cfdtBool then
    begin
      V:='是';
    end;
    
    if(Pos('内容',S)>0) or (Pos('说明',S)>0) or (Pos('备注',S)>0) or (Pos('列表',S)>0)
      or (F.DataLength>=9999) or (F.DataType = cfdtBlob) then
    begin
      if col=2 then
      begin
        CurOut.Add('<td></td><td></td></tr>');
        CurOut.Add('');
        col := 1;
      end;
      CurOut.Add('<tr>');
      CurOut.Add('<td>'+U+'</td>');
      T:='<td colspan="3"><textarea style="WIDTH: 491px; HEIGHT: ';
      if (F.DataType = cfdtBlob) then
        T:=T+'180'
      else
        T:=T+'72';
      T:=T+'" rows="1" cols="20">'+V+'</textarea></td>'
      CurOut.Add(T);
      col := 3;
      Continue;
    end;
    
    if col=3 then
    begin
      CurOut.Add('</tr>');
      CurOut.Add('');
      col := 1;
    end;
    if col=1 then
    begin
      CurOut.Add('<tr>');
    end;
    CurOut.Add('<td>'+U+'</td>');
    if (F.DataType = cfdtBool) then
      T:='<td><input type="checkbox" checked/>是</td>'
    else
    begin
      if(Pos('类型',S)>0) or (Pos('类别',S)>0) or (Pos('所属',S)>0) or (Pos('状态',S)>0) or (Pos('级别',S)>0) or (Pos('选择',S)>0)
        or (Pos('时间',S)>0) or (Pos('日期',S)>0) or (F.DataType = cfdtDate) or (F.DataType = cfdtEnum) then
        T:='<td><select style="WIDTH: 162px"><option selected>'+V+'</option></select></td>'
      else
        T:='<td><input style="WIDTH: 162px" value="'+V+'"/></td>';
    end;
    CurOut.Add(T);
    col:=col+1;
  end;
  if col=2 then
  begin
    CurOut.Add('<td></td><td></td>');
    col:=3;
  end;
  if col=3 then
  begin
    CurOut.Add('</tr>');
    CurOut.Add('');
  end;
  S := StringReplace(Tmp2,'%OBJ_NAME%',tbN,[rfReplaceAll]);
  CurOut.Add(S);

end.