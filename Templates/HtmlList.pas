var
  I, J: Integer;
  S,T, tbN, Tmp1,Tmp2,Tmp3: String;
  F: TCTMetaField;
  col, listColCount:Integer;
begin
  Tmp1 := '<html>' + #13#10 +
'' + #13#10 +
'  <head>' + #13#10 +
'    <meta http-equiv="Content-Type" content="text/html; charset=gbk">' + #13#10 +
'    <title>' + #13#10 +
'      %OBJ_NAME%列表' + #13#10 +
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
'      TEXT-align: left; background-color:#cecece; padding-left:5px; }' + #13#10 +
'      .table_border td{border-top:1px #888 solid;border-right:1px #888 solid;padding-left:5px;}' + #13#10 +
'      .table_border{border-bottom:1px #888 solid;border-left:1px #888 solid;}' + #13#10 +
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
'    &nbsp; %OBJ_NAME%信息查询' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10 +
'  <tr>' + #13#10 +
'  <td colSpan="4" height="5">' + #13#10 +
'  </td>' + #13#10 +
'  </tr>' + #13#10  +
'  <tr height="1"><td width="100"></td><td width="230"><td width="100"></td><td width="230"></td></tr>' + #13#10 + #13#10;
  Tmp2 :=
#13#10'<tr>' + #13#10 +
'<td height="6" colSpan="4" >' + #13#10 +
'</td>' + #13#10 +
'</tr>' + #13#10 +
'<tr>' + #13#10 +
'<td></td>' + #13#10 +
'<td colspan="3">' + #13#10 +
'    <input type="button" value="　　查　询　　">' + #13#10 +
'    &nbsp;&nbsp; 提示：' + #13#10 +
'      <span id="Cabp_Control_errtip" style="COLOR: red">' + #13#10 +
'      请输入过滤条件进行查询。' + #13#10 +
'      </span>' + #13#10 +
'</td></tr>' + #13#10 +
'<tr>' + #13#10 +
'<td height="6" colSpan="4" >' + #13#10 +
'</td>' + #13#10 +
'</tr>' + #13#10 +
'<tr>' + #13#10 +
'<tr>' + #13#10 +
'<td height="2" class="T9_blackB" colSpan="4" >' + #13#10 +
'</td>' + #13#10 +
'</tr>' + #13#10 +
'<tr>' + #13#10 +
'<td height="6" colSpan="4" >' + #13#10 +
'</td>' + #13#10 +
'</tr>' + #13#10 +
'' + #13#10 +
'<tr>' + #13#10 +
'<tr>' + #13#10 +
'<td colSpan="4" >' + #13#10 +
'<div style="width=660px; height=300px;overflow:auto">' + #13#10 +
'<table class="table_border" border="0" cellspacing="0" cellpadding="0" width="%LIST_TABLE_WIDTH%">';
  Tmp3 :=
  '</table>' + #13#10 +
'</div>' + #13#10 +
'</td>' + #13#10 +
'</tr>' + #13#10 +
'' + #13#10 +
'<tr>' + #13#10 +
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
  listColCount:=0;
  for I:=0 to CurTable.MetaFields.Count-1 do
  begin
    F:=CurTable.MetaFields.Items[I];
    S:=F.Name;
    if (F.KeyFieldType=cfktId) or (F.KeyFieldType=cfktRid) then
      Continue;
    if F.DisplayName<>'' then
      S:=F.DisplayName;
    listColCount:=listColCount+1;
    if(Pos('内容',S)>0) or (Pos('说明',S)>0) or (Pos('备注',S)>0) or (Pos('列表',S)>0)
      or (F.DataLength>=9999) or (F.DataType=cfdtBlob) then
    begin
      Continue;
    end;
    
    if col=3 then
    begin
      CurOut.Add('</tr>');
      col := 1;
    end;
    if col=1 then
    begin
      CurOut.Add('');
      CurOut.Add('<tr>');
    end;
    CurOut.Add('<td>'+S+'</td>');
    if (F.DataType = cfdtBool) then
      T:='<td><input type="checkbox"/>是</td>'
    else if(Pos('类型',S)>0) or (Pos('类别',S)>0) or (Pos('所属',S)>0) or (Pos('状态',S)>0) or (Pos('级别',S)>0) or (Pos('选择',S)>0)
      or (Pos('时间',S)>0) or (Pos('日期',S)>0) or (F.DataType = cfdtDate) or (F.DataType=cfdtEnum) then
    begin
      if (F.DataType = cfdtInteger) or (F.DataType = cfdtFloat) or (F.DataType = cfdtDate) then
        T:='<td><select style="WIDTH: 90px"></select>-<select style="WIDTH: 90px"></select></td>'
      else
        T:='<td><select style="WIDTH: 186px"></select></td>';
    end
    else
    begin
      if (F.DataType = cfdtInteger) or (F.DataType = cfdtFloat) or (F.DataType = cfdtDate) then
        T:='<td><input style="WIDTH: 90px" />-<input style="WIDTH: 90px" /></td>'
      else
        T:='<td><input style="WIDTH: 186px" /></td>';
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
  end;
  CurOut.Add('');
  
  S := StringReplace(Tmp2,'%OBJ_NAME%',tbN,[rfReplaceAll]);
  S := StringReplace(Tmp2,'%LIST_TABLE_WIDTH%',IntToStr(listColCount*120+30),[rfReplaceAll]);
  CurOut.Add(S);

  for J:=0 to 10 do
  begin
    if J=0 then
      CurOut.Add('<tr height="25" class="T9_blackB">')
    else
      CurOut.Add('<tr height="25">');
    col := 1;
    for I:=0 to CurTable.MetaFields.Count-1 do
    begin
      F:=CurTable.MetaFields.Items[I];
      S:=F.Name;
      if (F.KeyFieldType=cfktId) or (F.KeyFieldType=cfktRid) then
        Continue;
      if F.DisplayName<>'' then
        S:=F.DisplayName;

      if J>0 then
      begin
        S:=S+IntToStr(J);
        if F.DefaultValue<>'' then
          S:=F.DefaultValue
        else if F.DataType=cfdtDate then
          S:=FormatDateTime('yyyy-mm-dd hh:nn',Now-20+J+I+Cos(J+Time))
        else if F.DataType=cfdtInteger then
          S:=IntToStr(J)
        else if F.DataType=cfdtFloat then
          S:=Format('%f',[Abs(Sin(J+I+Time)*100)])
        else if F.DataType=cfdtBool then
        begin
          if (J mod 3)=0 then
            S:='否'
          else
            S:='是';
        end
      end;
      if col=1 then
        CurOut.Add('<td width="150">'+S+'</td>')
      else
        CurOut.Add('<td width="120">'+S+'</td>');
      col:=col+1;
    end;
    CurOut.Add('</tr>');
  end;
  
  S := StringReplace(Tmp3,'%OBJ_NAME%',tbN,[rfReplaceAll]);
  CurOut.Add(S);
end.