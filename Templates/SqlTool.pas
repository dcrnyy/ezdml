(*[SettingsPanel]
Control="Edit";Name="Format";Caption="Enter Format:";Value="SQL.Add('%s');";Params="[FULLWIDTH]"
[/SettingsPanel]*)

var
  I: Integer;
  V: string;
begin
  V:=GetParamValue('Format');
  //alert(V);
  if Trim(V)='' then
  begin
    CurOut.Add('Please enter a format from settings.');
    Exit;
  end;
  
  with CurTable do
  begin
    for I := 0 to MetaFields.Count - 1 do
      CurOut.Add(Format(V,[MetaFields[I].Name]));
  end;
end.
