unit Registry; 
 
{$R-,T-,H+,X+} 
 
interface 
 
 
{$IFDEF FPC}
    uses {$IFDEF LINUX} WinUtils, {$ENDIF} Windows, Classes, SysUtils, IniFiles;
    {$ELSE}
   uses {$IFDEF LINUX} WinUtils, {$ENDIF} winapi.Windows, system.Classes, system.SysUtils, system.IniFiles;
   {$endif}

 
type 
  ERegistryException = class(Exception); 
 
  TRegKeyInfo = record 
    NumSubKeys: Integer; 
    MaxSubKeyLen: Integer; 
    NumValues: Integer; 
    MaxValueLen: Integer; 
    MaxDataLen: Integer; 
    FileTime: TFileTime; 
  end; 
 
  TRegDataType = (rdUnknown, rdString, rdExpandString, rdInteger, rdBinary); 
 
  TRegDataInfo = record 
    RegData: TRegDataType; 
    DataSize: Integer; 
  end; 
 
  TRegistry = class(TObject) 
  private 
    FCurrentKey: HKEY; 
    FRootKey: HKEY; 
    FLazyWrite: Boolean; 
    FCurrentPath: string; 
    FCloseRootKey: Boolean; 
    FAccess: LongWord; 
    procedure SetRootKey(Value: HKEY); 
  protected 
    procedure ChangeKey(Value: HKey; const Path: string); 
    function GetBaseKey(Relative: Boolean): HKey; 
    function GetData(const Name: string; Buffer: Pointer; 
      BufSize: Integer; var RegData: TRegDataType): Integer; 
    function GetKey(const Key: string): HKEY; 
    procedure PutData(const Name: string; Buffer: Pointer; BufSize: Integer; RegData: TRegDataType); 
    procedure SetCurrentKey(Value: HKEY); 
      public 
    constructor Create; overload; 
    constructor Create(AAccess: LongWord); overload; 
    destructor Destroy; override; 
    procedure CloseKey; 
   function CreateKey(const Key: string): Boolean; 
    function DeleteKey(const Key: string): Boolean; 
    function DeleteValue(const Name: string): Boolean; 
 
   function GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean; 
    function GetDataSize(const ValueName: string): Integer; 
    function GetKeyInfo(var Value: TRegKeyInfo): Boolean; 
    procedure GetKeyNames(Strings: TStrings); 
    procedure GetValueNames(Strings: TStrings); 
    function KeyExists(const Key: string): Boolean; 
     function OpenKey(const Key: string; CanCreate: Boolean): Boolean; 
    function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer; 
    function ReadInteger(const Name: string): Integer; 
    function ReadString(const Name: string): string; 
    procedure RenameValue(const OldName, NewName: string); 
    function ValueExists(const Name: string): Boolean; 
    procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer); 
    procedure WriteInteger(const Name: string; Value: Integer); 
    procedure WriteString(const Name, Value: string); 
    procedure WriteExpandString(const Name, Value: string); 
    property CurrentPath: string read FCurrentPath; 
    property CurrentKey: HKEY read FCurrentKey; 
    property LazyWrite: Boolean read FLazyWrite write FLazyWrite; 
   property RootKey: HKEY read FRootKey write SetRootKey; 
  end; 
 
resourcestring 
  SInvalidRegType = 'Invalid data type for ''%s'''; 
  SRegCreateFailed = 'Failed to create key %s'; 
  SRegGetDataFailed = 'Failed to get data for ''%s'''; 
  SRegSetDataFailed = 'Failed to set data for ''%s'''; 
 
 
 
implementation 
 {$IFDEF FPC}
    uses RTLConsts;
    {$ELSE}
   uses system.RTLConsts;
   {$endif}

 
 
procedure ReadError(const Name: string); 
begin 
  raise ERegistryException.CreateResFmt(@SInvalidRegType, [Name]); 
end; 
 
function IsRelative(const Value: string): Boolean; 
begin 
  Result := not ((Value <> '') and (Value[1] = '\')); 
end; 
 
function RegDataToDataType(Value: TRegDataType): Integer; 
begin 
  case Value of 
    rdString: Result := REG_SZ; 
    rdExpandString: Result := REG_EXPAND_SZ; 
    rdInteger: Result := REG_DWORD; 
    rdBinary: Result := REG_BINARY; 
  else 
    Result := REG_NONE; 
  end; 
end; 
 
function DataTypeToRegData(Value: Integer): TRegDataType; 
begin 
  if Value = REG_SZ then Result := rdString 
  else if Value = REG_EXPAND_SZ then Result := rdExpandString 
  else if Value = REG_DWORD then Result := rdInteger 
  else if Value = REG_BINARY then Result := rdBinary 
  else Result := rdUnknown; 
end; 
 
constructor TRegistry.Create; 
begin 
  RootKey := HKEY_CURRENT_USER; 
  FAccess := KEY_ALL_ACCESS; 
  LazyWrite := True; 
end; 
 
constructor TRegistry.Create(AAccess: LongWord); 
begin 
  Create; 
  FAccess := AAccess; 
end; 
 
destructor TRegistry.Destroy; 
begin 
  CloseKey; 
  inherited; 
end; 
 
procedure TRegistry.CloseKey; 
begin 
  if CurrentKey <> 0 then 
  begin 
    if LazyWrite then 
      RegCloseKey(CurrentKey) else 
      RegFlushKey(CurrentKey); 
    FCurrentKey := 0; 
    FCurrentPath := ''; 
  end; 
end; 
 
procedure TRegistry.SetRootKey(Value: HKEY); 
begin 
  if RootKey <> Value then 
  begin 
    if FCloseRootKey then 
    begin 
      RegCloseKey(RootKey); 
      FCloseRootKey := False; 
    end; 
    FRootKey := Value; 
    CloseKey; 
  end; 
end; 
 
procedure TRegistry.ChangeKey(Value: HKey; const Path: string); 
begin 
  CloseKey; 
  FCurrentKey := Value; 
  FCurrentPath := Path; 
end; 
 
function TRegistry.GetBaseKey(Relative: Boolean): HKey; 
begin 
  if (CurrentKey = 0) or not Relative then 
    Result := RootKey else 
    Result := CurrentKey; 
end; 
 
procedure TRegistry.SetCurrentKey(Value: HKEY); 
begin 
  FCurrentKey := Value; 
end; 
 
function TRegistry.CreateKey(const Key: string): Boolean; 
var 
  TempKey: HKey; 
  S: string; 
  Disposition: Integer; 
  Relative: Boolean; 
begin 
  TempKey := 0; 
  S := Key; 
  Relative := IsRelative(S); 
  if not Relative then Delete(S, 1, 1); 
  Result := RegCreateKeyEx(GetBaseKey(Relative), PChar(S), 0, nil, 
    REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, nil, TempKey, @Disposition) = ERROR_SUCCESS; 
  if Result then RegCloseKey(TempKey) 
  else raise ERegistryException.CreateResFmt(@SRegCreateFailed, [Key]); 
end; 
 
function TRegistry.OpenKey(const Key: String; Cancreate: boolean): Boolean; 
var 
  TempKey: HKey; 
  S: string; 
  Disposition: Integer; 
  Relative: Boolean; 
begin 
  S := Key; 
  Relative := IsRelative(S); 
 
  if not Relative then Delete(S, 1, 1); 
  TempKey := 0; 
  if not CanCreate or (S = '') then 
  begin 
    Result := RegOpenKeyEx(GetBaseKey(Relative), PChar(S), 0, 
      FAccess, TempKey) = ERROR_SUCCESS; 
  end else 
    Result := RegCreateKeyEx(GetBaseKey(Relative), PChar(S), 0, nil, 
      REG_OPTION_NON_VOLATILE, FAccess, nil, TempKey, @Disposition) = ERROR_SUCCESS; 
  if Result then 
  begin 
    if (CurrentKey <> 0) and Relative then S := CurrentPath + '\' + S; 
    ChangeKey(TempKey, S); 
  end; 
end; 
 
function TRegistry.DeleteKey(const Key: string): Boolean; 
var 
  Len: DWORD; 
  I: Integer; 
  Relative: Boolean; 
  S, KeyName: string; 
  OldKey, DeleteKey: HKEY; 
  Info: TRegKeyInfo; 
begin 
  S := Key; 
  Relative := IsRelative(S); 
  if not Relative then Delete(S, 1, 1); 
  OldKey := CurrentKey; 
  DeleteKey := GetKey(Key); 
  if DeleteKey <> 0 then 
  try 
    SetCurrentKey(DeleteKey); 
    if GetKeyInfo(Info) then 
    begin 
      SetString(KeyName, nil, Info.MaxSubKeyLen + 1); 
      for I := Info.NumSubKeys - 1 downto 0 do 
      begin 
        Len := Info.MaxSubKeyLen + 1; 
        if RegEnumKeyEx(DeleteKey, DWORD(I), PChar(KeyName), Len, nil, nil, nil, 
          nil) = ERROR_SUCCESS then 
          Self.DeleteKey(PChar(KeyName)); 
      end; 
    end; 
  finally 
    SetCurrentKey(OldKey); 
    RegCloseKey(DeleteKey); 
  end; 
  Result := RegDeleteKey(GetBaseKey(Relative), PChar(S)) = ERROR_SUCCESS; 
end; 
 
function TRegistry.DeleteValue(const Name: string): Boolean; 
begin 
  Result := RegDeleteValue(CurrentKey, PChar(Name)) = ERROR_SUCCESS; 
end; 
 
function TRegistry.GetKeyInfo(var Value: TRegKeyInfo): Boolean; 
begin 
  FillChar(Value, SizeOf(TRegKeyInfo), 0); 
  Result := RegQueryInfoKey(CurrentKey, nil, nil, nil, @Value.NumSubKeys, 
    @Value.MaxSubKeyLen, nil, @Value.NumValues, @Value.MaxValueLen, 
    @Value.MaxDataLen, nil, @Value.FileTime) = ERROR_SUCCESS; 
  if SysLocale.FarEast and (Win32Platform = VER_PLATFORM_WIN32_NT) then 
    with Value do 
    begin 
      Inc(MaxSubKeyLen, MaxSubKeyLen); 
      Inc(MaxValueLen, MaxValueLen); 
    end; 
end; 
 
procedure TRegistry.GetKeyNames(Strings: TStrings); 
var 
  Len: DWORD; 
  I: Integer; 
  Info: TRegKeyInfo; 
  S: string; 
begin 
  Strings.Clear; 
  if GetKeyInfo(Info) then 
  begin 
    SetString(S, nil, Info.MaxSubKeyLen + 1); 
    for I := 0 to Info.NumSubKeys - 1 do 
    begin 
      Len := Info.MaxSubKeyLen + 1; 
      RegEnumKeyEx(CurrentKey, I, PChar(S), Len, nil, nil, nil, nil); 
      Strings.Add(PChar(S)); 
    end; 
  end; 
end; 
 
procedure TRegistry.GetValueNames(Strings: TStrings); 
var 
  Len: DWORD; 
  I: Integer; 
  Info: TRegKeyInfo; 
  S: string; 
begin 
  Strings.Clear; 
  if GetKeyInfo(Info) then 
  begin 
    SetString(S, nil, Info.MaxValueLen + 1); 
    for I := 0 to Info.NumValues - 1 do 
    begin 
      Len := Info.MaxValueLen + 1; 
      RegEnumValue(CurrentKey, I, PChar(S), Len, nil, nil, nil, nil); 
      Strings.Add(PChar(S)); 
    end; 
  end; 
end; 
 
function TRegistry.GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean; 
var 
  DataType: Integer; 
begin 
  FillChar(Value, SizeOf(TRegDataInfo), 0); 
  Result := RegQueryValueEx(CurrentKey, PChar(ValueName), nil, @DataType, nil, 
    @Value.DataSize) = ERROR_SUCCESS; 
  Value.RegData := DataTypeToRegData(DataType); 
end;    
 
function TRegistry.GetDataSize(const ValueName: string): Integer; 
var 
  Info: TRegDataInfo; 
begin 
  if GetDataInfo(ValueName, Info) then 
    Result := Info.DataSize else 
    Result := -1; 
end; 
 
 
procedure TRegistry.WriteString(const Name, Value: string); 
begin 
  PutData(Name, PChar(Value), Length(Value)+1, rdString); 
end; 
 
procedure TRegistry.WriteExpandString(const Name, Value: string); 
begin 
  PutData(Name, PChar(Value), Length(Value)+1, rdExpandString); 
end; 
 
function TRegistry.ReadString(const Name: string): string; 
var 
  Len: Integer; 
  RegData: TRegDataType; 
begin 
  Len := GetDataSize(Name); 
  if Len > 0 then 
  begin 
    SetString(Result, nil, Len); 
    GetData(Name, PChar(Result), Len, RegData); 
    if (RegData = rdString) or (RegData = rdExpandString) then 
      SetLength(Result, StrLen(PChar(Result))) 
    else ReadError(Name); 
  end 
  else Result := ''; 
end; 
 
procedure TRegistry.WriteInteger(const Name: string; Value: Integer); 
begin 
  PutData(Name, @Value, SizeOf(Integer), rdInteger); 
end; 
 
function TRegistry.ReadInteger(const Name: string): Integer; 
var 
  RegData: TRegDataType; 
begin 
  GetData(Name, @Result, SizeOf(Integer), RegData); 
  if RegData <> rdInteger then ReadError(Name); 
end; 
 
procedure TRegistry.WriteBinaryData(const Name: string; var Buffer; BufSize: Integer); 
begin 
  PutData(Name, @Buffer, BufSize, rdBinary); 
end; 
 
function TRegistry.ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer; 
var 
  RegData: TRegDataType; 
  Info: TRegDataInfo; 
begin 
  if GetDataInfo(Name, Info) then 
  begin 
    Result := Info.DataSize; 
    RegData := Info.RegData; 
    if ((RegData = rdBinary) or (RegData = rdUnknown)) and (Result <= BufSize) then 
      GetData(Name, @Buffer, Result, RegData) 
    else ReadError(Name); 
  end else 
    Result := 0; 
end; 
 
procedure TRegistry.PutData(const Name: string; Buffer: Pointer; 
  BufSize: Integer; RegData: TRegDataType); 
var 
  DataType: Integer; 
begin 
  DataType := RegDataToDataType(RegData); 
  if RegSetValueEx(CurrentKey, PChar(Name), 0, DataType, Buffer, 
    BufSize) <> ERROR_SUCCESS then 
    raise ERegistryException.CreateResFmt(@SRegSetDataFailed, [Name]); 
end; 
 
function TRegistry.GetData(const Name: string; Buffer: Pointer; 
  BufSize: Integer; var RegData: TRegDataType): Integer; 
var 
  DataType: Integer; 
begin 
  DataType := REG_NONE; 
  if RegQueryValueEx(CurrentKey, PChar(Name), nil, @DataType, PByte(Buffer), 
    @BufSize) <> ERROR_SUCCESS then 
    raise ERegistryException.CreateResFmt(@SRegGetDataFailed, [Name]); 
  Result := BufSize; 
  RegData := DataTypeToRegData(DataType); 
end; 
 
function TRegistry.ValueExists(const Name: string): Boolean; 
var 
  Info: TRegDataInfo; 
begin 
  Result := GetDataInfo(Name, Info); 
end; 
 
function TRegistry.GetKey(const Key: string): HKEY; 
var 
  S: string; 
  Relative: Boolean; 
begin 
  S := Key; 
  Relative := IsRelative(S); 
  if not Relative then Delete(S, 1, 1); 
  Result := 0; 
  RegOpenKeyEx(GetBaseKey(Relative), PChar(S), 0, FAccess, Result); 
end; 
 
function TRegistry.KeyExists(const Key: string): Boolean; 
var 
  TempKey: HKEY; 
  OldAccess: Longword; 
begin 
  OldAccess := FAccess; 
  try 
    FAccess := STANDARD_RIGHTS_READ or KEY_QUERY_VALUE or KEY_ENUMERATE_SUB_KEYS; 
    TempKey := GetKey(Key); 
    if TempKey <> 0 then RegCloseKey(TempKey); 
    Result := TempKey <> 0; 
  finally 
    FAccess := OldAccess; 
  end; 
end; 
 
procedure TRegistry.RenameValue(const OldName, NewName: string); 
var 
  Len: Integer; 
  RegData: TRegDataType; 
  Buffer: PChar; 
begin 
  if ValueExists(OldName) and not ValueExists(NewName) then 
  begin 
    Len := GetDataSize(OldName); 
    if Len > 0 then 
    begin 
      Buffer := AllocMem(Len); 
      try 
        Len := GetData(OldName, Buffer, Len, RegData); 
        DeleteValue(OldName); 
        PutData(NewName, Buffer, Len, RegData); 
      finally 
        FreeMem(Buffer); 
      end; 
    end; 
  end; 
end; 
 
 
              
 
end. 