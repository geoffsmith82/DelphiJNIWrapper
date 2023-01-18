{
Copyright (c) 1998-2001 Jonathan Revusky
All rights reserved.

This software was enhanced and ported to 32 bit and 64 bit by Amine Moulay Ramdane.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
3. All advertising materials mentioning features or use of this software
must display the following acknowledgement:
This product includes software developed by Jonathan Revusky
4. The name of the author may not be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit JavaRuntime;

// This unit is devoted to locating the JVM install directory
// and using the invocation API to create a JVM.
// All of the code here is Win32-specific heuristics.

interface
{$IFDEF FPC}
uses Classes, Windows, Registry, SysUtils, JNI, JUtils, JNIWrapper;
{$ELSE}
uses System.Classes, WinAPI.Windows, System.Win.Registry, System.SysUtils, JNI, JUtils, JNIWrapper,System.AnsiStrings;
{$endif}

type

  JvmType = (SunJava1, SunJava2);
  RuntimeOptions = set of JvmType;
  PPJavaVM = ^PJavaVM;

  TGetDefaultArgs = function (args : Pointer) : jint; stdcall;
  TCreateVM = function (vm : PPJavaVM ; penv : PPJNIEnv ; p : Pointer) : jint; stdcall;
  TGetCreatedVMs = function (vmBuf : PPJavaVM; buflen : Integer; nVMs : PJInt) : jint; stdcall;
  TExitProc = procedure (exitCode : jint) ; stdcall;
  TAbortProc = procedure ; stdcall;
  TPrintf = function (filepointer : Pointer ; const format : PAnsiChar ; args : va_list) : jint ; stdcall;

  EJvmException = class(Exception);
  EJavaRuntimeNotFound = class(Exception);
  EJavaRuntimeCreation = class(Exception);
  EClasspathException = class(Exception);

  TClassPath = class(TStringList)
  private
    // creates an instance based on a string.
    constructor Create;
  
    // Given a name of a class and its filename, perform a sanity check
    // to see if the fully qualified classname is consistent with this
    // filename classpath-wise.
    function SanityCheck(classname, filename : String) : String;
    // Performs similar sanity check on a .java file.
    function SanityCheckSource(filename : String) : String;
    procedure AddDir(dir : AnsiString);
    procedure AddPath(path : AnsiString);
  public
    function FullPath : AnsiString;
    class function GetDefault : TClasspath;
    class function GetBootPath : TClasspath;
  end;

    // class to encapsulate the location of the java runtime
    // and the use of the JNI invocation API.
  TJavaRuntime = class
  private
    FJava11 : Boolean;
    FHotspot : Boolean;
    FMS : Boolean;
    FJavaHome : AnsiString;
    FRuntimeLib : AnsiString;
    FJavaVM :  TJavaVM;
    DLLHandle : THandle;
    vmargs : JDK1_1InitArgs;
    vmargs2 : JavaVMInitArgs;
    FClasspath : TClasspath;
    FProperties : TStrings;
    FExitProc : TExitProc;
    FAbortProc : TAbortProc;
    FPrintf : TPrintf;
    FDebugPort, FVerbose, FDisableAsyncGC, FVerboseGC, FEnableClassGC,
    FVerifyMode, FCheckSource, FMinHeapSize, FMaxHeapSize, FJavaStackSize,
    FNativeStackSize : Integer;
    function FindJava11 : Boolean;
    function FindJava12 : Boolean;
    function CheckJavaRegistryKey(key : AnsiString) : boolean;
    function GetClasspath : AnsiString;
    procedure SetClasspath(S: AnsiString);
    procedure SetNativeStackSize(Size : Integer);
    procedure SetJavaStackSize(Size : Integer);
    procedure SetMinHeapSize(Size : Integer);
    procedure SetMaxHeapSize(Size : Integer);
    procedure SetVerifyMode(Arg : Integer);
    procedure SetCheckSource(arg : Integer);
    procedure SetEnableClassGC(B : Boolean);
    procedure SetVerboseGC(B: Boolean);
    procedure SetDisableAsyncGC(B: Boolean);
    procedure SetVerbose(B : Boolean);
    procedure SetDebugPort(Port : Integer);
    procedure SetDebugging(Arg : Integer);
    procedure SetAbortProc(proc : TAbortProc);
    procedure SetExitProc(proc : TExitProc);
    procedure SetPrintf(printproc : TPrintf);
    procedure Initialize; // Loads the DLL.
    procedure InitJava11;
    procedure InitJava2;
  public
    // processes a command-line option
    procedure ProcessCommandLineOption(S : AnsiString);
    // processes a bunch of command line options passed in a container.
    procedure ProcessCommandLine(Options : TStrings);
    procedure AddProperty(S: AnsiString);
    function SanityCheck(classname, filename : AnsiString) : AnsiString;
    function SanityCheckSource(filename : AnsiString) : AnsiString;
    procedure AddToClasspath(filename : AnsiString);
    function GetVM : TJavaVM; //Instantiates the JVM
    procedure CallMain(const ClassName : AnsiString ; args : TStrings);
    procedure CallExit(val : Integer);
    procedure Wait;
    property RuntimeLib : AnsiString read FRuntimeLib;
    property JavaHome : AnsiString read FJavaHome;
    property Classpath : AnsiString read getClasspath write SetClasspath;
    property IsJava11 : Boolean read FJava11;
    property IsMS : Boolean read FMS;
    property Hotspot : Boolean read FHotspot write FHotspot;
    
    // write-only properties that only work before instantiating VM.
    property NativeStackSize : Integer write SetNativeStackSize;
    property JavaStackSize : Integer write SetJavaStackSize;
    property CheckSource : Integer write setCheckSource;
    property MinHeapSize : Integer write SetMinHeapSize;
    property MaxHeapSize : Integer write SetMaxHeapSize;
    property VerifyMode : Integer write SetVerifyMode;
    property EnableClassGC : Boolean write setEnableClassGC;
    property VerboseGC : Boolean write SetVerboseGC;
    property DisableAsyncGC : Boolean write setDisableAsyncGC;
    property Verbose : Boolean write SetVerbose;
    property DebugPort : Integer write SetDebugPort;
    property Debugging : Integer write SetDebugging;
    property AbortProc : TAbortProc write SetAbortProc;
    property ExitProc : TexitProc write SetExitProc;
    property Printf : TPrintf write SetPrintf;
    
    constructor Create(option : JvmType);
    destructor Destroy; override;
    class function GetDefault : TJavaRuntime;
    class procedure SetJava11(Java11 : Boolean);
    class procedure SetAppClassPath(path : AnsiString);
    class procedure SetBasePath(path : AnsiString);
    class procedure SetNeedTools(B : Boolean); // a bit of a hack for use by SmartJC.
    class procedure SetClassicVM(B : Boolean);
  end;
  
  function GetPackageName(filename : AnsiString) : AnsiString;

implementation

var
  {$IFDEF FPC}
  SystemDirBuf : Array[0..MAX_PATH] of AnsiChar;
  {$ELSE}
  SystemDirBuf : Array[0..MAX_PATH] of AnsiChar;
  {$ENDIF}
  NeedsJDK : Boolean; // // True, if we need the sun.* classes for compilation, etc.
  Prefers11 : Boolean; // Do we look for java 1.1 first?
  UseClassicVM : Boolean; // Do we use the classic VM?
  GetDefaultArgs : TGetDefaultArgs;
  CreateVM : TCreateVM;
  GetCreatedVMs : TGetCreatedVMs;
  instanceCount : Integer;
  searchrec : TSearchRec;
  AppClassPath : AnsiString; // classpath specified
  BasePath : AnsiString; // The class considered to be the base path, found from snooping in classfile.
  cpath : TClasspath; // the singleton TClasspath instance.
  bootpath : TClasspath; // the TClasspath that represents the boot path
  DefaultRuntime : TJavaRuntime; // singleton JavaRuntime instance.

const
  PLUGIN_11_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\1.1';//Home
  IBM_JRE_11_KEY = '\SOFTWARE\IBM\IBM WIN32 Runtime Environment, Java(TM) Edition';
  IBM_JDK_117_KEY = '\SOFTWARE\IBM\IBM WIN32 Developer Kit, Java(TM) Edition\1.1.7';
  IBM_JDK_118_KEY = '\SOFTWARE\IBM\IBM WIN32 Developer Kit, Java(TM) Tech. Edition\1.1.8';
  JRE_11_KEY = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.1'; //JavaHome, Microversion
  JB_KEY = '\SOFTWARE\JavaSoft\Java Runtime\1.1.6'; //JavaHome
  // JDK_11_KEY = '\SOFTWARE\JavaSoft\Java Development Kit\1.1';
  JDK_11_KEY = '\SOFTWARE\JavaSoft\Java Development Kit\1.8.0_45'; //JavaHome, Microversion
  //JavaHome, Microversion
  JRE_12_KEY = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.2'; // JavaHome, RuntimeLib
  JRE_13_KEY='\SOFTWARE\JavaSoft\Java Runtime Environment\1.3';
  //JRE_14_KEY='\SOFTWARE\JavaSoft\Java Runtime Environment\1.4';
  //JRE_14_KEY='\SOFTWARE\JavaSoft\Java Runtime Environment\1.8.0_45';
  //JRE_15_KEY='\SOFTWARE\JavaSoft\Java Runtime Environment\1.7.0_25';
  JRE_14_KEY='\SOFTWARE\JavaSoft\Java Runtime Environment\1.8';
  JRE_15_KEY='\SOFTWARE\JavaSoft\Java Runtime Environment\1.7';
  JRE_16_KEY='\SOFTWARE\JavaSoft\Java Runtime Environment\1.6';
  JRE_17_KEY='\SOFTWARE\JavaSoft\Java Runtime Environment\1.5';
  JRE_18_KEY='\SOFTWARE\JavaSoft\JRE\10';

  PLUGIN_12_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\1.2'; // JavaHome, RuntimeLib
  PLUGIN_13_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\1.3'; // JavaHome, RuntimeLib
  //PLUGIN_14_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\1.4';
  PLUGIN_14_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\11.45.2';
  PLUGIN_15_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\10.25.2';
 
  JDK_12_KEY = '\SOFTWARE\JavaSoft\Java Development Kit\1.2'; //JavaHome, Microversion
  JDK_13_KEY = '\SOFTWARE\JavaSoft\Java Development Kit\1.3';
  JDK_14_KEY = '\SOFTWARE\JavaSoft\Java Development Kit\1.4';
  JDK_15_KEY = '\SOFTWARE\JavaSoft\JDK\10';


  JRE11Keys : array[1..3] of AnsiString = (PLUGIN_11_KEY, IBM_JRE_11_KEY, JRE_11_KEY);
  JDK11Keys : array[1..5] of AnsiString = (JDK_15_KEY,IBM_JDK_118_KEY, IBM_JDK_117_KEY,JDK_11_KEY,JB_KEY);
  JRE12Keys : array[1..11] of AnsiString = (JRE_18_KEY,JRE_14_KEY, PLUGIN_14_KEY,JRE_15_KEY, PLUGIN_15_KEY,JRE_16_KEY,JRE_17_KEY,JRE_13_KEY, PLUGIN_13_KEY, JRE_12_KEY, PLUGIN_12_KEY);

  BootClasspath : AnsiString = '';


function ReadRegKey(SubKey,Key:string):ansistring;
var
  Reg: TRegistry;
  list1: TStringList;
begin
  list1 := TStringList.create;
  Reg := TRegistry.Create(KEY_READ or $0100); //open Registry
  try
    {try if Root key exists}
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    {open the Registry key}
    if Reg.OpenKey(SubKey,false) then
    begin
      {read the value of the key HKEY_LOCAL_MACHINE\\Software\YourProgName\AnyParam }
      Result := Reg.ReadString(Key);
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
    list1.free;
  end;
end;

procedure StripComments(var Line : AnsiString; var InComment : Boolean);
var
  S : AnsiString;
  I : Integer;
begin
  S := '';
  if InComment then
  begin
    I := AnsiPos('*/', Line);
    if I>0 then
      begin
        Line := Copy(Line, 2 + I, length(Line));
        InComment := False;
        StripComments(Line, InComment);
      end
    else
      Line := '';
  end
  else begin
    I := AnsiPos('/*', Line);
    if I > 0 then
      begin
        InComment := True;
        S := Copy(Line, 1, I - 1);
        Line := Copy(Line, I + 2, Length(Line));
        StripComments(Line, InComment);
      end;
    Line := S + Line;
  end;
  I := AnsiPos('//', Line);
  if I > 0 then
    Line := Copy(Line, 1, I - 1);
end;

procedure TJavaRuntime.Initialize;
begin
  if DLLHandle <> 0 then
    exit; // already initialized.

  {$IFDEF FPC}
  DLLHandle := LoadLibrary(PChar(FRuntimeLib));
  {$ELSE}
  DLLHandle := SafeLoadLibrary(FRuntimeLib);
  {$endif}

  if DLLHandle = 0 then
    raise EJavaRuntimeCreation.Create('Could not load DLL ' + FRuntimeLib);
  @CreateVM := getProcAddress(DLLHandle, 'JNI_CreateJavaVM');
  @GetDefaultArgs := getProcAddress(DLLHandle, 'JNI_GetDefaultJavaVMInitArgs');
  @GetCreatedVMs := getProcAddress(DLLHandle, 'JNI_GetCreatedJavaVMs');
  if (@CreateVM = Nil) or (@GetDefaultArgs = Nil) or (@GetCreatedVMs = Nil) then
    raise EJavaRuntimeCreation.Create('Dynamic Link Library ' + FRuntimeLib + ' is not valid.');
  vmargs.version := $00010008;
  vmargs2.version := $00010008;
  GetDefaultArgs(@vmargs);
  GetDefaultArgs(@vmargs2);
end;

function TJavaRuntime.GetVM : TJavaVM;
var
  PVM : PJavaVM;
  penv : PJNIEnv;
  args : Pointer;
begin
  if FJavaVM <> nil then
  begin
    Result := FJavaVM;
    Exit;
  end;
  if @CreateVM = nil then
    Initialize;
  if IsJava11 then
  begin
    InitJava11;
    args := @vmargs;
  end
  else
  begin
    InitJava2;
    args := @vmargs2;
  end;
  if CreateVM(@pvm, @penv, args) <>0 then
    raise EJavaRuntimeCreation.Create('Could not create JVM');
  TJavaVM.SetThreadPenv(penv);
  FJavaVM := TJavaVM.Create(PVM);
  Result := FJavaVM;
end;

procedure TJavaRuntime.InitJava11;
begin
  vmargs.properties := convertStrings(FProperties);
  vmargs.classpath := PAnsiChar(Classpath);
  vmargs.Verbose := FVerbose;
  vmargs.DisableAsyncGC := FDisableAsyncGC;
  vmargs.EnableVerboseGC := FVerboseGC;
  vmargs.EnableClassGC := FEnableClassGC;
  vmargs.CheckSource := FCheckSource;
  vmargs.VerifyMode := FVerifyMode;
  if Assigned(FExitProc) then
    vmargs.Exit := FExitProc;
  if Assigned(FAbortProc)  then
    vmargs.abort := FAbortProc;
  if Assigned(FPrintf) then
    vmargs.vfprintf := FPrintf;
  if FDebugPort <> 0 then
    vmargs.DebugPort := FDebugPort;
  if FMinHeapSize >0 then
    vmargs.MinHeapSize := FMinHeapSize;
  if FMaxHeapSize >0 then
    vmargs.MaxHeapSize := FMaxHeapSize;
  if FJavaStackSize >0 then
    vmargs.JavaStackSize := FJavaStackSize;
  if FNativeStackSize >0 then
    vmargs.NativeStackSize := FNativeStackSize;
end;

procedure TJavaRuntime.InitJava2;
var
  I : Integer;
  S : AnsiString;
  PVMOption, PVO : PJavaVMOption;
begin
    // Just handle classpath and properties for now.
      
  VMArgs2.Noptions := 1+ FProperties.Count;
  if (FVerbose <> 0) or (FVerboseGC <>0) then Inc(VMArgs2.Noptions);
  if FVerboseGC <> 0 then Inc(VMArgs2.Noptions);
  if FMinHeapSize > 0 then Inc(VMArgs2.Noptions);
  if FMaxHeapSize > 0 then Inc(VMArgs2.Noptions);
  if BootClasspath <> '' then Inc(VMArgs2.Noptions);
  if FEnableClassGC <> 0 then Inc(VMArgs2.NOptions);
  if Assigned(FExitProc) then Inc(VMargs2.NOptions);
  if Assigned(FAbortProc)  then Inc(VMArgs2.NOptions);
  if Assigned(FPrintf) then Inc(VMArgs2.NOptions);
      
  vmargs2.ignoreUnrecognized := True;
  PVMOption := AllocMem(sizeof(JavaVMOPtion) * VMargs2.NOptions);
  PVO := PVMOption;
  S := '-Djava.class.path=' + Classpath;
  {$IFDEF FPC}
  PVMOption^.optionString := StrNew(PAnsiChar(S));
  {$ELSE}
  PVMOption^.optionString := System.AnsiStrings.StrNew(PAnsiChar(S));
  {$endif}
  
  PVMOption^.extraInfo := Nil;
  Inc(PVO);
      
  for I := 0 to  FProperties.Count - 1 do
  begin
    S := '-D' + FProperties[I];

    {$IFDEF FPC}
    PVO^.optionString := StrNew(PAnsiChar(S));
    {$ELSE}
    PVO^.optionString := System.AnsiStrings.StrNew(PAnsiChar(S));
    {$endif}
     
    Inc(PVO);
  end;
      
  if (FVerbose <> 0) or (FVerboseGC <> 0) then
  begin
    S := '-verbose:';
    if FVerbose <> 0 then
      S := S + 'class';
    if FVerboseGC <> 0 then
      S := S + ',';
    if FVerboseGC <> 0 then
      S := S + 'gc';
    {$IFDEF FPC}
    PVO^.optionString := StrNew(PAnsiChar(S));
    {$ELSE}
    PVO^.optionString := System.AnsiStrings.StrNew(PAnsiChar(S));
    {$endif}
     
    Inc(PVO);
  end;

  if FMinHeapSize > 0 then
  begin
    
    {$IFDEF FPC}
    PVO^.optionString := StrNew(PAnsiChar('-Xms' + IntToStr(FMinHeapSize)));
    {$ELSE}
    PVO^.optionString := System.AnsiStrings.StrNew(PAnsiChar('-Xms' + IntToStr(FMinHeapSize)));
    {$endif}
      
    Inc(PVO);
  end;
      
  if FMaxHeapSize > 0 then
  begin
    {$IFDEF FPC}
    PVO^.optionString := StrNew(PAnsiChar('-Xmx' + IntToStr(FMaxHeapSize)));
    {$ELSE}
    PVO^.optionString := System.AnsiStrings.StrNew(PAnsiChar('-Xmx' + IntToStr(FMaxHeapSize)));
    {$endif}

      
    Inc(PVO);
  end;
      
  if FEnableClassGC <> 0 then
  begin
    {$IFDEF FPC}
    PVO^.optionString := StrNew(PAnsiChar('-Xnoclassgc'));
    {$ELSE}
    PVO^.optionString := System.AnsiStrings.StrNew(PAnsiChar('-Xnoclassgc'));
    {$endif}

    
    Inc(PVO);
  end;
      
  if BootClasspath <> '' then
  begin
    {$IFDEF FPC}
    PVO^.optionString := StrNew(PAnsiChar('-Xbootclasspath/p:' + BootClasspath));
    {$ELSE}
    PVO^.optionString := System.AnsiStrings.StrNew(PAnsiChar('-Xbootclasspath/p:' + BootClasspath));
    {$endif}

    Inc(PVO);
  end;

  if Assigned(FPrintf) then
  begin
    {$IFDEF FPC}
    PVO^.optionString := StrNew(PAnsiChar('exit'));
    {$ELSE}
    PVO^.optionString := System.AnsiStrings.StrNew(PAnsiChar('exit'));
    {$endif}

     
    PVO^.ExtraInfo := @FPrintf;
    Inc(PVO);
  end;

  if Assigned(FExitProc) then
  begin
    {$IFDEF FPC}
    PVO^.optionString := StrNew(PAnsiChar('exit'));
    {$ELSE}
    PVO^.optionString := System.AnsiStrings.StrNew(PAnsiChar('exit'));
    {$endif}

     
    PVO^.ExtraInfo := @FExitProc;
    Inc(PVO);
  end;

  if Assigned(FAbortProc) then
  begin
    {$IFDEF FPC}
    PVO^.optionString := StrNew(PAnsiChar('abort'));
    {$ELSE}
    PVO^.optionString := System.AnsiStrings.StrNew(PAnsiChar('abort'));
    {$endif}

      
    PVO^.ExtraInfo := @FAbortProc;
  end;
      
  vmargs2.options := PVMOption;
  vmargs2.version := $00010008;
end;

  
//convenience wrappers.
  
procedure TJavaRuntime.CallMain(const ClassName : AnsiString ; args : TStrings);
begin
  TJavaVM.CallMain(className, args);
end;
  
procedure TJavaRuntime.Wait;
begin
  if FJavaVM <> Nil then
  begin
    if isMS then
      Sleep(INFINITE)
    else
      FJavaVM.Wait;
  end;
end;
  
procedure TJavaRuntime.CallExit(val : Integer);
begin
  TJavaVm.CallExit(val);
end;

procedure TJavaRuntime.ProcessCommandLineOption(S : AnsiString);
var
  L  : String;
  function extractSize(S : AnsiString) : Integer;
  begin
    if S[length(S)] = 'k' then
      Result := $400
    else if S[length(S)] = 'm' then
      Result := $100000
    else
      Result  := 1;
    if Result <> 1 then
      S := Copy(S, 1, length(S) - 1);
    Result := Result * StrToIntDef(S, 0);
  end;
begin
 //S:=S1;
  L  := LowerCase(S);
  if (L = '-v') or (L = 'verbose') then
    Verbose := true
  else if (L = '-verbosegc') then
    VerboseGC := true
  else if (L = '-noasync') then
    DisableAsyncGC := true
  else if (L = '-noclassgc') then
    EnableClassGC := false
  else if (L = '-verify') then
    VerifyMode := 2
  else if (L = '-noverify') then
    VerifyMode := 0
  else if (L = '-verifyremote') then
    VerifyMode :=1
  else if (L = '-nojit') then
    AddProperty('java.compiler=')
  else if Copy(L, 1, 3) = '-cp' then
    FClasspath.AddPath(Copy(S, 5, length(S)))
  else if Copy(L, 1, 10) = '-classpath' then
    FClasspath.AddPath(Copy(S, 12, length(S)))
  else if Copy(L, 1, 2) = '-d' then
    AddProperty(Copy(S, 3, length(S)))
  else if Copy(L, 1, 3) = '-ms' then
    MinHeapSize := ExtractSize(Copy(L, 4, length(L)))
  else if Copy(L, 1, 3) = '-mx' then
    MaxHeapSize := ExtractSize(Copy(L, 4, length(L)))
  else if Copy(L, 1, 3) = '-ss' then
    NativeStackSize := ExtractSize(Copy(L, 4, length(L)))
  else if Copy(L, 1, 3) = '-oss' then
    NativeStackSize := ExtractSize(Copy(L, 5, length(L)));
end;

procedure TJavaRuntime.ProcessCommandLine(Options : TStrings);
var
  I: Integer;
begin
  for I := 0 to Options.Count - 1 do
    ProcessCommandLineOption(Options[I]);
end;
  
class function TJavaRuntime.GetDefault : TJavaRuntime;
var
  FirstChoice, SecondChoice, ThirdChoice, temp : JvmType;
begin
 
  if DefaultRuntime = Nil then
  begin
    FirstChoice := SunJava2;
    SecondChoice := SunJava1;
    if Prefers11 then
    begin
      temp := FirstChoice;
      FirstChoice := SunJava1;
      SecondChoice := Temp;
    end;
    try
      DefaultRuntime := TJavaRuntime.Create(FirstChoice);
    except on EJavaRuntimeNotFound do
      try
        DefaultRuntime := TJavaRuntime.Create(SecondChoice);
      except on EJavaRuntimeNotFound do
        DefaultRuntime :=TJavaRuntime.Create(ThirdChoice);
      end;
    end;
  end;
  Result := DefaultRuntime;
end;

class procedure TJavaRuntime.SetJava11(Java11 : Boolean);
begin
  Prefers11 := Java11;
end;

class procedure TJavaRuntime.SetClassicVM(B : Boolean);
begin
  UseClassicVM := B;
end;
  
class procedure TJavaRuntime.SetNeedTools(B : Boolean);
begin
  NeedsJDK := True;
end;

procedure TJavaRuntime.AddToClasspath(filename : AnsiString);
begin
  FClasspath.AddDir(filename);
end;
  
function TJavaRuntime.getClasspath : AnsiString;
var
  CPath : TClasspath;
  Reg : TRegistry;
  GotKey : Boolean;
begin
  CPath := TClasspath.GetDefault;
  if ((not FJava11) and NeedsJDK) then
  begin
    reg := TRegistry.Create;
    reg.RootKey := HKEY_LOCAL_MACHINE;
    GotKey := Reg.OpenKey(JDK_13_KEY, false);
  
    if not GotKey then
      GotKey := Reg.OpenKey(JDK_12_KEY, false);
    if GotKey then
    begin
      if reg.ValueExists('JavaHome') then
        CPath.AddDir(reg.ReadString('JavaHome') + '\lib\tools.jar');
    end;
    reg.Free;
  end;
  result := CPath.Fullpath;
end;
  
procedure TJavaRuntime.SetClasspath(S : AnsiString);
begin
  FClasspath := TClasspath.GetDefault;
  FClasspath.AddPath(S);
end;
  
constructor TJavaRuntime.Create(option : JvmType);
begin
  if DefaultRuntime <> Nil then
    raise EJavaRuntimeCreation.Create('Can only instantiate one Java runtime per process');
  case option of
    SunJava1 :
      if not FindJava11 then
        raise EJavaRuntimeNotFound.Create('Java 1.1 runtime not found');
     SunJava2:
      if not FindJava12 then
        raise EJavaRuntimeNotFOund.Create('Java 2 runtime not found');
  end;
  DefaultRuntime := Self; // set the singleton
  FClasspath := TClasspath.GetDefault;
  FProperties := TStringList.Create;
  FVerifyMode := 1;
end;

destructor TJavaRuntime.Destroy;
begin
  DefaultRuntime := Nil;
    if (dllHandle <> 0) and (instanceCount = 0) then
      if FreeLibrary(dllHandle) then
        dllHandle := 0;
  inherited Destroy;
end;
  
function TJavaRuntime.FindJava12 : Boolean;
var
  I : Integer;
begin
  Result := False; //false;

{FRuntimeLib := FindOnSystemPath('javai.dll');
  if FRuntimeLib <> '' then
  begin
    FJavaHome := ExtractFileDir(ExtractFileDir(FRuntimeLib));
    FJava11 := false; //This is a 1.2 VM.
    result := true; // success!
    exit; // success!
  end;}


//WRITELN('\Software\JavaSoft\JDK\'+ReadRegKey('\SOFTWARE\JavaSoft\JDK','CurrentVersion'));
  if CheckJavaRegistryKey('\Software\JavaSoft\Java Development Kit\' + ReadRegKey('\SOFTWARE\JavaSoft\Java Development Kit','CurrentVersion')) then
  begin
    FJava11 := False; //This is a 1.2 VM.
    Result := True; // success!
    Exit;
  end;

  if CheckJavaRegistryKey('\Software\JavaSoft\Java Runtime Environment\'+ReadRegKey('\SOFTWARE\JavaSoft\Java Runtime Environment','CurrentVersion')) then
  begin
    FJava11 := False; //This is a 1.2 VM.
    Result := True; // success!
    Exit;
  end;

 { for I:=Low(JRE12Keys) to High(JRE12Keys) do
    if (CheckJavaRegistryKey(JRE12Keys[I])) then
    begin
      FJava11 := false; //This is a 1.2 VM.
      result := true; // success!
      Exit;
    end;}
end;


{heuristics to find a java 1.1 runtime.}

function TJavaRuntime.FindJava11 : Boolean;
var
  I: Integer;
begin
  // First look on the system path.
  FRuntimeLib := FindOnSystemPath('javai.dll');
  if FRuntimeLib <> '' then
  begin
    FJavaHome := ExtractFileDir(ExtractFileDir(FRuntimeLib));
    Result := True;
    FJava11 := True;
    Exit; // success!
  end;
  
// Failing that, search the Windows registry for location.
  
  if not needsJDK then
  begin
    for I := Low(JRE11Keys) to High(JRE11Keys) do
    begin
      if (CheckJavaRegistryKey(JRE11Keys[I])) then
      begin
        Result := True; // success!
        FJava11 := True;
        Exit;
      end;
    end;
  end;
  for I := Low(JDK11Keys) to High(JDK11Keys) do
  begin
    if (CheckJavaRegistryKey(JDK11Keys[I])) then
    begin
      Result := True; // success!
      FJava11 := True;
      Exit;
    end;
  end;
  Result := False; // failure.
end;

{Checks the Java registry key given as an argument.
Returns true on success and sets the FJavaLib and FJavaHome
fields}

function TJavaRuntime.CheckJavaRegistryKey(key : AnsiString) : boolean;
var
  reg : TRegistry;
  S, HotspotLib : AnsiString;
begin
  Result := False;
  reg := TRegistry.Create(KEY_READ or $0100);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey(key, False) then
    begin
      if True {reg.ValueExists('RuntimeLib')} then
      begin
        S := reg.ReadString('RuntimeLib');
       // if S = '' then S := reg.ReadString('JavaHome') + '\bin\classic\jvm.dll';

        if S = '' then
          S := reg.ReadString('JavaHome') + '\bin\server\jvm.dll';

        if FileExists(S) then
        begin
          Result := True;
          if not UseClassicVM then
          begin
            HotspotLib := ExtractFileDir(ExtractFileDir(S)) + '\server\jvm.dll';
            if FileExists(HotspotLib) then
            begin
              S := HotspotLib;
              FHotspot := True;
            end;
          end;
          FRuntimeLib := S;
          if reg.ValueExists('JavaHome') then
            FJavaHome := reg.ReadString('JavaHome')
          else
            FJavaHome := ExtractFileDir(ExtractFileDir(ExtractFileDir(FRuntimeLib)));
        end;
        Exit;
      end
      else
      begin
        if reg.ValueExists('JavaHome') then
          S := reg.ReadString('JavaHome')
        else if reg.valueExists('Home') then
          S := reg.ReadString('Home')
        else if reg.valueExists('java_home') then
          S := reg.ReadString('java_home')
        else
          Exit; // failure!
      end;
    end
    else
      Exit;

    S := ExcludeTrailingPathDelimiter(s);
    
    if FileExists(S + '\bin\server\jvm.dll') then
    begin
      FRuntimeLib := S + '\bin\server\jvm.dll'; // Success!
        //writeln(fruntimelib);
      FJavaHome := S;
      Result := True;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TJavaRuntime.SetNativeStackSize(Size : Integer);
begin
  if Size > 0 then
    FNativeStackSize := Size;
end;

procedure TJavaRuntime.SetJavaStackSize(Size : Integer);
begin
  if Size > 0 then
    FJavaStackSize := Size;
end;
  
procedure TJavaRuntime.SetMinHeapSize(Size : Integer);
begin
  if Size  > 0 then
    FMinHeapSize := Size;
end;
  
procedure TJavaRuntime.SetMaxHeapSize(Size : Integer);
begin
  if Size  > 0 then
    FMaxHeapSize := Size;
end;
  
procedure TJavaRuntime.SetVerifyMode(Arg : Integer);
begin
  FVerifyMode := Arg;
end;
  
procedure TJavaRuntime.SetCheckSource(arg : Integer);
begin
  FCheckSource := arg;
end;
  
procedure TJavaRuntime.SetEnableClassGC(B : Boolean);
begin
  FEnableClassGC := Integer(B);
end;
  
procedure TJavaRuntime.SetVerboseGC(B:Boolean);
begin
  FVerboseGC := Integer(B);
end;
  
procedure TJavaRuntime.SetDisableAsyncGC(B: Boolean);
begin
  FDisableAsyncGC := Integer(B);
end;
  
procedure TJavaRuntime.SetVerbose(B : Boolean);
begin
  FVerbose := Integer(B);
end;
  
procedure TJavaRuntime.SetDebugPort(Port : Integer);
begin
  FDebugPort := Port;
end;
  
procedure TJavaRuntime.SetDebugging(Arg : Integer);
begin
end;
  
procedure TJavaRuntime.SetAbortProc(proc : TAbortProc);
begin
  FAbortproc := proc;
end;
  
procedure TJavaRuntime.SetExitProc(proc : TExitProc);
begin
  FExitProc := Proc;
end;
  
procedure TJavaRuntime.SetPrintf(printproc : TPrintf);
begin
  fprintf := printproc;
end;
  
function TJavaRuntime.SanityCheck(classname, filename : AnsiString) : AnsiString;
begin
  Result := FClasspath.SanityCheck(classname, filename);
end;

function TJavaRuntime.SanityCheckSource(filename : AnsiString) : AnsiString;
begin
  Result := FClasspath.sanityCheckSource(filename);
end;
  
procedure TJavaRuntime.AddProperty(S: AnsiString);
begin
  FProperties.Add(S);
end;

procedure AddAllArchives(C : TClasspath; directory, pattern : AnsiString);
begin
  if FindFirst(Directory + pattern, faAnyFile, searchrec) = 0 then
  begin
    cpath.AddDir(Directory + searchrec.Name);
    while FindNext(searchrec) = 0 do
      cpath.AddDir(Directory + searchrec.Name);
  end;
  FindClose(searchrec);
end;

  
class function TClasspath.GetDefault : TClassPath;
var
  Home, libjars, ThirdPartyDir : AnsiString;
  Runtime : TJavaRuntime;
  SearchRec : TSearchRec;
begin
  if cpath = Nil then
  begin
    cpath := TClasspath.Create;
    Runtime := TJavaRuntime.GetDefault;
    Home := Runtime.JavaHome;


    if FileExists(Home + '\classes') then cpath.AddDir(Home + '\classes');

    // Now see if there are any other jars or zips in there and add them.

    Libjars := Home + '\lib\*.jar';
    AddAllArchives(cpath, Home + '\lib\ext\', '*.jar');
    AddAllArchives(cpath, Home + '\lib\ext\', '*.zip');
    AddAllArchives(cpath, Home + '\lib\', '*.jar');
    AddAllArchives(cpath, Home + '\lib\', '*.zip');
//          if BaseClassPath = '' then
//             BaseClassPath := GetEnvironmentString('CLASSPATH');

    ThirdPartyDir := GetEnvironmentString('JARS_DIR');
    if ThirdPartyDir = '' then ThirdPartyDir := '.';
    if ThirdPartyDir[Length(ThirdPartyDir)] <> '\' then
      ThirdPartyDir := ThirdPartyDir + '\';

    if FindFirst(ThirdPartyDir + '*.jar', 0, searchRec) = 0 then
    repeat
      cpath.AddDir(ThirdPartyDir + searchRec.name);
    until FindNext(SearchRec) <> 0;

    cpath.AddPath(GetEnvironmentString('CLASSPATH'));
    cpath.AddPath(AppClassPath);
    cpath.AddPath(BasePath);
    cpath.AddDir(getCurrentDir); // Maybe better off without this.
  end;
  Result := cpath;
end;


class function TClasspath.GetBootPath : TClassPath;
var
  Home, ThirdPartyDir : AnsiString;
  SearchRec : TSearchRec;
begin
  if bootpath = Nil then
  begin
    bootpath := TClasspath.Create;
    Home := TJavaRuntime.GetDefault.JavaHome;
    if FileExists(Home + '\classes') then
      cpath.AddDir(Home + '\classes');

    // Now see if there are any other jars or zips in there and add them.

    AddAllArchives(cpath, Home + '\lib\ext\', '*.jar');
    AddAllArchives(cpath, Home + '\lib\ext\', '*.zip');
    AddAllArchives(cpath, Home + '\lib\', '*.jar');
    AddAllArchives(cpath, Home + '\lib\', '*.zip');
//          if BaseClassPath = '' then
//             BaseClassPath := GetEnvironmentString('CLASSPATH');

    ThirdPartyDir := GetEnvironmentString('JARS_DIR');
    if ThirdPartyDir = '' then ThirdPartyDir := '.';
    if ThirdPartyDir[Length(ThirdPartyDir)] <> '\' then
      ThirdPartyDir := ThirdPartyDir + '\';

    if FindFirst(ThirdPartyDir + '*.jar', 0, searchRec) = 0 then
    repeat
      cpath.AddDir(ThirdPartyDir + searchRec.name);
    until FindNext(SearchRec) <> 0;

    cpath.AddPath(GetEnvironmentString('CLASSPATH'));
    cpath.AddPath(AppClassPath);
    cpath.AddPath(BasePath);
    cpath.AddDir(getCurrentDir); // Maybe better off without this.
  end;
  Result := cpath;
end;

constructor TClasspath.Create;
begin
end;


procedure TClasspath.AddPath(Path : AnsiString);
var
  Len: Integer;
  Dirs  : TStringList;
  I : Integer;
begin
  Dirs  := TStringList.Create;
  repeat
    Len := AnsiPos(';', Path);
    if Len > 1 then
      Dirs.add(Copy(Path, 1, Len - 1));
    Path := Copy(Path, Len + 1, Length(Path));
  until Len=0;
  if length(Path)>0 then
    Dirs.add(Path);
  for I := Dirs.Count downto 1 do
    AddDir(Dirs[I - 1]);
  Dirs.Free;
end;
  
procedure TClasspath.AddDir(dir : AnsiString);
var
  S: AnsiString;
  I : Integer;
begin
  S := ExpandFileName(dir);
  if (S[length(S)] = '\') and (S[length(S) - 1] <> ':') then
    S := Copy(S, 1, length(S) - 1);
  I := IndexOf(S);
  if I >= 0 then
    Delete(I);
  add(S);
end;
  

function TClasspath.FullPath : AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := Count downto 1 do
  begin
    if I < Count then
      Result := Result + ';';
    Result := Result + Strings[I - 1];
  end;
end;


// Sets the part of the classpath that is specific to the app.
  
class procedure TJavaRuntime.SetAppClassPath(path : AnsiString);
begin
  AppClassPath := path;
end;

procedure addAllFilesToPath(Directory, Pattern : AnsiString; var Path : AnsiString);
begin
  if FindFirst(Directory + pattern, faAnyFile, searchrec) = 0 then
  begin
    Path := Path + ';' + Directory + searchrec.Name;
    while FindNext(searchrec) = 0 do
      Path := Path + ';' + Directory + searchrec.Name;
  end;
    FindClose(searchrec);
end;

class procedure TJavaRuntime.SetBasePath(Path : AnsiString);
var
  Dir : AnsiString;
begin
  BasePath := ExpandFileName(Path);
  Dir := ExtractFilePath(ExpandFileName(BasePath));
  addAllFilesToPath(Dir, '*.zip', BasePath);
  AddAllFilesToPath(Dir, '*.jar', BasePath);
  addAllFilesToPath(Dir + 'lib\', '*.zip', BasePath);
  AddAllFilesToPath(Dir + 'lib\', '*.jar', BasePath);
  addAllFilesToPath(Dir + 'libs\', '*.zip', BasePath);
  AddAllFilesToPath(Dir + 'libs\', '*.jar', BasePath);
{
  addAllFilesToPath(Dir + '..\lib\', '*.zip', BasePath);
  AddAllFilesToPath(Dir + '..\lib\', '*.jar', BasePath);
  addAllFilesToPath(Dir + '..\libs\', '*.zip', BasePath);
  AddAllFilesToPath(Dir + '..\libs\', '*.jar', BasePath);
}      
  if CPath <> Nil then
    CPath.AddPath(BasePath);
end;

function TClassPath.SanityCheck(classname, filename : String) : String;
var
  fullFile, pathName, package, basePath, temp : String;
  I : Integer;
  Oops : Boolean;
begin
  fullFile := ExpandFileName(filename);
  pathName := ExtractFileDir(fullfile);
  temp := toBackSlash(classname); // temp is string where the / is now \.
  for I := length(temp) downto 1 do
  begin
    if temp[I] = '\' then break;
  end;
  if I = 0 then // no slashes, anonymous package
  begin
    AddDir(pathName); // put the filename's path on the classpath
    {$IFDEF FPC}
    SetCurrentDirectory(PChar(pathName));
    {$ELSE}
    SetCurrentDirectory(PChar(pathName));
    {$ENDIF}
     
    Exit;
  end;
  package := Copy(temp, 1, I - 1);
  Oops := Length(Package) > Length(PathName) - 3;
  if not Oops then
  begin
    Temp := Copy(PathName, 1 + Length(PathName) - Length(Package), Length(Package));
    Oops := (LowerCase(Temp) <> LowerCase(Package));
  end;
  if Oops then // There is a problem.
    raise EClasspathException.Create('File ' + fullFile + ' should be on relative path ' +package);
  basePath := Copy(pathName, 1, length(PathName) - Length(Temp));
  AddDir(basePath);
  Result := BasePath;
end;
  
function TClasspath.SanityCheckSource(filename : String) : String;
var
  Package, Classname : String;
begin
  Package := GetPackageName(Filename);
  Classname := Package + ExtractFileName(Filename);
  ChopExtension(Classname);
  Result := SanityCheck(Classname, Filename);
end;
  
// Get the package name inside a source file.
// This code is a bit messy. Maybe I'll clean it up later.
  
function GetPackageName(filename : AnsiString) : AnsiString;
var
  T : TextFile;
  inComment : Boolean;
  Line : AnsiString;
  I : Integer;
begin
 AssignFile(T, filename);
 Reset(T);
 inComment := false;
 while not Eof(T) do
 begin
   ReadLn(T, Line);
   StripComments(Line, InComment);
   I := AnsiPos('package', Line);
   if I > 0 then
   begin
     Result := Copy(Line, I + 8, length(Line));
     I := AnsiPos(';', Result);
     if I > 0 then
       begin
         Result := Trim(Copy(Result, 1, I - 1));
         break;
       end;
     if AnsiPos('{', Line) > 0 then
       break;
   end;
 end;
 CloseFile(T);
 if length(Result) > 0 then Result := Result + '.';
end;
  

end.
