unit JNIWrapper;
{
Copyright (c) 1998-2001 Jonathan Revusky
All rights reserved.

This software was enhanced and ported to 32 bit and 64 bit and 
to all the Delphi XE versions and to FreePascal by 
Amine Moulay Ramdane.

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


// an object-oriented wrapper around the JNI.
// The code here (by contrast with JavaRuntime.pas) should be
// cross-platform.

interface
{$IFDEF FPC}
   uses Windows, Classes, SysUtils, JNI, JUtils;
   {$ELSE}
   uses WinAPI.Windows, System.Classes,    System.SysUtils,System.ansistrings, JNI, JUtils;
   {$endif}

{$R-}

type


// encapsulates a JVM instance,
// wraps around the PJavaVM handle
// and provides some static methods
  TJavaVM = class
  private
    pvm : PJavaVM;
  public
    constructor Create(p : PJavaVM);
    destructor Destroy; override;
    // convenience method to call a method's static main
    // uses delphi's native TStrings to pass the
    // array of string args
    class procedure CallMain(const classname : AnsiString ; strings : TStrings);
    
    // waits until all the threads have completed.      
    procedure Wait;
  
    // Convenience method. Calls Exit procedure
    class procedure CallExit(exitCode : Integer);
  
    // procedure to explicitly detach a local reference.
    class procedure FreeRef(jobj : JObject; isGlobal : Boolean);
  
    // returns the current JNI environment pointer.
    class function GetPenv : PJNIEnv;
    
  // IMPORTANT: The following method must be called by native methods
  // that receive a penv argument if they intend to use this unit.
    class procedure SetThreadPenv(p : PJNIEnv);
    
  // This method sets whether you will only be using the JNIWrapper 
  // methods from a single thread of execution. Basically, this
  // turns off thread-safety in order to obtain better code performance.
  // Only to be used if you really know what you're doing. Even 
  // then, it's probably rarely worth it.
    class procedure SetSingleThreaded(B : Boolean);
  end; // class TJavaVM

  TJavaClass = class;
  TJavaObject = class;
  TJavaType =   (Void, Aobject, Aboolean, Abyte, Achar, Ashort, Aint, Along,Afloat,Adouble,AString,ABooleanArray,AByteArray,ACharArray,AShortArray,AIntArray,ALongArray,AFloatArray,ADoubleArray,AStringArray);
  TMethodAttribute = (static, nonstatic, nonvirtual);

{Delphi class to encapsulate list of params to Java method.}

  TJavaParams = class
  private
    RefList : TList; //a list of references to be freed by the destructor.
    Fsig : AnsiString;
    FArgPointer : Pointer;
    bufLength : Integer;
    procedure AddToArgBuffer(P : Pointer ; NumBytes : Integer); //add an element to buffer.
  public
    constructor Create;
    destructor Destroy ; override;
  // The following methods add the various types to the parameter list,
  // updating the signature as well.
    procedure AddBoolean(val : Boolean);
    procedure AddByte(val : JByte);
    procedure AddChar(val : JChar);
    procedure AddShort(val : JShort);
    procedure AddInt(val : JInt);
    procedure AddLong(val : Jlong);
    procedure AddFloat(val : JFloat);
    procedure AddDouble(val : JDouble);
    procedure AddString(val : AnsiString);
    procedure AddBooleanArray(var arr : array of JBoolean);
    procedure AddByteArray(var arr : array of JByte);
    procedure AddCharArray(var arr : array of JChar);
    procedure AddShortArray(var arr : array of JShort);
    procedure AddIntArray(var arr : array of JInt);
    procedure AddLongArray(var arr : array of Jlong);
    procedure AddFloatArray(var arr : array of JFloat);
    procedure AddDoubleArray(var arr : array of JDouble);
    procedure AddStringArray(var strings : TStrings);
    
  // In the following two methods, the second parameter
  // indicates the TJavaClass of which the object is an instance
    procedure AddObject(val : TJavaObject; jcl : TJavaClass);
    procedure AddObjectArray(arr : array of TJavaObject; jcl : TJavaClass);
    
  //the java signature of this parameter list.
    property Signature : AnsiString read FSig;
  // a pointer to the buffer that contains the Parameters to be passed.
    property ArgPointer : Pointer read FArgPointer;
  end;
  

{Delphi class to encapsulate a Java method; }

  TJavaMethod = class
  private
    Fclass : TJavaClass;
    Fsig : AnsiString;
    FmethodType : TMethodAttribute;
    FmethodID : JMethodID;
    FRetval : TJavaType;
  public
  // the constructor. The retclass is Nil unless returntype is an object.
  // raises a EJavaMethodNotFound exception if method is not found.
    constructor Create( cls : TJavaClass ;
                               name : AnsiString ;
                               methodType : TMethodAttribute ;
                               returntype : TJavaType ;
                               params : TJavaParams ;
                               retclass : TJavaClass) ;
// a minimal constructor for virtual methods that
// take no arguments and return nothing.
    constructor CreateVoid(cls : TJavaClass; name : AnsiString);
    function Call(params : TJavaParams ; jobj : TJavaObject) : jvalue;
  end;

{Delphi class to encapsulate a Java object reference.}

  TJavaObject = class
  private
    FLocalHandle : jobject;
    FGlobalHandle : jobject;
    FClass : TJavaClass;
    FPenv : PJNIEnv;
    function GetPenv : PJNIEnv;
    procedure SetGlobal(B : Boolean);
    function IsGlobal : Boolean;
    function IsValid : Boolean;
    function GetHandle : jobject;
  public
// instantiates a new object of the type passed as the first param,
// using the constructor with parameters as encapsulated by the params argument.
    constructor Create(jcl : TJavaClass ; params : TJavaParams);
// creates a wrapper object around the low-level JNI handle passed as an argument.
// to be used when you already have a JNI local object reference but want a delphi wrapper.
    constructor CreateWithHandle(jcl : TJavaClass; jobj : jobject);
    destructor Destroy; override;

// returns a native delphi string by calling the object's toString()
// if the object itself is a String, it simply copies it to a Delphi string.
    function ToString : AnsiString;
// returns true if the argument represents the same java object.
    function Equals(JavaObject : TJavaObject) : Boolean;
// returns true if this object is an instance of the java class.
    function IsInstanceOf(JavaClass : TJavaClass) : Boolean;
    property Handle : jobject read GetHandle;
    property ClassRef : TJavaClass read FClass;
    property Global : Boolean read IsGlobal write SetGlobal;
    property Valid : Boolean read IsValid;
  end;

{Delphi class to encapsulate a Java class reference.}

  TJavaClass = class(TJavaObject)
  private
    Fsig : AnsiString;
  public
// the constructor raises a EJavaClassNotFound exception if class is not found.
    constructor Create(name : AnsiString );
// a constructor that creates a TJavaClass wrapper object when it already has
// a local object ref to the class's JNI handle.
    constructor CreateWithHandle(name : AnsiString; jc : jclass);
// returns a handle to a new instance of this class.
    function Instantiate(params : TJavaParams) : TJavaObject;
    
    function Extends(JavaClass : TJavaClass) : Boolean;
    
    property Signature : ansistring read FSig;
  end;

{Exceptions to be raised when stuff goes wrong with the Java runtime.}

  EJvmException = class(Exception);
  EJavaClassNotFound = class(EJvmException);
  EJavaMethodNotFound = class(EJvmException);
  EJavaObjectInstantiation = class(EJvmException);
  EInvalidJNIHandle = class(EJvmException);
    
{ Various utility functions for creating java objects from delphi objects.}
  function createJString (s: ansistring ) : jstring;
  function createJStringArray (var strings : TStrings) : jarray;
  function createJBooleanArray (var arr : array of JBoolean) : jBooleanArray;
  function createJByteArray (var arr : array of JByte) : jByteArray;
  function createJCharArray (var arr : array of JChar) : jCharArray; 
  function createJShortArray (var arr : array of JShort) : jShortArray;
  function createJIntArray (var arr : array of JInt) : jIntArray; 
  function createJLongArray (var arr : array of JLong) : jLongArray; 
  function createJFloatArray (var arr : array of JFloat) : jFloatArray; 
  function createJDoubleArray (var arr : array of JDouble) : jDoubleArray; 
  function getStringClass : jclass;
 
 
{various utility functions for creating Delphi objects from Java objects}


  function JToDString(js : JString) : ansiString;
  function JToTStrings(jarr : JobjectArray) : TStrings;
  function JstringArrayToDTStrings(jarr : JArray) : TStrings;
  function JdoubleArrayToDdoubleArray(jarr : JdoubleArray) : TDdoubleArray;
  function JfloatArrayToDsingleArray(jarr : JFloatArray) : TDsingleArray;
  function JcharArrayToDwordArray(jarr : JCharArray) : TDwordArray;
  function JbyteArrayToDshortintArray(jarr : JByteArray) : TDshortintArray;
  function JshortArrayToDsmallintArray(jarr : JShortArray) : TDsmallintArray;
  function JbooleanArrayToDbooleanArray(jarr : JBooleanArray) : TDbooleanArray;

  function JlongArrayToDlongArray(jarr : JlongArray) : TDlongArray;
  function JintArrayToDintArray(jarr : JintArray) : TDintArray;

implementation

uses JavaRuntime;
    
threadvar
  penvThread : PJNIEnv;

var
  penvGlobal : PJNIenv;
  sc : jclass = Nil;
  SingleThreaded : Boolean;
  
function JNIPointer : PJNIEnv;
begin
  Result :=  PEnvGlobal;
  if (not SingleThreaded) or (penvGlobal = Nil) then
  begin
    Result := PEnvThread;
    if SingleThreaded then pEnvGlobal := pEnvThread;
  end;
  if Result  = Nil then 
  begin
    TJavaRuntime.getDefault.GetVM; 
    Result := penvThread;
    if SingleThreaded then 
      pEnvGlobal := pEnvThread;
  end;
  if Result = Nil then 
    raise EJVMException.Create('No penv pointer is available');
end;
    
constructor TJavaVM.Create(p : PJavaVM);
begin
  pvm := p;
end;
    
destructor TJavaVM.Destroy;
begin
  if pvm <> Nil then 
    CallExit(0);
  inherited Destroy;
end;
    
procedure TJavaVM.Wait;
begin
  if pvm<> Nil then 
    pvm^.DestroyJavaVM(pvm);
  pvm := Nil;
end;
    
class function TJavaVM.GetPenv;
begin
  Result := JNIPointer;
end;
    
class procedure TJavaVM.SetThreadPenv (p :PJNIEnv);
begin
  penvThread := p;
  penvGlobal := p;
end;
        
class procedure TJavaVM.SetSingleThreaded(B : Boolean);
begin
  if B then 
      penvGlobal := penvThread;
  SingleThreaded := B;
end;
    
class procedure TJavaVM.FreeRef(jobj : JObject; isGlobal : Boolean);
var
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  if isGlobal then 
    penv^.DeleteGlobalRef(penv, jobj)
  else 
    penv^.DeleteLocalRef(penv, jobj);
end;
    
class procedure TJavaVM.CallMain(const classname : AnsiString ; strings : TStrings);
var
  classID : jclass;
  methodID : jmethodID;
  stringArray : jarray;
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  classID := penv^.FindClass(penv, PAnsiChar(dotToSlash(classname)));
  if classID = nil then 
    raise EJavaClassNotFound.Create('Could not find class ' + classname);
  methodID := penv^.GetStaticMethodID(penv, classID, PAnsiChar('main'), PAnsiChar('([Ljava/lang/String;)V'));
  if methodID = nil then 
    raise EJavaMethodNotFound.create('Could not find main method in class ' + classname);
  stringArray := createJStringArray(strings);
  penv^.CallStaticVoidMethodV(penv, classID, methodID, @stringArray);
  FreeRef(stringArray, false);
end;

    
class procedure TJavaVM.CallExit(exitCode : Integer);
var
  classID : jclass;
  methodID : jmethodID;
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  classID := penv^.FindClass(penv, 'java/lang/System');
  methodID := penv^.GetStaticMethodID(penv, classID, 'exit', '(I)V');
  penv^.CallStaticVoidMethodV(penv, classID, methodID, @exitCode);
end;
    
constructor TJavaClass.Create(name : AnsiString);
begin
  Fpenv := JNIPointer;
  Fsig := dotToSlash(name);
  FLocalHandle := Fpenv^.FindClass(Fpenv, PAnsiChar(Fsig));
  if FLocalHandle = Nil then 
      raise EJavaClassNotFound.Create('class ' + name + ' not found.');
end;
    
constructor TJavaClass.CreateWithHandle(name : AnsiString; jc : jclass);
begin
  FPenv := JNIPointer;
  Fsig := DotToSlash(name);
  FLocalHandle := jc;
end;
    
function TJavaClass.Instantiate(params : TJavaParams) : TJavaObject;
begin
  Result := TJavaObject.Create(self, params)
end;

function TJavaClass.Extends(JavaClass : TJavaClass) : Boolean;
var
  penv : PJNIEnv;
begin
  penv := GetPenv;
  Result := penv^.isAssignableFrom(penv, Handle, JavaClass.Handle);
end;

constructor TJavaObject.Create(jcl : TJavaClass ; params : TJavaParams);
var
  Signature : AnsiString;
  MethodID : JMethodID;
  ArgPointer : Pointer;
begin
  Signature := '';
  ArgPointer := Nil;
  FClass := jcl;
  FPenv := JNIPointer;
  if params <> Nil then 
  begin
    Signature := Params.Signature;
    ArgPointer := Params.ArgPointer;
  end;
  Signature := '(' + Signature + ')V';
  MethodID := Fpenv^.GetMethodID(Fpenv, jcl.Handle, '<init>', PAnsiChar(Signature));
  if MethodID = Nil then 
    raise EJavaObjectInstantiation.Create('No such constructor ' + Signature);
  FLocalHandle := Fpenv^.NewObjectV(Fpenv, jcl.Handle, MethodID, ArgPointer);
  if FLocalHandle = Nil then 
    raise EJavaObjectInstantiation.Create('Could not create new instance of ' + jcl.signature);
end;
    
constructor TJavaObject.CreateWithHandle(jcl : TJavaClass; jobj : jobject);
begin
  FPenv := JNIPointer;
  FClass := jcl;
  FLocalHandle := jobj;
end;
        
destructor TJavaObject.Destroy;
begin
  if FGlobalHandle <> Nil then 
    TJavaVM.FreeRef(FGlobalHandle, true);
  inherited Destroy;
end;
        
function TJavaObject.GetPenv : PJNIEnv;
begin
  if IsGlobal or (FPenv = Nil) then
    Result := JNIPointer
  else
    Result := FPenv;
end;

function TJavaObject.Equals(JavaObject : TJavaObject) : Boolean;
var
  penv : PJNIEnv;
begin
  penv := GetPenv;
  if (not self.Valid) or (not JavaObject.Valid) then 
    raise EInvalidJNIHandle.Create('Attempt to use JNI local object reference in a different thread.');
  Result  := penv^.IsSameObject(penv, Handle, JavaObject.Handle);
end;
  
function TJavaObject.IsInstanceOf(JavaClass : TJavaClass) : Boolean;
var
  penv : PJNIEnv;
begin
  penv := GetPenv;
  if (not self.Valid) or (not JavaClass.Valid) then 
      raise EInvalidJNIHandle.Create('Attempt to use JNI local object reference in a different thread.');
  Result := penv^.IsInstanceOf(penv, Handle, JavaClass.Handle);
end;

procedure TJavaObject.SetGlobal(B : Boolean);
begin
  if B = GLobal then 
    Exit; 
  if B then 
    FGlobalHandle := FPenv^.NewGlobalRef(FPenv, FLocalhandle)
  else 
  begin
    FPenv := JNIPointer;
    FLocalHandle := FPenv^.NewLocalRef(FPenv, FGlobalHandle);
    Fpenv^.DeleteGlobalRef(FPenv, FGlobalHandle);
    FGlobalHandle := Nil;
  end;
end;
    
function TJavaObject.IsGlobal : Boolean;
begin
  Result := FGlobalHandle <> Nil;
end;
    
function TJavaObject.IsValid : Boolean;
begin
  if IsGlobal then
    Result := true
  else
    Result := (FLocalHandle <> Nil) and (FPenv = JNIPointer) ;
end;
  
function TJavaObject.GetHandle : jobject;
begin
  Result := FGlobalHandle;
  if Result = Nil then
    Result := FLocalHandle;
end;

function TJavaObject.ToString : AnsiString;
var
  toStringMethod : jmethodID;
  js : jstring;
  penv : PJNIEnv;
begin
  penv := GetPenv;
  toStringMethod := penv^.getMethodID(penv, classRef.Handle, 'toString', '()Ljava/lang/String;');
  js := penv^.callObjectMethod(penv, Handle, toStringMethod);
  Result := JToDString(js);
end;
    
    
constructor TJavaParams.Create;
begin
  RefList := TList.Create;
end;
    
destructor TJavaParams.Destroy;
var
  I : Integer;
begin
  for I:=0 to RefList.Count - 1 do
    TJavaVM.FreeRef(Reflist.Items[i], false);
  RefList.Free;
  if Assigned(FArgPointer) 
    then FreeMem(FArgPointer);
  inherited Destroy;
end;
  
procedure TJavaParams.AddBoolean(val : Boolean);
begin
  AddToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'Z';
end;
  
procedure TJavaParams.AddByte(val : JByte);
begin
  AddToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'B';
end;
  
procedure TJavaParams.AddChar(val : JChar);
begin
  AddToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'C';
end;
    
procedure TJavaParams.AddShort(val : JShort);
begin
  AddToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'S';
end;
  
procedure TJavaParams.AddInt(val : JInt);
begin
  AddToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'I';
end;
    
procedure TJavaParams.AddLong(val : Jlong);
begin
  AddToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'J';
end;
  
procedure TJavaParams.AddFloat(val : JFloat);
begin
  AddToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'F';
end;
    
procedure TJavaParams.AddDouble(val : JDouble);
begin
  AddToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'D';
end;
    
procedure TJavaParams.AddString(val : AnsiString);
var
  js : Jstring;
begin
  js := createJString(val);
  AddToArgBuffer(@js, sizeof(js));
  Fsig := Fsig + 'Ljava/lang/String;';
  RefList.add(js);
end;
    
procedure TJavaParams.AddObject(val : TJavaObject; jcl : TJavaClass);
var
  objHandle : JObject;
begin
  objHandle := val.Handle;
  AddToArgBuffer(@objHandle, sizeof(objHandle));
  Fsig := Fsig + 'L' + jcl.signature + ';';
end;
    
procedure TJavaParams.AddObjectArray(arr : array of TJavaObject ; jcl : TJavaClass);
var
  penv : PJNIEnv;
  jarr : jobjectarray;
  I: Integer;
begin
  penv := JNIPointer;
  jarr := penv^.NewObjectArray(penv, High(Arr)+1, jcl.Handle, arr[0].Handle);
  for I:=1+Low(arr) to High(arr) do
      penv^.setObjectArrayElement(penv, jarr, I, arr[I].Handle);
  AddToArgBuffer(@jarr, sizeof(jarr));
  Fsig := FSig + '[L' + jcl.signature + ';';
  RefList.add(jarr)
end;
    
procedure TJavaParams.AddBooleanArray(var arr : array of JBoolean);
var
  jbarray : JBooleanArray;
begin
  jbarray := createJBooleanArray(arr);
  AddToArgBuffer(@jbarray, sizeof(jbarray));
  Fsig := FSig + '[Z';
  RefList.add(jbarray)
end;
    
procedure TJavaParams.AddByteArray(var arr : array of JByte);
var
  jbarray : JByteArray;
begin
  jbarray := createJByteArray(arr);
  AddToArgBuffer(@jbarray, sizeof(jbarray));
  Fsig := FSig + '[B';
  RefList.add(jbarray)
end;
        
procedure TJavaParams.AddCharArray(var arr : array of JChar);
var
  jcarray : JCharArray;
begin
  jcarray := createJCharArray(arr);
  AddToArgBuffer(@jcarray, sizeof(jcarray));
  Fsig := FSig + '[C';
  RefList.add(jcarray)
end;
    
procedure TJavaParams.AddShortArray(var arr : array of JShort);
var
  jsarray : JShortArray;
begin
  jsarray := createJShortArray(arr);
  AddToArgBuffer(@jsarray, sizeof(jsarray));
  Fsig := FSig + '[S';
  RefList.add(jsarray)
end;
    
procedure TJavaParams.AddIntArray(var arr : array of JInt);
var
  jiarray : JIntArray;
begin
  jiarray := createJIntArray(arr);
  AddToArgBuffer(@jiarray, sizeof(jiarray));
  Fsig := FSig + '[I';
  RefList.add(jiarray)
end;
    
procedure TJavaParams.AddLongArray(var arr : array of Jlong);
var
  jlarray : JLongArray;
begin
  jlarray := createJLongArray(arr);
  AddToArgBuffer(@jlarray, sizeof(jlarray));
  Fsig := FSig + '[J';
  RefList.add(jlarray)
end;
    
procedure TJavaParams.AddFloatArray(var arr : array of JFloat);
var
  jfarray : JFloatArray;
begin
  jfarray := createJFloatArray(arr);
  AddToArgBuffer(@jfarray, sizeof(jfarray));
  Fsig := FSig + '[F';
  RefList.add(jfarray)
end;
        
procedure TJavaParams.AddDoubleArray(var arr : array of JDouble);
var
  jdarray : JDoubleArray;
begin
  jdarray := createJDoubleArray(arr);
  AddToArgBuffer(@jdarray, sizeof(jdarray));
  Fsig := FSig + '[D';
  RefList.add(jdarray)
end;
    
procedure TJavaParams.AddStringArray(var strings : TStrings);
var
  jsarray : JArray;
begin
  jsarray := createJStringArray(strings);
  AddToArgBuffer(@jsarray, sizeof(jsarray));
  Fsig := Fsig + '[Ljava/lang/String;';
  RefList.Add(jsarray)
end;
        
procedure TJavaParams.AddToArgBuffer(P : Pointer; numBytes : INteger);
var
  P1, P2 : Pointer;
  I: Integer;
begin
  ReallocMem(FArgPointer, buflength + numBytes); 
 // P1 := PAnsiChar(FArgPointer) + buflength;
 // P2 := PAnsiChar(P);
   
  P1 := Pointer(NativeInt(FArgPointer) + buflength);
  P2 := Pointer(P);
 
  for I := 0 to (numBytes - 1) do
    PAnsiChar(NativeInt(P1) + NativeInt(I))^ := pansichar(NativeInt(P2) + NativeInt(I))^;
  Inc(buflength, numBytes);	
end;
    
constructor TJavaMethod.Create(cls : TJavaClass ;
                                           name : AnsiString ;
                                           methodType : TMethodAttribute ;
                                           returntype : TJavaType ;
                                           params : TJavaParams ;
                                           retclass : TJavaClass) ;
  var
    penv : PJNIEnv;
  begin
    FClass := cls;
    if params=Nil then 
      FSig := '()'
    else 
      FSig := '(' + params.signature + ')';
    FMethodType := methodTYpe;
    FRetval := ReturnType;
    case Fretval of
      ABoolean : FSig := FSig + 'Z';
      AByte : FSig := FSig + 'B';
      AChar : FSig := FSig + 'C';
      AShort : FSig := FSig + 'S';
      AInt : FSig := FSig + 'I';
      ALong : FSIg := FSig + 'J';
      AFLoat : FSIg := FSig + 'F';
      ADouble : FSig := FSig + 'D';
      AString : FSig:=FSig+'Ljava/lang/String;';
      AObject : FSig := FSig + 'L' + retClass.Signature + ';';
      ABooleanArray: Fsig := FSig + '[Z';
      AByteArray: Fsig := FSig + '[B';
      ACharArray: Fsig := FSig + '[C';
      AShortArray: Fsig := FSig + '[S';
      AIntArray: Fsig := FSig + '[I';
      ALongArray: Fsig := FSig + '[J';
      AFloatArray: Fsig := FSig + '[F';
      ADoubleArray: Fsig := FSig + '[D';
      AStringArray: Fsig := Fsig + '[Ljava/lang/String;';

      else FSig := FSig + 'V';
      end;
    penv := JNIPointer;
    if FmethodTYpe = static then
      FmethodID := penv^.getStaticMethodID(penv, Fclass.Handle, PAnsiChar(name), PAnsiChar(FSig))
    else
      FmethodID := penv^.getMethodID(penv, Fclass.Handle, PAnsiChar(name), PAnsiChar(FSig));
    if FmethodID = Nil then
      raise EJavaMethodNotFound.Create('method ' + name + FSig + ' not found.');
  end;

constructor TJavaMethod.CreateVoid(cls : TJavaClass; name : AnsiString);
begin
  Create(cls, name, nonstatic, void, Nil, Nil);
end;

  function TJavaMethod.Call(params : TJavaParams ; jobj : TJavaObject) : jvalue;
  var
    penv: PJNIEnv;
    obj : jobject;
    argpointer : Pointer;
  begin
    penv := JNIPointer;
    argpointer := Nil;
    if params <> Nil then argPointer := params.ArgPointer;
    if jobj <> Nil then 
        obj := jobj.Handle 
    else 
        obj := Nil;
    if FmethodTYpe = static then
      case Fretval of
         void :
            penv^.CallStaticVoidMethodV(penv, FClass.Handle, FmethodID, argPointer);
         Aboolean :
            Result.z := penv^.CallStaticBooleanMethodV(penv, FClass.Handle, FmethodID, argPointer);
         Abyte :
            Result.b:= penv^.CallStaticByteMethodV(penv, FClass.Handle, FmethodID, argPointer);
         AChar :
            Result.c := penv^.CallStaticCharMethodV(penv, FClass.Handle, FmethodID, argPointer);
         AShort :
            Result.S := penv^.CallStaticShortMethodV(penv, FClass.Handle, FmethodID, argPointer);
         AInt :
            Result.I := penv^.CallStaticIntMethodV(penv, FClass.Handle, FmethodID, argPointer);
         ALong :
            Result.J := penv^.CallStaticLongMethodV(penv, FClass.Handle, FmethodID, argPointer);
         AFloat :
            Result.F := penv^.CallStaticFloatMethodV(penv, FClass.Handle, FmethodID, argPointer);
         ADouble:
            Result.D := penv^.CallStaticDoubleMethodV(penv, FClass.Handle, FmethodID, argPointer);
         AObject :
            Result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
 AString :
            Result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
 ADoubleArray:
            Result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
ABooleanArray:
            Result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
AByteArray:
            Result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
ACharArray:
            Result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
AShortArray:
            Result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
AIntArray: 
 Result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
ALongArray:
Result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
AFloatArray:
Result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
AStringArray: 
Result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);

      end;
  
   if FmethodTYpe = nonvirtual then
      case Fretval of
         void :
            penv^.CallNonvirtualVoidMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         Aboolean :
            Result.z := penv^.CallNonVirtualBooleanMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         Abyte :
            Result.b:= penv^.CallNonVirtualByteMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         AChar :
            Result.c := penv^.CallNonVirtualCharMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         AShort :
            Result.S := penv^.CallNonVirtualShortMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         AInt :
            Result.I := penv^.CallNonVirtualIntMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         ALong :
            Result.J := penv^.CallNonVirtualLongMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         AFloat :
            Result.F := penv^.CallNonVirtualFloatMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         ADouble:
            Result.D := penv^.CallNonVirtualDoubleMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         AObject :
            Result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AString :
           Result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
 ADoubleArray:
            Result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
ABooleanArray:
            Result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AByteArray:
            Result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
ACharArray:
            Result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AShortArray:
            Result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AIntArray:
Result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
ALongArray:
Result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AFloatArray:
Result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AStringArray: 
Result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
      
end;
   
   if FmethodTYpe = nonstatic then
      case Fretval of
         void :
            penv^.CallVoidMethodV(penv, obj, FmethodID, argPointer);
         Aboolean :
            Result.z := penv^.CallBooleanMethodV(penv, obj, FmethodID, argPointer);
         Abyte :
            Result.b:= penv^.CallByteMethodV(penv, obj, FmethodID, argPointer);
         AChar :
            Result.c := penv^.CallCharMethodV(penv, obj, FmethodID, argPointer);
         AShort :
            Result.S := penv^.CallShortMethodV(penv, obj, FmethodID, argPointer);
         AInt :
            Result.I := penv^.CallIntMethodV(penv, obj, FmethodID, argPointer);
         ALong :
            Result.J := penv^.CallLongMethodV(penv, obj, FmethodID, argPointer);
         AFloat :
            Result.F := penv^.CallFloatMethodV(penv, obj, FmethodID, argPointer);
         ADouble:
            Result.D := penv^.CallDoubleMethodV(penv, obj, FmethodID, argPointer);
         AObject :
            Result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
 AString :
            Result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
 ADoubleArray:
              Result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
ABooleanArray:
              Result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
AByteArray:
             Result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
ACharArray:
              Result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
AShortArray:
             Result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
AIntArray:
           Result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
ALongArray:
 Result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
AFloatArray:
Result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
AStringArray: 
Result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
   end;
  end;
    
function createJString (s : ansistring) : jstring;
var
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  Result := penv^.NewStringUTF(penv, PAnsiChar(s));
end;

function JToDString(js : JString) : ansiString;
var
  penv : PJNIEnv;
  len : NativeInt;
  CharBuf : PAnsiChar;
  IsCopy : JBoolean;
begin
  penv := JNIPointer;
  CharBuf := penv^.GetStringUTFChars(penv, js, IsCopy);
  len := penv^.GetStringUTFLength(penv, js);
  SetLength(Result, 1 + len);
   
  {$IFDEF FPC}
  StrLCopy(PAnsiChar(result), Charbuf, len);
  {$ELSE}
  system.AnsiStrings.StrLCopy(PAnsiChar(result), Charbuf, len);
  {$endif}
    
  if IsCopy then
    penv^.ReleaseStringUTFChars(penv, js, CharBuf);
end;

function JToTStrings(jarr : JObjectarray) : Tstrings;
var
  penv : PJNIEnv;
  jobj : jobject;
  len, I:NativeInt;
begin
  penv := JNIPointer;
  Result := TStringList.Create;
  len := penv^.GetArrayLength(penv, jarr);

  for I:=1 to len do
  begin
    jobj := penv^.GetObjectArrayElement(penv, jarr, I - 1);
    Result.add(JToDString(jobj));
  end;
end;
    
function JstringArrayToDTStrings(jarr : Jarray) : Tstrings;
var
  penv : PJNIEnv;
  jobj : jobject;
  len, I : NativeInt;
begin
  penv := JNIPointer;
  Result := TStringList.Create;
  len := penv^.GetArrayLength(penv, jarr);
  i := 0;
  if len > 0 then 
  begin
    repeat
      inc(i);
      jobj := penv^.GetObjectArrayElement(penv, jarr, I - 1);
      Result.add(JToDString(jobj)); 
    until i=len;  
  end;
end;
 
function JdoubleArrayToDdoubleArray(jarr : JdoubleArray) : TDdoubleArray;
var
  penv : PJNIEnv;
  len, I : NativeInt;
  d1: PJDouble;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetDoubleArrayElements(penv, jarr, 0);
  i := 0;
  if len > 0 then 
  begin
    SetLength(Result, 1000);
    repeat
      inc(i);
      if (i - 1) = Length(Result) then  
        SetLength(Result, length(Result) + 1000);
      Result[i - 1] := TPdoubleArray(d1)[i - 1];
    until i > len;  
  end;     
  SetLength(Result, len);	
end;

function JfloatArrayToDsingleArray(jarr : JFloatArray) : TDsingleArray;
var
  penv : PJNIEnv;
  len, I : NativeInt;
  d1: PJFloat;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetFloatArrayElements(penv, jarr, 0);
  i := 0;
  if len > 0 then 
  begin
    SetLength(Result, 1000);
    repeat
      Inc(i);
      if (i - 1) = length(Result) then  
        SetLength(Result, length(Result) + 1000);
      Result[i - 1] := TPsingleArray(d1)[i - 1];
    until i > len;  
  end;     
  SetLength(Result, len);	
end;

function JCharArrayToDwordArray(jarr : JCharArray) : TDwordArray;
var
  penv : PJNIEnv;
  len, I : NativeInt;
  d1: PJChar;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetCharArrayElements(penv, jarr,0);
  i := 0;
  if len > 0 then 
  begin
    SetLength(Result, 1000);
    repeat
      Inc(i);
      if (i - 1) = length(Result) then  
        SetLength(Result, length(Result) + 1000);
      Result[i - 1] := TPwordArray(d1)[i - 1];
    until i>len;  
  end;     
  SetLength(Result, len);	
end;

function JbyteArrayToDshortintArray(jarr : JbyteArray) : TDshortintArray;
var
  penv : PJNIEnv;
  len, I : NativeInt;
  d1: PJByte;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetByteArrayElements(penv, jarr, 0);
  i:=0;
  if len > 0 then 
  begin
  SetLength(Result, 1000);
  repeat
    Inc(i);
    if (i - 1) = length(Result) then  
      SetLength(Result, length(Result) + 1000);
    Result[i - 1] := TPshortintArray(d1)[i - 1];
    until i > len;  
  end;     
  SetLength(Result, len);	
end;

function JshortArrayToDsmallintArray(jarr : JShortArray) : TDsmallintArray;
var
  penv : PJNIEnv;
  len, I : NativeInt;
  d1:PJShort;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetShortArrayElements(penv, jarr, 0);
  i:= 0;
  if len > 0 then 
  begin
  SetLength(Result,1000);
  repeat
    Inc(i);
    if (i - 1) = length(Result) then  
      SetLength(Result, Length(Result) + 1000);
    Result[i-1] := TPsmallintArray(d1)[i - 1];
    until i > len;  
  end;     
  SetLength(Result, len);	
end;

function JbooleanArrayToDbooleanArray(jarr : JBooleanArray) : TDbooleanArray;
var
  penv : PJNIEnv;
  len, I : NativeInt;
  d1: PJBoolean;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetBooleanArrayElements(penv, jarr, 0);
  i := 0;
  if len > 0 then 
  begin
    SetLength(Result, 1000);
    repeat
      Inc(i);
      if (i - 1) = length(Result) then  
        SetLength(Result, Length(Result) + 1000);
      Result[i - 1] := TPbooleanArray(d1)[i - 1];
    until i > len;  
  end;     
  SetLength(Result, len);	
end;

function JlongArrayToDlongArray(jarr : JlongArray) : TDlongArray;
var
  penv : PJNIEnv;
  len, I : NativeInt;
  d1: PJLong;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetLongArrayElements(penv, jarr, 0);
  i := 0;
  if len > 0 then 
  begin
    SetLength(Result, 1000);
    repeat
    inc(i);  
    if (i - 1) = length(Result) then  SetLength(Result, length(Result) + 1000);
    Result[i - 1] := TPlongArray(d1)[i - 1];
    until i > len;  
  end;     
  SetLength(Result, len);
end;
  
function JintArrayToDintArray(jarr : JintArray) : TDintArray;
var
  penv : PJNIEnv;
  len, I : NativeInt;
  d1: PJInt;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetIntArrayElements(penv, jarr, 0);
  i := 0;
  if len > 0 then 
  begin
    SetLength(Result, 1000);
    repeat
      Inc(i);
      if (i - 1) = length(Result) then  
        SetLength(Result, length(Result) + 1000);
      Result[i - 1] := TPintArray(d1)[i - 1];
    until i > len;  
  end;     
  SetLength(Result, len);
end;
  
function getStringClass : jclass;
var
  penv : PJNIEnv;
begin
  if sc = Nil then 
  begin
    penv := JNIPointer;
    sc := penv^.FindClass(JNIPointer, 'java/lang/String');
    sc := penv^.NewGlobalRef(penv, sc);
  end;
  result := sc;
end;
    
function createJStringArray (var strings : TStrings) : jarray;
var
  I, count: NativeInt;
  js : jstring;
  penv : PJNIEnv;
 begin
  penv := JNIPointer;
  count := 0;
  if strings <> Nil then 
    count := strings.count;
  js := createJString('');
  Result := penv^.NewObjectArray(penv, count, getStringClass, js);
  for I := 0 to count - 1 do
  begin
  js := createJString(ansistring(strings.strings[i]));
  penv^.SetObjectArrayElement(penv, Result, I, js);
  end;
end;
    
function createJBooleanArray (var arr : array of JBoolean) : JBooleanArray;
var
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  Result := penv^.newBooleanArray(penv, High(arr) + 1);
  penv^.setBooleanArrayRegion(penv, Result, low(arr), High(arr) + 1, @arr);
end;
    
function createJByteArray (var arr : array of JByte) : JByteArray;
var
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  Result := penv^.newByteArray(penv, High(arr) + 1);
  penv^.setByteArrayRegion(penv, Result, 0, High(arr) + 1, @arr);
end;
    
function createJCharArray (var arr : array of JChar) : JCharArray;
var
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  Result := penv^.newCharArray(penv, High(arr) + 1);
  penv^.setCharArrayRegion(penv, Result, low(arr), High(arr) + 1, @arr);
end;
    
function createJShortArray (var arr : array of JShort) : JShortArray;
var
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  Result := penv^.newShortArray(penv, High(arr) + 1);
  penv^.setShortArrayRegion(penv, Result, 0, High(arr) + 1, @arr);
end;
    
function createJIntArray (var arr : array of Jint) : JIntArray;
var
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  Result := penv^.newIntArray(penv, High(arr) + 1);
  penv^.setIntArrayRegion(penv, Result, low(arr), High(arr) + 1, @arr);
end;
  
function createJLongArray (var arr : array of JLong) : JLongArray;
var
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  Result := penv^.newLongArray(penv, High(arr) + 1);
  penv^.setLongArrayRegion(penv, Result, low(arr), High(arr) + 1, @arr);
end;

function createJFloatArray (var arr : array of JFloat) : JFloatArray;
var
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  Result := penv^.newFloatArray(penv, High(arr) + 1);
  penv^.setFloatArrayRegion(penv, Result, low(arr), High(arr) + 1, @arr);
end;
      
function createJDoubleArray (var arr : array of JDouble) : JDoubleArray;
var
  penv : PJNIEnv;
begin
  penv := JNIPointer;
  Result := penv^.newDoubleArray(penv, High(arr) + 1);
  penv^.setDoubleArrayRegion(penv, Result, 0, High(arr) + 1, @arr);
end;

end.

{$R+}