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
    class procedure freeRef(jobj : JObject; isGlobal : Boolean);
  
    // returns the current JNI environment pointer.
    class function getPenv : PJNIEnv;
    
  // IMPORTANT: The following method must be called by native methods
  // that receive a penv argument if they intend to use this unit.
    class procedure setThreadPenv(p : PJNIEnv);
    
  // This method sets whether you will only be using the JNIWrapper 
  // methods from a single thread of execution. Basically, this
  // turns off thread-safety in order to obtain better code performance.
  // Only to be used if you really know what you're doing. Even 
  // then, it's probably rarely worth it.
    class procedure setSingleThreaded(B : Boolean);
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
    procedure addToArgBuffer(P : Pointer ; NumBytes : Integer); //add an element to buffer.
  public
    constructor Create;
    destructor Destroy ; override;
  // The following methods add the various types to the parameter list,
  // updating the signature as well.
    procedure addBoolean(val : Boolean);
    procedure addByte(val : JByte);
    procedure addChar(val : JChar);
    procedure addShort(val : JShort);
    procedure addInt(val : JInt);
    procedure addLong(val : Jlong);
    procedure addFloat(val : JFloat);
    procedure addDouble(val : JDouble);
    procedure addString(val : AnsiString);
    procedure addBooleanArray(var arr : array of JBoolean);
    procedure addByteArray(var arr : array of JByte);
    procedure addCharArray(var arr : array of JChar);
    procedure addShortArray(var arr : array of JShort);
    procedure addIntArray(var arr : array of JInt);
    procedure addLongArray(var arr : array of Jlong);
    procedure addFloatArray(var arr : array of JFloat);
    procedure addDoubleArray(var arr : array of JDouble);
    procedure addStringArray(var strings : TStrings);
    
  // In the following two methods, the second parameter
  // indicates the TJavaClass of which the object is an instance
    procedure addObject(val : TJavaObject; jcl : TJavaClass);
    procedure addObjectArray(arr : array of TJavaObject; jcl : TJavaClass);
    
  //the java signature of this parameter list.
    property Signature : AnsiString read FSig;
  // a pointer to the buffer that contains the Parameters to be passed.
    property argPointer : Pointer read FArgPointer;
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
    function getPenv : PJNIEnv;
    procedure setGlobal(B : Boolean);
    function isGlobal : Boolean;
    function isValid : Boolean;
    function getHandle : jobject;
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
    function toString : AnsiString;
// returns true if the argument represents the same java object.
    function equals(JavaObject : TJavaObject) : Boolean;
// returns true if this object is an instance of the java class.
    function isInstanceOf(JavaClass : TJavaClass) : Boolean;
    property Handle : jobject read GetHandle;
    property ClassRef : TJavaClass read FClass;
    property Global : Boolean read isGlobal write setGlobal;
    property Valid : Boolean read isValid;
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
    
    function extends(JavaClass : TJavaClass) : Boolean;
    
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
    result :=  PEnvGlobal;
    if (not SingleThreaded) or (penvGlobal = Nil) then
    begin
      result := PEnvThread;
      if SingleThreaded then pEnvGlobal := pEnvThread;
    end;
    if result  = Nil then 
    begin
      TJavaRuntime.getDefault.GetVM; 
      result := penvThread;
      if SingleThreaded then 
        pEnvGlobal := pEnvThread;
    end;
    if result = Nil then 
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
    
  class function TJavaVM.getPenv;
  begin
    result := JNIPointer;
  end;
    
  class procedure TJavaVM.setThreadPEnv (p :PJNIEnv);
  begin
    penvThread := p;
    penvGlobal := p;
  end;
        
  class procedure TJavaVM.setSingleThreaded(B : Boolean);
  begin
    if B then 
        penvGlobal := penvThread;
    SingleThreaded := B;
  end;
    
  class procedure TJavaVM.freeRef(jobj : JObject; isGlobal : Boolean);
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
    result := TJavaObject.Create(self, params)
  end;

  function TJavaClass.extends(JavaClass : TJavaClass) : Boolean;
  var
    penv : PJNIEnv;
  begin
    penv := getPenv;
    result := penv^.isAssignableFrom(penv, Handle, JavaClass.Handle);
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
      TJavaVM.freeRef(FGlobalHandle, true);
    inherited Destroy;
  end;
        
  function TJavaObject.getPenv : PJNIEnv; 
  begin
    if isGlobal or (FPenv = Nil) then 
      result := JNIPointer
    else
      result := FPenv;
  end;

  function TJavaObject.equals(JavaObject : TJavaObject) : Boolean;
  var
    penv : PJNIEnv;
  begin
    penv := getPenv;
    if (not self.Valid) or (not JavaObject.Valid) then 
      raise EInvalidJNIHandle.Create('Attempt to use JNI local object reference in a different thread.');
    result  := penv^.IsSameObject(penv, Handle, JavaObject.Handle);
  end;
  
  function TJavaObject.isInstanceOf(JavaClass : TJavaClass) : Boolean;
  var
    penv : PJNIEnv;
  begin
    penv := GetPenv;
    if (not self.Valid) or (not JavaClass.Valid) then 
        raise EInvalidJNIHandle.Create('Attempt to use JNI local object reference in a different thread.');
    result := penv^.IsInstanceOf(penv, Handle, JavaClass.Handle);
  end;

  procedure TJavaObject.setGlobal(B : Boolean);
  begin
    if B = GLobal then 
      Exit; 
    if B then 
      FGlobalHandle := FPenv^.NewGlobalRef(FPenv, FLocalhandle)
    else begin
      FPenv := JNIPointer;
      FLocalHandle := FPenv^.NewLocalRef(FPenv, FGlobalHandle);
      Fpenv^.DeleteGlobalRef(FPenv, FGlobalHandle);
      FGlobalHandle := Nil;
    end;
  end;
    
    function TJavaObject.isGlobal : Boolean;
    begin
      result := FGlobalHandle <> Nil;
    end;
    
  function TJavaObject.isValid : Boolean;
  begin
    if isGlobal then
      result := true
    else
      result := (FLocalHandle <> Nil) and (FPenv = JNIPointer) ;
  end;
  
  function TJavaObject.getHandle : jobject;
      begin
         result := FGlobalHandle;
         if result = Nil then
             Result := FLocalHandle;
      end;

  function TJavaObject.toString : AnsiString;
  var
    toStringMethod : jmethodID;
    js : jstring;
    penv : PJNIEnv;
  begin
    penv := getPenv;
    toStringMethod := penv^.getMethodID(penv, classRef.Handle, 'toString', '()Ljava/lang/String;');
    js := penv^.callObjectMethod(penv, Handle, toStringMethod);
    result := JToDString(js);
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
  
  procedure TJavaParams.addBoolean(val : Boolean);
  begin
    addToArgBuffer(@val, sizeof(val));
    Fsig := Fsig + 'Z';
  end;
  
  procedure TJavaParams.addByte(val : JByte);
  begin
    addToArgBuffer(@val, sizeof(val));
    Fsig := Fsig + 'B';
  end;
  
  procedure TJavaParams.addChar(val : JChar);
  begin
    addToArgBuffer(@val, sizeof(val));
    Fsig := Fsig + 'C';
  end;
    
  procedure TJavaParams.addShort(val : JShort);
  begin
    addToArgBuffer(@val, sizeof(val));
    Fsig := Fsig + 'S';
  end;
  
  procedure TJavaParams.addInt(val : JInt);
  begin
    addToArgBuffer(@val, sizeof(val));
    Fsig := Fsig + 'I';
  end;
    
  procedure TJavaParams.addLong(val : Jlong);
  begin
    addToArgBuffer(@val, sizeof(val));
    Fsig := Fsig + 'J';
  end;
  
  procedure TJavaParams.addFloat(val : JFloat);
  begin
    addToArgBuffer(@val, sizeof(val));
    Fsig := Fsig + 'F';
  end;
    
  procedure TJavaParams.addDouble(val : JDouble);
  begin
    addToArgBuffer(@val, sizeof(val));
    Fsig := Fsig + 'D';
  end;
    
  procedure TJavaParams.addString(val : AnsiString);
  var
    js : Jstring;
  begin
    js := createJString(val);
    addToArgBuffer(@js, sizeof(js));
    Fsig := Fsig + 'Ljava/lang/String;';
    RefList.add(js);
  end;
    
  procedure TJavaParams.addObject(val : TJavaObject; jcl : TJavaClass);
  var
    objHandle : JObject;
  begin
    objHandle := val.Handle;
    addToArgBuffer(@objHandle, sizeof(objHandle));
    Fsig := Fsig + 'L' + jcl.signature + ';';
  end;
    
  procedure TJavaParams.addObjectArray(arr : array of TJavaObject ; jcl : TJavaClass);
  var
    penv : PJNIEnv;
    jarr : jobjectarray;
    I: Integer;
  begin
    penv := JNIPointer;
    jarr := penv^.NewObjectArray(penv, High(Arr)+1, jcl.Handle, arr[0].Handle);
    for I:=1+Low(arr) to High(arr) do
        penv^.setObjectArrayElement(penv, jarr, I, arr[I].Handle);
    addToArgBuffer(@jarr, sizeof(jarr));
    Fsig := FSig + '[L' + jcl.signature + ';';
    RefList.add(jarr)
  end;
    
  procedure TJavaParams.addBooleanArray(var arr : array of JBoolean);
  var
    jbarray : JBooleanArray;
  begin
    jbarray := createJBooleanArray(arr);
    addToArgBuffer(@jbarray, sizeof(jbarray));
    Fsig := FSig + '[Z';
    RefList.add(jbarray)
  end;
    
  procedure TJavaParams.addByteArray(var arr : array of JByte);
  var
    jbarray : JByteArray;
  begin
    jbarray := createJByteArray(arr);
    addToArgBuffer(@jbarray, sizeof(jbarray));
    Fsig := FSig + '[B';
    RefList.add(jbarray)
  end;
        
  procedure TJavaParams.addCharArray(var arr : array of JChar);
  var
    jcarray : JCharArray;
  begin
    jcarray := createJCharArray(arr);
    addToArgBuffer(@jcarray, sizeof(jcarray));
    Fsig := FSig + '[C';
    RefList.add(jcarray)
  end;
    
  procedure TJavaParams.addShortArray(var arr : array of JShort);
  var
    jsarray : JShortArray;
  begin
    jsarray := createJShortArray(arr);
    addToArgBuffer(@jsarray, sizeof(jsarray));
    Fsig := FSig + '[S';
    RefList.add(jsarray)
  end;
    
  procedure TJavaParams.addIntArray(var arr : array of JInt);
  var
    jiarray : JIntArray;
  begin
    jiarray := createJIntArray(arr);
    addToArgBuffer(@jiarray, sizeof(jiarray));
    Fsig := FSig + '[I';
    RefList.add(jiarray)
  end;
    
  procedure TJavaParams.addLongArray(var arr : array of Jlong);
  var
    jlarray : JLongArray;
  begin
    jlarray := createJLongArray(arr);
    addToArgBuffer(@jlarray, sizeof(jlarray));
    Fsig := FSig + '[J';
    RefList.add(jlarray)
  end;
    
  procedure TJavaParams.addFloatArray(var arr : array of JFloat);
  var
    jfarray : JFloatArray;
  begin
    jfarray := createJFloatArray(arr);
    addToArgBuffer(@jfarray, sizeof(jfarray));
    Fsig := FSig + '[F';
    RefList.add(jfarray)
  end;
        
  procedure TJavaParams.addDoubleArray(var arr : array of JDouble);
  var
    jdarray : JDoubleArray;
  begin
    jdarray := createJDoubleArray(arr);
    addToArgBuffer(@jdarray, sizeof(jdarray));
    Fsig := FSig + '[D';
    RefList.add(jdarray)
  end;
    
  procedure TJavaParams.addStringArray(var strings : TStrings);
  var
    jsarray : JArray;
  begin
    jsarray := createJStringArray(strings);
    addToArgBuffer(@jsarray, sizeof(jsarray));
    Fsig := Fsig + '[Ljava/lang/String;';
    RefList.add(jsarray)
  end;
    
    
  procedure TJavaParams.addToArgBuffer(P : Pointer; numBytes : INteger);
  var
    P1, P2 : Pointer;
    I: INteger;
  begin
    ReallocMem(FArgPointer, buflength + numBytes); 
   // P1 := PAnsiChar(FArgPointer) + buflength;
   // P2 := PAnsiChar(P);
   
     P1 := Pointer(NativeInt(FArgPointer) + buflength);
    P2 := Pointer(P);
 

  for I:=0 to (numBytes- 1) do 
        PAnsiChar(NativeInt(P1) +NativeInt(I))^ := pansichar(NativeInt(P2)+NativeInt(I))^;
    inc(buflength, numBytes);
	
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
    if params <> Nil then argPointer := params.argpointer;
    if jobj <> Nil then 
        obj := jobj.Handle 
    else 
        obj := Nil;
    if FmethodTYpe = static then
      case Fretval of
         void :
            penv^.CallStaticVoidMethodV(penv, FClass.Handle, FmethodID, argPointer);
         Aboolean :
            result.z := penv^.CallStaticBooleanMethodV(penv, FClass.Handle, FmethodID, argPointer);
         Abyte :
            result.b:= penv^.CallStaticByteMethodV(penv, FClass.Handle, FmethodID, argPointer);
         AChar :
            result.c := penv^.CallStaticCharMethodV(penv, FClass.Handle, FmethodID, argPointer);
         AShort :
            result.S := penv^.CallStaticShortMethodV(penv, FClass.Handle, FmethodID, argPointer);
         AInt :
            result.I := penv^.CallStaticIntMethodV(penv, FClass.Handle, FmethodID, argPointer);
         ALong :
            result.J := penv^.CallStaticLongMethodV(penv, FClass.Handle, FmethodID, argPointer);
         AFloat :
            result.F := penv^.CallStaticFloatMethodV(penv, FClass.Handle, FmethodID, argPointer);
         ADouble:
            result.D := penv^.CallStaticDoubleMethodV(penv, FClass.Handle, FmethodID, argPointer);
         AObject :
            result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
 AString :
            result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
 ADoubleArray:
            result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
ABooleanArray:
            result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
AByteArray:
            result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
ACharArray:
            result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
AShortArray:
            result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
AIntArray: 
 result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
ALongArray:
result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
AFloatArray:
result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);
AStringArray: 
result.l := penv^.CallStaticObjectMethodV(penv, FClass.Handle, FmethodID, argPointer);

      end;
  
   if FmethodTYpe = nonvirtual then
      case Fretval of
         void :
            penv^.CallNonvirtualVoidMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         Aboolean :
            result.z := penv^.CallNonVirtualBooleanMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         Abyte :
            result.b:= penv^.CallNonVirtualByteMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         AChar :
            result.c := penv^.CallNonVirtualCharMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         AShort :
            result.S := penv^.CallNonVirtualShortMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         AInt :
            result.I := penv^.CallNonVirtualIntMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         ALong :
            result.J := penv^.CallNonVirtualLongMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         AFloat :
            result.F := penv^.CallNonVirtualFloatMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         ADouble:
            result.D := penv^.CallNonVirtualDoubleMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
         AObject :
            result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AString :
           result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
 ADoubleArray:
            result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
ABooleanArray:
            result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AByteArray:
            result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
ACharArray:
            result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AShortArray:
            result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AIntArray:
result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
ALongArray:
result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AFloatArray:
result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
AStringArray: 
result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, FClass.Handle, FmethodID, argPointer);
      
end;
   
   if FmethodTYpe = nonstatic then
      case Fretval of
         void :
            penv^.CallVoidMethodV(penv, obj, FmethodID, argPointer);
         Aboolean :
            result.z := penv^.CallBooleanMethodV(penv, obj, FmethodID, argPointer);
         Abyte :
            result.b:= penv^.CallByteMethodV(penv, obj, FmethodID, argPointer);
         AChar :
            result.c := penv^.CallCharMethodV(penv, obj, FmethodID, argPointer);
         AShort :
            result.S := penv^.CallShortMethodV(penv, obj, FmethodID, argPointer);
         AInt :
            result.I := penv^.CallIntMethodV(penv, obj, FmethodID, argPointer);
         ALong :
            result.J := penv^.CallLongMethodV(penv, obj, FmethodID, argPointer);
         AFloat :
            result.F := penv^.CallFloatMethodV(penv, obj, FmethodID, argPointer);
         ADouble:
            result.D := penv^.CallDoubleMethodV(penv, obj, FmethodID, argPointer);
         AObject :
            result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
 AString :
            result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
 ADoubleArray:
              result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
ABooleanArray:
              result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
AByteArray:
             result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
ACharArray:
              result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
AShortArray:
             result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
AIntArray:
           result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
ALongArray:
 result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
AFloatArray:
result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
AStringArray: 
result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
   end;
  end;
    
  function createJString (s : ansistring) : jstring;
  var
   penv : PJNIEnv;
  begin
   penv := JNIPointer;
   result := penv^.NewStringUTF(penv, PAnsiChar(s));
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
    SetLength(Result, 1+len);
   
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
    len,I:NativeInt;
  begin
    penv := JNIPointer;
    result := TStringList.Create;
    len := penv^.GetArrayLength(penv, jarr);
    
   for I:=1 to len
    Do
      begin
      jobj := penv^.GetObjectArrayElement(penv, jarr, I-1);
      result.add(JToDString(jobj));  
     end; 
  end;
    
function JstringArrayToDTStrings(jarr : Jarray) : Tstrings;
  var
    penv : PJNIEnv;
    jobj : jobject;
    len, I : NativeInt;
  begin
    penv := JNIPointer;
    result := TStringList.Create;
    len := penv^.GetArrayLength(penv, jarr);
	i:=0;
	if len >0
	then 
	 begin
	 repeat
     inc(i);
	 jobj := penv^.GetObjectArrayElement(penv, jarr, I-1);
     result.add(JToDString(jobj)); 
    until i=len;  
	end;
  end;
 
 function JdoubleArrayToDdoubleArray(jarr : JdoubleArray) : TDdoubleArray;
  var
    penv : PJNIEnv;
    len, I : NativeInt;
    d1:PJDouble;
  begin
    penv := JNIPointer;
    len := penv^.GetArrayLength(penv, jarr);
    d1:=penv^.GetDoubleArrayElements(penv, jarr,0);
	i:=0;
	if len >0
	then 
	 begin
	 setlength(result,1000);
	repeat
     inc(i);
	 if (i-1)=length(result) then  setlength(result,length(result)+1000);
    result[i-1]:=TPdoubleArray(d1)[i-1];
    until i>len;  
  end;     
setlength(result,len);	
  end;

function JfloatArrayToDsingleArray(jarr : JFloatArray) : TDsingleArray;
  var
    penv : PJNIEnv;
    len, I : NativeInt;
    d1:PJFloat;
  begin
    penv := JNIPointer;
    len := penv^.GetArrayLength(penv, jarr);
    d1:=penv^.GetFloatArrayElements(penv, jarr,0);
	i:=0;
	if len >0
	then 
	 begin
	 setlength(result,1000);
	repeat
     inc(i);
	 if (i-1)=length(result) then  setlength(result,length(result)+1000);
    result[i-1]:=TPsingleArray(d1)[i-1];
    until i>len;  
  end;     
setlength(result,len);	
  end;

function JCharArrayToDwordArray(jarr : JCharArray) : TDwordArray;
  var
    penv : PJNIEnv;
    len, I : NativeInt;
    d1:PJChar;
  begin
    penv := JNIPointer;
    len := penv^.GetArrayLength(penv, jarr);
    d1:=penv^.GetCharArrayElements(penv, jarr,0);
	i:=0;
	if len >0
	then 
	 begin
	 setlength(result,1000);
	repeat
     inc(i);
	 if (i-1)=length(result) then  setlength(result,length(result)+1000);
    result[i-1]:=TPwordArray(d1)[i-1];
    until i>len;  
  end;     
setlength(result,len);	
  end;

function JbyteArrayToDshortintArray(jarr : JbyteArray) : TDshortintArray;
  var
    penv : PJNIEnv;
    len, I : NativeInt;
    d1:PJByte;
  begin
    penv := JNIPointer;
    len := penv^.GetArrayLength(penv, jarr);
    d1:=penv^.GetByteArrayElements(penv, jarr,0);
	i:=0;
	if len >0
	then 
	 begin
	 setlength(result,1000);
	repeat
     inc(i);
	 if (i-1)=length(result) then  setlength(result,length(result)+1000);
    result[i-1]:=TPshortintArray(d1)[i-1];
    until i>len;  
  end;     
setlength(result,len);	
  end;

function JshortArrayToDsmallintArray(jarr : JShortArray) : TDsmallintArray;
  var
    penv : PJNIEnv;
    len, I : NativeInt;
    d1:PJShort;
  begin
    penv := JNIPointer;
    len := penv^.GetArrayLength(penv, jarr);
    d1:=penv^.GetShortArrayElements(penv, jarr,0);
	i:=0;
	if len >0
	then 
	 begin
	 setlength(result,1000);
	repeat
     inc(i);
	 if (i-1)=length(result) then  setlength(result,length(result)+1000);
    result[i-1]:=TPsmallintArray(d1)[i-1];
    until i>len;  
  end;     
setlength(result,len);	
  end;

function JbooleanArrayToDbooleanArray(jarr : JBooleanArray) : TDbooleanArray;
  var
    penv : PJNIEnv;
    len, I : NativeInt;
    d1:PJBoolean;
  begin
    penv := JNIPointer;
    len := penv^.GetArrayLength(penv, jarr);
    d1:=penv^.GetBooleanArrayElements(penv, jarr,0);
	i:=0;
	if len >0
	then 
	 begin
	 setlength(result,1000);
	repeat
     inc(i);
	 if (i-1)=length(result) then  setlength(result,length(result)+1000);
    result[i-1]:=TPbooleanArray(d1)[i-1];
    until i>len;  
  end;     
setlength(result,len);	
  end;

function JlongArrayToDlongArray(jarr : JlongArray) : TDlongArray;
  var
    penv : PJNIEnv;
    len, I : NativeInt;
    d1:PJLong;
  begin
    penv := JNIPointer;
    len := penv^.GetArrayLength(penv, jarr);
    d1:=penv^.GetLongArrayElements(penv, jarr,0);
	i:=0;
	if len >0
	then 
	 begin
	 setlength(result,1000);
	repeat
    inc(i);  
    if (i-1)=length(result) then  setlength(result,length(result)+1000);
    result[i-1]:=TPlongArray(d1)[i-1];
   	until i>len;  
 end;     
setlength(result,len);
 end;
  
function JintArrayToDintArray(jarr : JintArray) : TDintArray;
  var
    penv : PJNIEnv;
    len, I : NativeInt;
    d1:PJInt;
  begin
    penv := JNIPointer;
    len := penv^.GetArrayLength(penv, jarr);
    d1:=penv^.GetIntArrayElements(penv, jarr,0);
	i:=0;
	if len >0
	then 
	 begin
	 setlength(result,1000);
	repeat
    inc(i);
    if (i-1)=length(result) then  setlength(result,length(result)+1000);
    result[i-1]:=TPintArray(d1)[i-1];
   	until i>len;  
 end;     
	setlength(result,len);
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
    if strings<>Nil then 
      count := strings.count;
      js := createJString('');
	  result := penv^.NewObjectArray(penv, count, getStringClass, js);
    for I :=0 to count-1 do
    begin
	  js := createJString(ansistring(strings.strings[i]));
	  penv^.SetObjectArrayElement(penv, result, I, js);
    end;
  end;
    
  function createJBooleanArray (var arr : array of JBoolean) : JBooleanArray;
  var
    penv : PJNIEnv;
  begin
    penv := JNIPointer;
    result := penv^.newBooleanArray(penv, High(arr) + 1);
    penv^.setBooleanArrayRegion(penv, result, low(arr), High(arr) +1, @arr);
  end;
    
  function createJByteArray (var arr : array of JByte) : JByteArray;
  var
    penv : PJNIEnv;
  begin
    penv := JNIPointer;
    result := penv^.newByteArray(penv, High(arr) + 1);
    penv^.setByteArrayRegion(penv, result, 0, High(arr) +1, @arr);
  end;
    
  function createJCharArray (var arr : array of JChar) : JCharArray;
  var
    penv : PJNIEnv;
  begin
    penv := JNIPointer;
    result := penv^.newCharArray(penv, High(arr) + 1);
    penv^.setCharArrayRegion(penv, result, low(arr), High(arr) +1, @arr);
  end;
    
  function createJShortArray (var arr : array of JShort) : JShortArray;
  var
    penv : PJNIEnv;
  begin
    penv := JNIPointer;
    result := penv^.newShortArray(penv, High(arr) + 1);
    penv^.setShortArrayRegion(penv, result, 0, High(arr) +1, @arr);
  end;
    
  function createJIntArray (var arr : array of Jint) : JIntArray;
  var
    penv : PJNIEnv;
  begin
    penv := JNIPointer;
    result := penv^.newIntArray(penv, High(arr) + 1);
    penv^.setIntArrayRegion(penv, result, low(arr), High(arr) +1, @arr);
  end;
  
  function createJLongArray (var arr : array of JLong) : JLongArray;
  var
    penv : PJNIEnv;
  begin
    penv := JNIPointer;
    result := penv^.newLongArray(penv, High(arr) + 1);
    penv^.setLongArrayRegion(penv, result, low(arr), High(arr) +1, @arr);
  end;

  function createJFloatArray (var arr : array of JFloat) : JFloatArray;
  var
    penv : PJNIEnv;
  begin
    penv := JNIPointer;
    result := penv^.newFloatArray(penv, High(arr) + 1);
    penv^.setFloatArrayRegion(penv, result, low(arr), High(arr) +1, @arr);
  end;
      
  function createJDoubleArray (var arr : array of JDouble) : JDoubleArray;
  var
    penv : PJNIEnv;
  begin
    penv := JNIPointer;
    result := penv^.newDoubleArray(penv, High(arr) + 1);
    penv^.setDoubleArrayRegion(penv, result, 0, High(arr) +1, @arr);
  end;

end.

{$R+}