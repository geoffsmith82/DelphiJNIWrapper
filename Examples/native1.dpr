{******************************************************
*
*       Source for a DLL to demonstrate
*       how Delphi code can be invoked from Java
*       and create and return a Java object.
*
*       See NativeExample.java for the other half
*       of the demo source code
*
*       Copyright (c) 1998 Jonathan Revusky
*
*       Java and Delphi Freelance programming
*             jon@revusky.com
*
*******************************************************}

library native;

{$IFDEF FPC}
uses
  SysUtils,
  Classes,
  JNI,
  JNIWrapper;
{$ELSE}
uses
  System.SysUtils,
  System.Classes,
  JNI,
  JNIWrapper;
{$ENDIF}

// Just a Win32 API call.
  function MessageBox(hWnd: Integer;
                      lpText, lpCaption: PChar;
                      uType: Integer)
  : Integer; stdcall; external 'user32.dll' name 'MessageBoxA';

// Below is the native function referenced in NativeExample.java
// You can get the signature of the function by calling javah -jni ClassName.
// Unfortunately, you must then translate the function prototype from C
// to Pascal. That's not so hard though.


  function Java_NativeExample_delphiFunc (penv : PJNIEnv ;
                                          jc : jclass;
                                          message : jstring;
                                          width, height : jint
                                          ) : jstring ; stdcall;
var
  S : UTF8String;
  JavaClass : TJavaClass;
  JavaObject : TJavaObject;
  Params : TJavaParams;
begin
  TJavaVM.SetThreadPenv(penv);
  TJavaVM.SetSingleThreaded(False);

  // Convert the message to a delphi string.
  S  := JToDString(message);
  writeln(S);
  writeln(width);

  Result := message;
end;

exports Java_NativeExample_delphiFunc;

end.
