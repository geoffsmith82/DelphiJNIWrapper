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

uses SysUtils, Classes, JNI, JNIWrapper;



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
                                          ) : jobject ; stdcall;
  var
      S : String;
      JavaClass : TJavaClass;
      JavaObject : TJavaObject;
      Params : TJavaParams;
  begin
    // The following line is vital for using the JNI Wrapper code
    // in the J->D direction.
    TJavaVM.SetThreadPenv(penv);

    // Convert the message to a delphi string.
    S  := JToDString(message);
    
    JavaClass := TJavaClass.Create('NativeExample$PresizedClosableFrame');
    Params := TJavaParams.Create;
    Params.AddString('Greetings from Delphi');
    Params.AddInt(width);
    Params.AddInt(height);
    JavaObject := TJavaObject.Create(JavaClass, Params);
    MessageBox(0, PChar(S), 'Just a Windows Message Box', 0);

// Use the Object wrapper's handle as the return value.
    result := JavaObject.Handle;

    // Clean up 
    Params.Free;
    JavaClass.Free;
    JavaObject.Free;
  end;

exports Java_NativeExample_delphiFunc;

end.
