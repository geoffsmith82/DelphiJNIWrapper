{*******************************************************
*                                    
*       JNI Wrapper for Delphi Demo 5                   
*       Demonstrates how to explicitly invoke a 1.2 level VM
*
*       Copyright (c) 1998 Jonathan Revusky
*
*       Java and Delphi Freelance programming
*             jon@revusky.com
*
*******************************************************}

program jtest5;

// example that shows how to instantiate an object
// and invoke a method on it using the JNI Wrapper

{$APPTYPE CONSOLE}

uses
{$IFDEF FPC}
  Windows,
  SysUtils,
  Classes,
  JavaRuntime,
  JNIWrapper;
{$ELSE}
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  JavaRuntime,
  JNIWrapper;
{$ENDIF}

var
  JFrame : TJavaClass;
  ShowMethod, SizeMethod : TJavaMethod;
  Params : TJavaParams;
  TestObject : TJavaObject;
  Runtime : TJavaRuntime;
begin
  try
    Runtime := TJavaRuntime.Create(SunJava2);
    JFrame := TJavaClass.Create('javax.swing.JFrame');
    Params := TJavaParams.Create;
    Params.AddString('This is a Java 1.2 Swing JFrame');
    TestObject := JFrame.Instantiate(Params);
    Params.Free;
    ShowMethod := TJavaMethod.Create(JFrame, 'show', nonstatic, Void, Nil, Nil);
    Params := TJavaParams.Create;
    Params.AddInt(500);
    Params.AddInt(300);
    SizeMethod := TJavaMethod.Create(JFrame, 'setSize', nonstatic, Void, Params, Nil);
    SizeMethod.Call(Params, TestObject);
    ShowMethod.Call(nil, TestObject);
    Runtime.Wait; // Wait until the JVM has exited all the //threads.
    Runtime.CallExit(0);
    JFrame.free;
    Params.free;
    Runtime.free;
  except
    on EJavaRuntimeNotFound do
        writeln('Could not load Sun 1.2 JVM');
    on EJvmException do
        ShowException(ExceptObject, ExceptAddr);
  end;
end.

