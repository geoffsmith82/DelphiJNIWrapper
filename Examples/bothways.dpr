{*******************************************************}
{                                                       }
{       JNI Wrapper for Delphi Demo                     }
{       This example demonstrates a Delphi program      }
{       that invokes a Java program through             }
{       the JNI Wrapper and the java program in         }
{       turn invokes a DLL written in Delphi which       }
{       in turn calls Java code!                        }
{       (See native.dpr and NativeExample.java          }
{                                                       }
{       Copyright (c) 1998 Jonathan Revusky             }
{                                                       }
{       Java and Delphi Freelance programming           }
{             jon@revusky.com                      }
{                                                       }
{*******************************************************}

program bothways;

// This demonstrates an .exe that invokes a java class
// which in turns invokes delphi code in a dll.
// See NativeExample.java and Native.dpr

uses

{$IFDEF FPC}
  SysUtils,
  Classes,
  JavaRuntime;
{$ELSE}
  System.SysUtils,
  System.Classes,
  JavaRuntime;
{$ENDIF}

var
  Runtime : TJavaRuntime;
begin
  try
    //    Runtime := TJavaRuntime.Create(MSJava); // If you used this line, you would create a MS JVM.

    Runtime := TJavaRuntime.Create(SunJava2);
    // We can pass a Nil as the String list and this
    // gets massaged into a java String[] array with zero elements.
    Runtime.CallMain('NativeExample', Nil);
    Runtime.Wait;
  except
    on Exception do
      ShowException(ExceptObject, ExceptAddr);
  end;
end.

