{*******************************************************}
{                                                       }
{       JNI Wrapper for Delphi Demo 2                   }
{       Demonstrates the use of the TJavaParams         }
{       class to pass parameters to a static            }
{       method.  The test method simply echoes          }
{       the parameters to the console.                  }
{                                                       }
{       Copyright (c) 1998 Jonathan Revusky             }
{                                                       }
{       Java and Delphi Freelance programming           }
{             jon@revusky.com                      }
{                                                       }
{*******************************************************}

program jtest2;

// Example that shows how to call 
// a static method using the JNIWrapper unit.

{$APPTYPE CONSOLE}

uses
{$IFDEF FPC}
  Windows,
  SysUtils,
  Classes,
  JNIWrapper,
  JNI,Javaruntime;
{$ELSE}
  winapi.Windows,
  System.SysUtils,
  System.Classes,
  JNIWrapper,
  JNI,
  JavaRuntime;
{$ENDIF}

var
  JNITest : TJavaClass;
  Params : TJavaParams;
  JMethod : TJavaMethod;
  Runtime : TJavaRuntime;
  arr1:array of jlong;
  i:integer;
begin
  SetLength(arr1,4);
 
  for i:=0 to high(arr1) do
    arr1[i]:=i;

  Runtime := TJavaRuntime.GetDefault;
  //Runtime := TJavaRuntime.Create(SunJava2);

  try
    Params := TJavaParams.Create;
    Params.addInt(13);
    Params.addLong(11);
    Params.addString('A Brave New World');
    params.addLongArray(arr1);

    JNITest := TJavaClass.Create('HelloWorld');
             
    JMethod := TJavaMethod.Create(JNITest, 'test', static, AInt, params, Nil);
          
    JMethod.Call(Params, nil);

    Params.Free;
    JMethod.Free;
    JNITest.Free;
   
    SetLength(arr1, 0);
  except
    on EJvmException do
      ShowException(ExceptObject, ExceptAddr);
  end;
  runtime.callexit(0);
  runtime.free;
end.

