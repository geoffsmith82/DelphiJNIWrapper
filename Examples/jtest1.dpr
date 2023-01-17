{*******************************************************}
{                                                       }
{       JNI Wrapper for Delphi Demo                     }
{       Demonstrates about the Simplest                 }
{       possible use of the JNI Wrapper.                }
{                                                       }
{       The main in HelloWorld.java simply spits        }
{       back the strings it receives. This wrapper      }
{       allows you to use TStringList to pass the       }
{       arguments. CallMain is a convenience method.    }
{                                                       }
{       Copyright (c) 1998 Jonathan Revusky             }
{                                                       }
{       Java and Delphi Freelance programming           }
{             jon@revusky.com                      }
{                                                       }
{*******************************************************}

program jtest1;

// example that shows to launch a class
// using the JavaRuntime unit.

{$APPTYPE CONSOLE}

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
  Strings : TStringList;
  Runtime : TJavaRuntime;

begin
  try
    Strings := TStringList.Create;
    Strings.Add('Hello, world.');
    Strings.Add('Salut, monde.');
    Strings.Add('Hola, mundo.');
    Runtime := TJavaRuntime.GetDefault;
    //Runtime := TJavaRuntime.Create(SunJava2);
    Runtime.CallMain('HelloWorld', Strings);
    Runtime.Wait;
    Strings.Free;
  except
    on EJvmException do
      ShowException(ExceptObject, ExceptAddr);
  end;
end.

