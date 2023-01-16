{*******************************************************}
{                                                       }
{       JNI Wrapper for Delphi Demo 3                   }
{       Demonstrates the use of the JNIWrapper          }
{       classes to dynamically create an instance       }
{       of an object.                                   }
{                                                       }
{       Copyright (c) 1998 Jonathan Revusky             }
{                                                       }
{       Java and Delphi Freelance programming           }
{             jon@revusky.com                      }
{                                                       }
{*******************************************************}

program jtest3;

// example that shows how to instantiate an object
// and invoke a method on it using the JNI Wrapper

{$APPTYPE CONSOLE}

    
    uses
{$IFDEF FPC}
       Windows, 
        SysUtils,
        Classes,
        JNIWrapper,
        JNI,
        JavaRuntime;

{$ELSE}
    winapi.Windows, 
        system.SysUtils,
        system.Classes,
        JNIWrapper,
       JNI,
       JavaRuntime;
{$ENDIF}

    var
        JNITest : TJavaClass;
        Params : TJavaParams;
        TestObject : TJavaObject;
        Runtime : TJavaRuntime;
       
    begin

        try
        //  Runtime := TJavaRuntime.Create(SunJava1); //use this line if you want Java 1.1 specifically.
        //  Runtime := TJavaRuntime.Create(MSJava); //or this line if you want MS specifically
        
            Runtime := TJavaRuntime.GetDefault;
            // Runtime := TJavaRuntime.Create(SunJava2); 

            JNITest := TJavaClass.Create('HelloWorld');
            Params := TJavaParams.Create;
           
             Params.AddString('JNI Test');
            Params.AddInt(200);
            Params.AddInt(200);
             
            TestObject := JNITest.Instantiate(params);
            Runtime.Wait; // Wait until the JVM has exited all the threads.
            
            // cleanup now just to be compulsive.
            Params.Free;
            TestObject.Free;
            JNITest.Free;
        except
            on EJvmException do 
                ShowException(ExceptObject, ExceptAddr);
        end;
runtime.callexit(0);
runtime.free;

end.

