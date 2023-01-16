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
        system.SysUtils,
        system.Classes,
        JNIWrapper,
       JNI,Javaruntime;
{$ENDIF}

       
    var
        JNITest,JNITest1: TJavaClass;
        Params ,params1,params2,params3: TJavaParams;
        JMethod ,jMethod1: TJavaMethod;
        Runtime : TJavaRuntime;
       i:integer; 
         ret,ret1,ret2:jvalue;   
   d2:TDdoubleArray;
TestObject,TestObject1 : TJavaObject;


begin


Runtime := TJavaRuntime.GetDefault;  
//Runtime := TJavaRuntime.Create(SunJava2); 

       try

        
      JNITest := TJavaClass.Create('Hello');
      JNITest1 := TJavaClass.Create('Rect.Rectangle');
       


Params2 := TJavaParams.Create;
Params2.addInt(80);
    TestObject := JNITest.Instantiate(params2);
Params2.free;
 Params3 := TJavaParams.Create;
             Params3.addInt(1000);
             Params3.addInt(81);
        TestObject1 := JNITest1.Instantiate(params3);
     Params3.free;
     
Params1 := TJavaParams.Create;
             Params1.addInt(80);

JMethod := TJavaMethod.Create(JNITest, 'test', nonstatic, Aobject, params1, JNITest1);

ret1:=JMethod.Call(params1,TestObject);
if ret1.z
then writeln('True')
else  writeln('False');


            Params := TJavaParams.Create;
                      Params.addObject(TJavaObject(TestObject1),TJavaClass(JNITest1));

JMethod1 := TJavaMethod.Create(JNITest, 'test2', nonstatic, Aint, params,  JNITest);


ret:=JMethod1.Call(Params,TestObject);
writeln('Return value from test2 method is: ',ret.i);

            
   
JNITest.free;
JNITest1.free;
TestObject.free;
TestObject1.free;
Params.free;
   

              
        except
         on EJvmException do 
             ShowException(ExceptObject, ExceptAddr);
        end;
runtime.callexit(0);
runtime.free;
end.

