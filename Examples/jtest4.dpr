{*******************************************************}
{                                                       }
{       JNI Wrapper for Delphi Demo 4. A more           }
{       involved demonstration of the JNIWrapper        }
{       classes. A Frame and label are instantiated.    }
{       and displayed.                                  }
{                                                       }
{       Copyright (c) 1998 Jonathan Revusky             }
{                                                       }
{       Java and Delphi Freelance programming           }
{             jon@revusky.com                      }
{                                                       }
{*******************************************************}

program jtest4;

//rather involved example showing how to
//create java objects and script them from delphi
// (Hmm, now remind me why anyone would want to do this...)

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
  Params : TJavaParams;
  FrameClass, LabelClass, ComponentClass : TJavaClass;
  FrameObject, LabelObject : TJavaObject;
  setVisible, setSize, setLocation, add, setText : TJavaMethod;
  Runtime : TJavaRuntime;
  I: Integer;
begin
  try
    Runtime := TJavaRuntime.GetDefault;
     //Runtime := TJavaRuntime.Create(SunJava2);

    // Get references to the java classes we need.
    FrameClass := TJavaClass.Create('java.awt.Frame');
    LabelClass := TJavaClass.Create('java.awt.Label');
    ComponentClass := TJavaClass.Create('java.awt.Component');
      
    // Create the Frame.
    Params := TJavaParams.Create;
    Params.AddString('JNI Demo 4');
    FrameObject := FrameClass.Instantiate(params);
    Params.Free;
      
    // Create the Label.
    Params := TJavaParams.Create;
    Params.AddString('Just give me 10 seconds....');
    Params.AddInt(1); //The constant Label.CENTER is 1.
    LabelObject := LabelClass.Instantiate(params);
    Params.Free;
      
    // Add the Label to the Frame.
    Params := TJavaParams.Create;
    Params.AddString('South');
    Params.AddObject(LabelObject, ComponentClass);
    add := TJavaMethod.Create(FrameClass, 'add', nonstatic, aObject, Params, ComponentClass);
    add.Call(Params, FrameObject);
    Params.Free;
      
    // Call setSize() on the Frame.
    Params := TJavaParams.Create;
    Params.AddInt(200);
    Params.AddInt(100);
    setSize := TJavaMethod.Create(FrameClass, 'setSize', nonstatic, void, Params, nil);
    setSize.call(Params, FrameObject);
    Params.free;
      
    // Call setLocation() on the Frame.
    Params := TJavaParams.Create;
    Params.AddInt(300);
    Params.AddInt(200);
    setLocation := TJavaMethod.Create(FrameClass, 'setLocation', nonstatic, void, Params, nil);
    setLocation.call(Params, FrameObject);
    Params.free;
      
    // Call setVisible() on the Frame.
    Params := TJavaParams.Create;
    Params.AddBoolean(true);
    setVisible := TJavaMethod.Create(FrameClass, 'setVisible', nonstatic, void, Params, nil);
    setVisible.call(Params, FrameObject);
    Params.free;
      
    // Call setText() on the Label
    Params := TJavaParams.Create;
    Params.AddString('Will count down from 10');
    setText := TJavaMethod.Create(LabelClass, 'setText', nonstatic, void, Params, nil);
    setText.call(Params, LabelObject);
      
    for I:=9 downto 0 do
    begin
    // Pause for one second (approximately)
      Sleep(1000);
      Params := TJavaParams.Create;
      Params.AddString(IntToStr(I) + ' seconds left');
      setText.call(Params, LabelObject);
      Params.free;
    end;
      
    //Free the objects that were defined.
    add.Free;
    setSize.Free;
    setLocation.Free;
    setText.Free;
    setVisible.Free;
    FrameObject.Free;
    LabelObject.Free;
    FrameClass.Free;
    LabelClass.Free;
    ComponentClass.Free;
      
    // Convenience method. Calls System.exit()
    Runtime.CallExit(1234);
  
          // Exception handler
  except
    on EJvmException do
      ShowException(ExceptObject, ExceptAddr);
  end;
end.

