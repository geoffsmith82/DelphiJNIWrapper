

program test;

// example that shows to launch a class
// using the JavaRuntime unit.

{$APPTYPE CONSOLE}

    uses
{$IFDEF FPC}
        sysutils, Classes,jniwrapper,jni,javaruntime;
{$ELSE}
    system.SysUtils, system.Classes,jniwrapper,jni,javaruntime;
{$ENDIF}

{$IFDEF FPC}
type PUTF8char = PAnsiChar;
{$endif}


    var
       
    a:PUTF8char;
   d1,d3:TDsingleArray;
    d2:jfloatArray;
  i:integer;
str:TDsingleArray;
Runtime : TJavaRuntime;
    begin
Runtime := TJavaRuntime.GetDefault;  

setlength(d1,20);
for i:=0 to 20-1
do d1[i]:=i;
d2:=createJFloatArray(d1);
str:=JfloatArrayToDsingleArray(d2);

for i:=0 to high(str)
do 
writeln(str[i]);


setlength(d1,0);
setlength(str,0);
Runtime.callexit(0);
runtime.free;
end.

