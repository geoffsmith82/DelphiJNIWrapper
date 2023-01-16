
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
   d1,d3:TDbooleanArray ;
    d2:JbooleanArray;
  i:integer;
str:TDbooleanArray ;
Runtime : TJavaRuntime;
    begin
 Runtime := TJavaRuntime.GetDefault;  

setlength(d1,20);
	
for i:=0 to 20-1
do 
begin
if (i mod 2) = 0 
then d1[i]:=true
else d1[i]:=false;
end;
d2:=createJBooleanArray(d1);
str:=JbooleanArrayToDbooleanArray(d2);

for i:=0 to high(str)
do 
if str[i]=true 
then writeln(i,': true')
else writeln(i,': false');


setlength(d1,0);
setlength(str,0);

runtime.callexit(0);
Runtime.free;
end.

