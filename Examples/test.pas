
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
   d1,d3:TDstringArray;
    d2:jArray;
  i:integer;
str1:TStrings;
str:TStrings;
Runtime : TJavaRuntime;

    begin
Runtime := TJavaRuntime.GetDefault;  
	str1:=TStringlist.create;
for i:=0 to 20-1
do str1.add(inttostr(i));
d2:=createJstringArray(str1);
str:=JstringArrayToDTStrings(d2);

for i:=0 to str.count-1
do 
writeln(str[i]);


str.free;
str1.free;
Runtime.callexit(0);
runtime.free;

end.

