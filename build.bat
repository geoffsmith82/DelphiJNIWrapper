@echo Compiling JNI wrapper code
cd src
dcc32 JNI.pas JavaRuntime.pas JNIWrapper.pas JUtils.pas
cd ..\examples
@echo Compiling native examples
dcc32 jtest1.dpr jtest2.dpr jtest3.dpr jtest4.dpr jtest5.dpr native1.dpr bothways.dpr 
@echo Compiling java code examples...
javac HelloWorld.java NativeExample.java 
cd ..
