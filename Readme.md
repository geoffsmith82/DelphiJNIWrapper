# JNI Wrapper for Delphi and FreePascal

    Contents of Archive
    Notes on Use
    Licensing Terms 

Version: 2.94

Date:

April 22, 2019

Authors:

Jonathan Revusky (Lead programmer), jon@revusky.com

Salvatore Meschini (Maintainer), salvatoremeschini@interfree.it
          
Amine Moulay Ramdane has enhanced more JNI Wrapper and ported it to 64 bit and to all the Delphi versions and to Delphi Tokyo and to FreePascal and i have made it work with Java, my email is aminer68@gmail.com

This JNI Wrapper for Delphi and FreePascal provides a powerful and simplified object-oriented API for doing mixed language programming in Java and Delphi (Object Pascal language) or FreePascal. This may provide an easier and more productive way of getting Win32 and Win64 features in Java projects and integrating legacy code (at least for the Delphi or FreePascal community). Please read the readme file inside the zip file to learn more.

I Amine Moulay Ramdane have noticed that JNIWrapper for Delphi and FreePascal was not supporting returning of Strings and Arrays types from Java, so i have implemented that and i have now enhanced JNI Wrapper to be very powerful , so it's now supporting all the necessary functions and methods and and much more... hope you will happy with it cause i have worked hard to bring this new 2.94 to you, it is really now a professional software of a good quality. 

Also i Amine Moulay Ramdane have enhanced the JNI Wrapper more and ported it to 64 bit and to both FreePascal and the Delphi XE versions sanmd to Delphi Tokyo, here is the functions that i have implemented and added:
```Pascal
function JstringArrayToDTStrings(jarr : JArray) : TStrings;

function JdoubleArrayToDdoubleArray(jarr : JdoubleArray) : TDdoubleArray;

function JfloatArrayToDsingleArray(jarr : JFloatArray) : TDsingleArray;

function JcharArrayToDwordArray(jarr : JCharArray) : TDwordArray;

function JbyteArrayToDshortintArray(jarr : JByteArray) : TDshortintArray;

function JshortArrayToDsmallintArray(jarr : JShortArray) : TDsmallintArray;

function JbooleanArrayToDbooleanArray(jarr : JBooleanArray) : TDbooleanArray; 
```
And don't forget to call TJavaVM.freeRef() method from Delphi or FreePascal when you need to garbage collect and free the memory that was allocated. 

Read the rest:

## Design Goal:

To provide a simplified object-oriented API for doing mixed language programming in Java and Delphi (Borland Object Pascal language) or FreePascal. This may provide an easier and more productive way of getting Win32 and Win64 features in Java projects and integrating legacy code (at least for the Delphi or FreePascal community).
General Description:

The accompanying files contain source code that makes it easy to implement mixed language projects with Java and Delphi or FreePascal. JNI.pas is a straight translation of the jni.h header file distributed with Sun's JDK. This can be used on its own with no restriction except those applying to the use of the Sun JDK (not to be used flying aircraft or operating nuclear facilities and whatnot.)

The files JNIWrapper.pas and JavaRuntime.pas contain the source code to wrapper classes I have written to provide a much easier object-oriented interface to the Java Native Interface (JNI). These classes include the following:

    * TJavaRuntime is an encapsulation of the JNI Invocation API. In most cases, you will get an instance of this class by calling the class method GetDefault.
    * TJavaParams is an encapsulation of the list of parameters passed to a Java method.
    * TJavaObject is an encapsulation of a Java object.
    * TJavaClass is an encapsulation of a Java class.
    * TJavaMethod is an encapsulation of a Java method.

Use of the above classes is demonstrated in the various .dpr and .java files that are included.
Notes on Use of the JNI wrapper methods:

Usage should be fairly intuitive, particularly from study of the included examples.

In this version, there is a pretty sophisticated wrapper around the invocation API. Typically, you instantiate a TJavaRuntime instance by calling the static method GetDefault, which will look for a Java runtime on your system. By default, it starts off looking for a Oracle Java JVM, and failing that, looks for a Sun 1.1 VM, and if it can't find that, looks for the MS VM. (Note that the latter will only work with the most recent JVM from Microsoft, the one that supports JNI.) You can give it hints by calling the methods SetPrefersMS and SetJava11 before calling GetDefault. If you do that, you change the order in which the method searches.

You can also specify directly which VM you want to instantiate by calling:

TJavaRuntime.Create(<choice>) where the choice can be one of:
SunJava11, SunJava2, or MSJava.

This method will raise an exception if:

    * You have already instantiated a JavaRuntime instance (you can only create one.)

    * The JVM you specified cannot be found on the system.

The various run-time JVM options can be set via the properties MaxHeapSize, MinHeapSize, Verbose, etc. of the class TJavaRuntime. Note that these properties are effectively read-only after the VM is actually instantiated.
Notes on calling Delphi or FreePascal methods from Java:

An example is provided of calling a Delphi or FreePascal DLL from Java. Note that in order to use the JNI wrapper methods, you must first call:
TJavaVM.setThreadPenv(penv);

at the top of your native method.

The other big gotcha in terms of calling a native Delphi or FreePascal DLL from Java involves threading issues. Delphi or FreePascal variables that are to be used in multiple threads should be declared in a threadvar section. Java objects wrappers that are to be used from multiple threads should be promoted to global references. This amounts to setting:
JavaObject.Global := true;

with a TJavaObject (or TJavaClass) instance.
Direct Use of JNI.pas:

I anticipate that most people will prefer to call Java methods using the wrapper classes in JNIWrapper.pas and JavaRuntime.pas. However, not all of the JNI functionality is currently wrapped, so you may have to use JNI.pas directly. If you do that, the source in JNIWrapper.pas provides a fairly involved model of how to do so. Note that there is significant redundancy in the function pointers which are provided by the JNI. Most calls have three forms of parameter list that you can use, CallXXXMethod(), CallXXXMethodV(), and CallXXXMethodA(). The first form uses a C-style variable length parameter list. As far as I know, this has no Object Pascal equivalent. I have mostly opted to use the CallXXXMethodV calls, which basically work by passing a totally raw pointer to the list of arguments.

That is indeed about as ugly as it gets. However, the TJavaParams wrapper class is designed to shelter you from this ugliness. This class (in conjunction with TJavaMethod) also protects you from having to supply the Java method signature to obtain a reference to a Java method.
Contents of archive:

    * JNI.pas -- translation of the jni.h file in the Sun JDK to Pascal.

    * JNIWrapper.pas -- delphi source code containing object pascal class wrappers for JNI handles.

    * JavaRuntime.pas -- delphi source code containing object pascal class TJavaRuntime that provides an easy-to-use wrapper around the JNI invocation API.

    * JUtils.pas -- Source of various routines used here and there.

    * JTest*.dpr -- These files contain delphi source of demos that show how to use the JNIWrapper.pas code in your programs.

    * HelloWorld.java -- the source to a Java program that is used in JTest demos.

    * native.dpr -- Source for a trivial DLL written in Delphi to demonstrate a native call to delphi from Java. The Java part of this demo is in the file NativeExample.java

    * Bothways.dpr -- This provides an example in which Delphi code calls Java code and then viceversa!

    * build.bat -- A command-line batch file which compiles all the included source files. It is assumed that you have the Delphi command line compiler DCC32.exe and the default Java command-line compiler javac.exe on your system path. Note: This code compiles with Delphi 4. I haven't tried it with any later version. Please tell me if there is a problem.

Partial wish list:

    * A delphi version of the javah tool that provides the stubs for java native methods.

    * Better documentation.

(Write me to add to this wishlist.)
## License:

Open source. The library may be incorporated into your products as long as you specify in your banner (console mode) and/or about box (GUI mode) that you make use of this product and provide the URL where the user may obtain the latest version. That URL is currently:

http://www.revusky.com/

Redistribution of this product in source form must include this readme file.

Have fun!

Jonathan Revusky, 1 May 2001
