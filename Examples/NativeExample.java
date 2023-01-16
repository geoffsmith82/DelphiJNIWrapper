/**********************************************

       Java Class used to demonstrate
        calling Delphi code from Java
        see native.dpr for the other half.

       Copyright (c) 1998 Jonathan Revusky

       Java and Delphi Freelance programming
             jon@revusky.com

**********************************************/
import java.awt.Frame;
import java.awt.event.*;

public class NativeExample {

// Loads native.dll when the class is initialized.

    static {
        System.loadLibrary("native1");
    }

/** 
 * See native.dpr for the native implementation of this method.
 */

    public static native Frame delphiFunc(String greeting, int width, int height);

// The main method calls the native method and outputs
// its return value.

    public static void main(String args[]) {
       
delphiFunc("Say hi to Bill!", 500, 300);
System.out.println("Amine");
 System.exit(1234);
         }

  //  public static class PresizedClosableFrame extends Frame {
   //     public PresizedClosableFrame(String title, int width, int //height ) {
   //         super(title);
     //       setSize(width, height);
      //      addWindowListener(new WindowAdapter() {
  //                                public void windowClosing//(WindowEvent e) {
    //                                  System.exit(0);
      //                            }
       //                       });
       // }
   // }
}
