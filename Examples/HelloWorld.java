/**********************************************

       Java Class used in Demos of
          JNIWrapper for Delphi

       Copyright (c) 1998 Jonathan Revusky

       Java and Delphi Freelance programming
             jon@revusky.com

**********************************************/

import java.awt.*;
import java.awt.event.*;
import java.io.Console;
import java.lang.Object;

public class HelloWorld extends java.awt.Frame {
    public static void main (String[] args) {
        for (int i = 0; i<args.length; i++) System.out.println(args[i]);
        System.exit(1234);
    }

    public static int test(int i,long l,String s,long[] myList) {
       Console c = System.console();

    System.out.println(i);
        System.out.println(l);
        System.out.println(s);
        System.out.println("myList elements are: ");
for (int j = 0; j < myList.length; j++) {
         System.out.println(myList[j] + " ");
        }
 

//    c.readLine();
       
      // Get a Runtime object
      Runtime r = Runtime.getRuntime();
//long freeMem = r.freeMemory();
 //System.out.println(freeMem);
//myList=null;
//      r.gc();
//long freeMem1 = r.freeMemory();
// System.out.println(freeMem1);

//System.gc ();
//System.runFinalization ();
//c.readLine();
        return 1234;
    
    }

    public static int testArray(int[] args)
    {
        for (int i=0; i<args.length; i++)
        {
            System.out.println(args[i]);
        }
        return args[0];
    }

    public HelloWorld(String s, int width, int height) {
        super(s);
        add("Center", new Label("Hello, GUI World.", Label.CENTER));
        setSize(width, height);
        setVisible(true);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                HelloWorld.this.dispose();
                System.exit(23);
            }
        });
    }
}
