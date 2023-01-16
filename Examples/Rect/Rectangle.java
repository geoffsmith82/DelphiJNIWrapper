/**********************************************

       Java Class used in Demos of
          JNIWrapper for Delphi

       Copyright (c) 1998 Jonathan Revusky

       Java and Delphi Freelance programming
             jon@revusky.com

**********************************************/
package Rect;

import java.awt.*;
import java.awt.event.*;
import java.io.Console;
import java.lang.Object;
import java.util.ArrayList;



public class Rectangle {

private int length = 0;        
private  int breadth = 0;        

    public  void main (String[] args) {
        for (int i = 0; i<args.length; i++) System.out.println(args[i]);
        System.exit(1234);
    }

  
    public  Rectangle(int l,int b) {
        length = l;
        breadth = b;
      //System.exit(1234);
      }

     public  static Rectangle getRectangleObject(int a,int b) {
        Rectangle rect = new Rectangle(a,b);
        return rect;
    
      }
public int getLength() {
        
        return length;
    
      }

 public int  setRectangle(int a,int b) {
        
       length = a;
        breadth = b;
 return 1234;
    
      }
 

}
