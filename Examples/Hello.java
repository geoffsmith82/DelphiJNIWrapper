
import Rect.Rectangle;



public class Hello extends java.awt.Frame {
public Rect.Rectangle rect2;
   

public  Rectangle test(int a) {
   
 
Rect.Rectangle rect1 = new Rect.Rectangle(90,91);
rect1.setRectangle(100,101);
System.out.println(rect1.getLength());
rect2 = rect1;
return (Rect.Rectangle)rect2;
    }
      

public  int test2(Rect.Rectangle a1) {


System.out.println("Length of the rectangle is: ");

System.out.println(a1.getLength());

return 1234;
   }

 public static void main (String[] args) {
        for (int i = 0; i<args.length; i++) System.out.println(args[i]);
        System.exit(1234);
    }

    public  int testArray(int[] args)
    {
        for (int i=0; i<args.length; i++)
        {
            System.out.println(args[i]);
        }
        return args[0];
    }



    public Hello(int a) {
        
                
        }
}
