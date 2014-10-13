package pajama.js;
import java.util.*;
import java.io.PrintStream;

public class JSAnd implements JSAst{
  
   private JSAst l, r;
   public JSAnd(JSAst l, JSAst r){
      this.l = l;
	  this.r = r;
   }
   @Override
   public void genCode(PrintStream out){
	 this.l.genCode(out); 
	 out.format("&&"); 
	 this.r.genCode(out); 
   }
} 
