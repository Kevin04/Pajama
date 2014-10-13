package pajama.js;
import java.util.*;
import java.io.*;
public class JSApply implements JSAst{
   
   private JSAst f;
   private List<JSAst> args;
   private boolean flag = false;
   
   public JSApply(JSAst f, JSAst e){
      this(f, Arrays.asList(e));
   }
   public JSApply(JSAst f, List<JSAst> args){
      this.f = f;
      this.args = args;
   }
   public JSApply(JSAst f, JSAst e, boolean b){
      this(f, Arrays.asList(e),b);
   }
   public JSApply(JSAst f, List<JSAst> args, boolean b){
      this.f = f;
      this.args = args;
	  this.flag = b;
   }
   @Override
   public void genCode(PrintStream out){
      out.print("(");
	  f.genCode(out);
	  out.print(")");
	  out.print("(");
	  genCode(out, args);//Creo que este es el problema
	  out.print(")");
	  if(flag) out.print(";");
	  //(nomFuncion)(arg1,arg2,arg3)
   }
}
