package pajama.compile;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import java.io.FileInputStream;
import java.io.InputStream;

public class Pajamac {

    public static void main(String[] args) throws Exception {
        String inputFile = null;
        if (args.length > 0) {
            inputFile = args[0];
        }
        InputStream is = System.in;
        if (inputFile != null) {
            is = new FileInputStream(inputFile);
            System.err.println("Reading from " + inputFile);
        } else {
            System.err.println("Reading from console (enter CTRL-Z+ENTER to finish");
        }

	ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(baos);
        PrintStream old = System.out;
        System.setOut(ps);

        ANTLRInputStream input = new ANTLRInputStream(is);
        PajamaLexer lexer = new PajamaLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        PajamaParser parser = new PajamaParser(tokens);
        ParseTree tree = parser.rules();

        System.err.println("Pajamac");
        Compiler comp = new Compiler();
        comp.compile(tree);
        comp.genCode();

 	System.out.flush();
        System.setOut(old);
        String str = baos.toString().replace("$", "_");        
        System.out.println(JsBeautify.jsBeautify(str, 2).replace("_", "$"));

    }
}
