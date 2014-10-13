package pajama.compile;

import pajama.js.*;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import java.util.*;
import java.util.stream.*;
import java.util.Map;
import java.util.Hashtable;

class DepthOffset {

    public Integer d;
    public Integer o;

    public DepthOffset(Integer d, Integer o) {
        this.d = d;
        this.o = o;
    }
}

class SymbolEntry {

    JSId x;
    JSAccess s;
    JSNum offset;

    public SymbolEntry(JSId x, JSNum offset, JSAccess s) { 
        this.x = x;
        this.s = s;
        this.offset = offset;
    }

    public JSAccess getAccess() {
        return this.s;
    }

    public JSNum getOffset() {
        return this.offset;
    }

    public void update() {
        this.offset = new JSNum(this.offset.getValue() + 1);
    }

}

public class Compiler extends PajamaBaseVisitor<JSAst> implements Emiter {

    List<JSAst> rules = new ArrayList<>();
	List<JSAst> tests = new ArrayList<>();
    Map<String, SymbolEntry> symbolTable;
    Stack<JSAst> stack = new Stack<>();
    int offset = 0;
	JSId ruleName;

	public void push(JSAccess a){this.stack.push(a);}
	public void push(int a){this.stack.push(NUM(a));}
	public JSAst pop(){return this.stack.pop();}
		
	public JSAst locatePatternID(JSId x){
		System.err.println("locatePatternID: "+x.getValue()+" "+stack+" "+this.offset);
		if(this.offset<0){
			setOnTopLevel(x);
			return locateOnTopLevel();
		}
		locate(x);
		JSNum off = NUM(this.offset);
		JSAccess a = ACCESS(x,off);
		return a;
	}
	
	public JSAst locateExprID(JSId x){
		System.err.println("locateExprID: "+x.getValue());
		SymbolEntry entry = symbolTable.get(x.getValue());
        if (entry != null) {
			
			System.err.println("--NON NULL ENTRY");
			if(entry.getAccess().equals(TOP_ACCESS))
				return entry.getAccess();
            return entry.getAccess().setId(X);
        }
		System.err.println("NULL ENTRY RETURNING ID");
        return x;
		
	}
	
    public JSAst locate(JSId x) {
        System.err.println("locate: "+x.getValue()+" "+stack+" "+this.offset);
		if (this.offset < 0)return x
        List<JSAst> rstack = new ArrayList<>();
		
		
		for(JSAst k:stack){ 
			rstack.add(k);
		}
		
        JSAst a = x;//Guardo el ID en JSast.
        JSNum off = NUM(this.offset);//El offset en la lista actual para accesarla.
		
        for (JSAst k : rstack) {
			if(k instanceof JSAccess){
				System.err.println("SI ERA JSACCESS");
				JSAccess na = (JSAccess)k;
				a = na.setLeft(a);
			}
			else {
				 System.err.println("NO ERA UN JSACCESS");
				a = ACCESS(a,k);
			}
        }
		if(!(a instanceof JSOAccess))
			a = ACCESS(a, off);
        SymbolEntry e = new SymbolEntry(x, off, (JSAccess) a);
        symbolTable.put(x.getValue(), e);
        return a;
    }
	
	
	public JSAst setOnTopLevel(JSId x){
		System.err.println("setOnTopLevel: "+this.ruleName.getValue());
		JSAccess a = TOP();
		SymbolEntry e = new SymbolEntry(x,NULL_OFFSET,a);
		symbolTable.put(x.getValue(),e);
		return a;
	}
	
	public JSAst locateOnTopLevel(){
		System.err.println("locateOnTopLevel: "+this.ruleName.getValue());
		SymbolEntry e = symbolTable.get(this.ruleName.getValue());
		if(e!=null)return e.getAccess();
		return setOnTopLevel(this.ruleName);
	}

	public SymbolEntry resetAccess(JSId x, JSAccess a){
		System.err.println("resetAccess: "+x.getValue());
		SymbolEntry e = new SymbolEntry(x, NULL_OFFSET, a);
		symbolTable.put(x.getValue(), e);
		return e;
	}
    public String PATH = "rt/util.js";

    public void genCode() {
        LOAD(PATH).genCode();
        rules.stream().forEach((t) -> t.genCode());
		tests.stream().forEach((s) -> s.genCode());
    }

    public JSAst compile(ParseTree tree) {
		System.err.println("******COMPILING*******");
        return visit(tree);
    }

    @Override
    public JSAst visitRuleStatement(PajamaParser.RuleStatementContext ctx) {
		System.err.println("visitRuleStatement");
        JSId id = ID(ctx.ID().getText());
		ruleName = id;
        JSAst formal = visit(ctx.formal());
        JSAst body = visit(ctx.ruleBody());
        JSAst frule = FUNCTION(id, formal, RET(APP(body, formal)));
        rules.add(frule);
        return frule;
    }

	@Override
	public JSAst visitTestStatement (PajamaParser.TestStatementContext ctx){
		System.err.println("visitTestStatement");
		JSId id = ID(ctx.ID().getText());
		List<JSAst> listArgs = ctx.args().expr()
		            .stream()
		            .map((o) -> visit(o))
		            .collect(Collectors.toList());	
		JSAst ftest = APP(id,listArgs, true);
		tests.add(ftest);
		return ftest;
	}

    @Override
    public JSAst visitFormal(PajamaParser.FormalContext ctx) {
		System.err.println("visitFormal "+ctx.ID().getText());
        return ID(ctx.ID().getText());
    }

    @Override
    public JSAst visitRuleBody(PajamaParser.RuleBodyContext ctx) {
		System.err.println("visitRuleBody ");
        List<JSAst> fcases = ctx.caseRule().stream()
                .map((c) -> visit(c))
                .collect(Collectors.toList());
        Collections.reverse(fcases);

        return fcases.stream()
                .reduce(FAIL, (z, fc)
                        -> FUNCTION(FORMALS(X), RET(APP(fc, ARGS(X, z)))));

    }

    @Override 
    public JSAst visitCaseRule(PajamaParser.CaseRuleContext ctx) {
		System.err.println("---------START CASE RULE-----------");
		System.err.println("visitCaseRule");
        symbolTable = new Hashtable<String, SymbolEntry>();
        stack = new Stack<JSAst>();
        this.offset = -1;

        JSAst p = visit(ctx.pattern());
        JSAst e = APP(visit(ctx.expr()),N);
        // function(n, c)if(p(n)) return e; else return c(n);
		System.err.println("---------FINISH CASE RULE-----------");
        return FUNCTION(FORMALS(N, C),
                IF(APP(p, N), RET(e), RET(APP(C, N))));
    }

    @Override
    public JSAst visitPatNum(PajamaParser.PatNumContext ctx) {
		System.err.println("visitPatNum"+ctx.NUMBER().getText());
        JSAst n = NUM(Integer.valueOf(ctx.NUMBER().getText()));
        return FUNCTION(FORMALS(X), RET(EQ(locatePatternID(X), n))); //function(x)x===n;
    }

    @Override
    public JSAst visitExprString(PajamaParser.ExprStringContext ctx) {
		System.err.println("visitExprString");
        return STRING(ctx.STRING().getText());
    }

    @Override
    public JSAst visitPattArray(PajamaParser.PattArrayContext ctx) {
        System.err.println("VisitPattArray");
        return visit(ctx.pattListOrEmpty());
    }

    @Override
    public JSAst visitPattListOrEmpty(PajamaParser.PattListOrEmptyContext ctx) {
        System.err.println("VisitPattListOrEmpty");
        if (ctx.pattList() == null) {
            return visit(ctx.pattEmpty());
        }
        return visit(ctx.pattList());
    }

    @Override
    public JSAst visitPattEmpty(PajamaParser.PattEmptyContext ctx) {
        System.err.println("VisitPattEmpty");
        return EMPTY_PREDICATE();
    }

    @Override
    public JSAst visitPattList(PajamaParser.PattListContext ctx) {
        System.err.println("VisitPattList con offset "+Integer.toString(this.offset));
        int lastOffset = this.offset;
        if (this.offset > 0) {
            this.push(this.offset);
        }
        this.offset = 0;
        List<JSAst> args = new ArrayList<JSAst>();
        ctx.pattern()
                .stream()
                .forEach((p) -> {
                    JSAst vp = visit(p);
                    if (vp != null) {
                        args.add(vp);
                    }
                    this.offset++;
                });
        int restOffset = this.offset;
		if(!stack.empty() && ( (this.stack.peek() instanceof JSNum) ) ){//Si el de mas arriba es num
			this.offset = ( ((JSNum)this.pop()).getValue());
			System.err.println("pattList: tengo un numero en el stack");
			}
		else
			this.offset = lastOffset;
		System.err.println("--VisitPattList: creating predFirstPart");

		JSAst predicateFirstPart;
		if(ctx.pattRestArray()!=null)
			predicateFirstPart = APP(PATLIST,ARGS(ARRAY(args),SLICE(locate(X),NUM(0),NUM(restOffset))));
		else
			predicateFirstPart = APP(PATLIST,ARGS(ARRAY(args),locate(X)));
		
		JSAst predicateRestPart, predicateComplete;
		if(ctx.pattRestArray()!=null){
			JSAccess slice = SLICE(locate(X),NUM(restOffset));
			this.push(slice);
			System.err.println("PRESTARRAY: LASTOFFSET: "+Integer.toString(lastOffset)+" OFFSET: "+Integer.toString(this.offset));
			lastOffset = this.offset;
			this.offset = 0;
			System.err.println("--VisitPattList: creating predicateRestPart");
			predicateRestPart=visit(ctx.pattRestArray());
			this.offset = lastOffset;
			this.stack.pop();
			
			predicateComplete = AND(predicateFirstPart,APP(predicateRestPart,X));
			
			resetAccess(X,slice);
		}
		else predicateComplete=predicateFirstPart;
		return FUNCTION(FORMALS(X), RET(predicateComplete));
	}

    @Override
    public JSAst visitPArray(PajamaParser.PArrayContext ctx) {
        System.err.println("visitPArray");
        return visit(ctx.pattArray());
    }

    @Override
    public JSAst visitPId(PajamaParser.PIdContext ctx) {
		System.err.println("visitPId");
        JSId id = ID(ctx.ID().getText());
		
		
		this.locatePatternID(id);
        return ANY;
    }

	
	
	@Override
	public JSAst visitPattRestID(PajamaParser.PattRestIDContext ctx) {
		System.err.println("visitPattRestID");
		JSId id = ID(ctx.ID().getText());
		System.err.println("Visita ID: "+ctx.ID().getText());
        locate(id);
        return ANY;
    }

	@Override
    public JSAst visitPAny(PajamaParser.PAnyContext ctx) {
		System.err.println("visitPAny");
		return ANY;
    }

	@Override
	public JSAst visitPattAny(PajamaParser.PattAnyContext ctx) {
		System.err.println("visitPattAny");
        return FUNCTION(FORMALS(X), RET(TRUE));
    }
    //------------------------------------------------------------

    @Override
    public JSAst visitIdSingle(PajamaParser.IdSingleContext ctx) {
		System.err.println("visitIdSingle "+ctx.ID().getText());
        String value = ctx.ID().getText();
		System.err.println("--idvalue="+value);
        JSId id = ID(value);
		
		return locateExprID(id);
    }

    @Override
    public JSAst visitArithOperation(PajamaParser.ArithOperationContext ctx) {
		System.err.println("visitArithOperation");
        List<JSId> opers = ctx.operAddPlus()
                .stream()
                .map((o) -> (JSId) visit(o))
                .collect(Collectors.toList());
        List<JSAst> monoms = ctx.arithMonom()
                .stream()
                .map((m) -> visit(m))
                .collect(Collectors.toList());
        JSAst a = monoms.get(0);
        JSAst point = monoms.stream()
                .skip(1)
                .reduce(POINT(0, a), (z, m) -> {
                    JSPoint p = (JSPoint) z;
                    int k = p.index;
                    return POINT(p.add(1).index, OPERATION(opers.get(k), p.y, m));
                });
        return ((JSPoint) point).y;
    }

	@Override
	public JSAst visitArithMonom(PajamaParser.ArithMonomContext ctx){
		System.err.println("visitArithMonom");
		List<JSId> opers = ctx.operTimesDiv()
                .stream()
                .map((o) -> (JSId) visit(o))
                .collect(Collectors.toList());
        List<JSAst> singles = ctx.arithSingle()
                .stream()
                .map((m) -> visit(m))
                .collect(Collectors.toList());
        JSAst a = singles.get(0);
        JSAst point = singles.stream()
                .skip(1)
                .reduce(POINT(0, a), (z, m) -> {
                    JSPoint p = (JSPoint) z;
                    int k = p.index;
                    return POINT(p.add(1).index, OPERATION(opers.get(k), p.y, m));
                });
        return ((JSPoint) point).y;
	}
    @Override
    public JSAst visitOperAddPlus(PajamaParser.OperAddPlusContext ctx) {
	System.err.println("visitOperAddPlus");
        return ID(ctx.op.getText());
    }
	@Override
    public JSAst visitOperTimesDiv(PajamaParser.OperTimesDivContext ctx) {
		System.err.println("visitOperTimesDiv");
        return ID(ctx.op.getText());
    }

    @Override
    public JSAst visitFunCallExpr(PajamaParser.FunCallExprContext ctx) {
		System.err.println("visitFunCallExpr");
		JSAst nom = visit(ctx.arithSingle());
		//JSID nomFunc = ctx.arithSingle().idSingle().getText();
		List<JSAst> listArgs;
		if(ctx.params() != null) {
			listArgs = ctx.params().args().expr()
		            .stream()
		            .map((o) -> (JSAst) APP(visit(o),X))
		            .collect(Collectors.toList());
		}
		else {
			listArgs = ctx.args().expr()
		            .stream()
		            .map((o) -> (JSAst) APP(visit(o),X))
		            .collect(Collectors.toList());	
					System.err.println("Visit R!!");
		}
		if(listArgs.size()>1) return APP(nom,ARRAY(listArgs));
		return APP(nom,listArgs);
    }
	
	 @Override
    public JSAst visitRelOperation(PajamaParser.RelOperationContext ctx) {
	System.err.println("visitRelOperation");
        List<JSId> opers = ctx.relOperator()
                .stream()
                .map((o) -> (JSId) visit(o))
                .collect(Collectors.toList());
        List<JSAst> ariths = ctx.arithOperation()
                .stream()
                .map((m) -> visit(m))
                .collect(Collectors.toList());
        JSAst a = ariths.get(0);
        JSAst point = ariths.stream()
                .skip(1)
                .reduce(POINT(0, a), (z, m) -> {
                    JSPoint p = (JSPoint) z;
                    int k = p.index;
                    return POINT(p.add(1).index,OPERATION(opers.get(k), p.y, m));
                });

			if(((JSPoint)point).y.getClass().getName()=="pajama.js.JSString") return ((JSPoint) point)
		return FUNCTION(FORMALS(X),RET(((JSPoint) point).y)); 
			
    }
	
	@Override
    public JSAst visitRelOperator(PajamaParser.RelOperatorContext ctx) {
		System.err.println("visitRelOperator");
		return ID(ctx.op.getText());
	}
	

    @Override
    public JSAst visitExprNum(PajamaParser.ExprNumContext ctx) {
		System.err.println("visitExprNum");
        return NUM(Integer.valueOf(ctx.NUMBER().getText()));
    }

    @Override
    public JSAst visitExprTrue(PajamaParser.ExprTrueContext ctx) {
        return TRUE;
    }

    @Override
    public JSAst visitExprFalse(PajamaParser.ExprFalseContext ctx) {
        return FALSE;
    }
}
