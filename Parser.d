module Puzzle.Parser;

import std.stdio;
import std.array : back;
import std.string : format;
import std.exception : enforce;
import std.conv : to;

import Puzzle.Lexer;
import Puzzle.Types;

/**
Token stream, to simplify parsing
*/
struct TokStream {
public:
	/// Current index
	size_t index;
    /// Internal token array
    const Token[] tokens;
	
    this(ref const Token[] tokens) {
        this.tokens = tokens;
    }
	
	void move(int index) {
		this.index += index;
	}
	
	void moveBack() {
		this.index--;
	}

    ref const(Token) peek() const pure nothrow {
        return this.tokens[this.index];
    }
	
	ref const(Token) peekNext() const pure nothrow {
        return this.tokens[this.index + 1];
    }

    ref const(Token) pop() pure nothrow {
        // Cannot read the last (EOF) token
        assert(this.index < this.tokens.length, "cannot read final EOF token");
        this.index++;
		
        return this.top();
    }
	
	ref const(Token) top() const pure nothrow {
		return this.tokens[this.index - 1];
	}
	
	ref const(Token) previous(ubyte num = 2) const pure nothrow {
		return this.tokens[this.index - num];
	}
	
	bool match(TokType type) pure nothrow {
		if (this.peek().type == type) {
			this.pop();
			return true;
		}
		
		return false;
	}
	
	bool match(string val) pure nothrow {
		if (this.peek().value == val) {
			this.pop();
			return true;
		}
		
		return false;
	}
	
    bool isEof() const pure nothrow {
        return this.index >= this.tokens.length;;
    }
}

VarAttributes searchForVarAttr(ref const TokStream ts, ubyte i) pure {
	VarAttributes vattr = isVarAttribute(ts.tokens[ts.index - i].value);
	if (vattr != VarAttributes.none) {
		i++;
		for (;; ++i) {
			vattr |= isVarAttribute(ts.tokens[ts.index - i].value);
			if (vattr & VarAttributes.none) {
				vattr &= ~VarAttributes.none;
				break;
			}
		}
		enforce(!(vattr & VarAttributes.none));
	}
	
	return vattr;
}

string resolveOType(TokType type) {
	switch (type) {
		case TokType.DoubleLiteral:
			return "double";
		case TokType.FloatLiteral:
			return "float";
		case TokType.IntLiteral:
			return "int";
		case TokType.LongLiteral:
			return "long";
		case TokType.RealLiteral:
			return "real";
		case TokType.HexLiteral:
		case TokType.BinaryLiteral:
		case TokType.UintLiteral:
			return "uint";
		case TokType.UlongLiteral:
			return "ulong";
		case TokType.CharacterLiteral:
			return "char";
		case TokType.DStringLiteral:
			return "dstring";
		case TokType.RegexStringLiteral:
		case TokType.StringLiteral:
			return "string";
		case TokType.WStringLiteral:
			return "wstring";
		default: throw new Exception(format("Unknown type: %s", type));
	}
}

Variable[] searchForParams(size_t* offset, ref const TokStream ts) {
	Variable[] fparams;
	
	size_t mark;
	size_t i = *offset;
	
	while (ts.tokens[i].type != TokType.RParen) {
		for (; ts.tokens[i].type != TokType.Comma; ++i) {
			// writeln(" --> ", ts.tokens[i].value, " -> ", ts.tokens[i].type);
			mark = i;
			TokType ct = ts.tokens[i].type;
			
			if (ct == TokType.Identifier || ct == TokType.Type) {
				string param, type;
				type = ts.tokens[i].value;
				i++;
				
				bool isPtr, isArray, isCppRef, isTpl, isLnk;
				int aDim = -1;
				switch (ts.tokens[i].type) {
					case TokType.Star:
						i++;
						isPtr = true;
					break;
					/*case TokType.BitAnd:
						i++;
						isCppRef = true;
					break;*/
					case TokType.LBracket:
						i++;
						for (; ts.tokens[i].type != TokType.RBracket; ++i) {
							if (ts.tokens[i].type == TokType.IntLiteral) {
								writeln(" --> Array Dimension: ", ts.tokens[i].value);
								aDim = to!(int)(ts.tokens[i].value);
							}
						}
						i++;
						isArray = true;
					break;
					case TokType.Not:
						i++;
						isTpl = true;
						
						if (ts.tokens[i + 1].type == TokType.LParen) {
							i += 2;
							for (; ts.tokens[i].type != TokType.RParen; ++i) { }
							i += 2;
						}
					break;
					case TokType.Dot:
						i++;
						isLnk = true;
					break;
					default: break;
				}
				
				enforce(ts.tokens[i].type == TokType.Identifier, ts.tokens[i].value ~ " - " ~ type);
				param = ts.tokens[i].value;
				
				if (isTpl || isLnk) {
					type ~= (isTpl ? '!' : '.') ~ param;
					i++;
					enforce(ts.tokens[i].type == TokType.Identifier, ts.tokens[i].value);
					param = ts.tokens[i].value;
				} 
				
				// writeln(" --> ", type, " -> ", param);
				fparams ~= new Variable(param, ts.tokens[i].line, type, isPtr, isArray);
				fparams.back.arrayDim = aDim;
				
				/// Search for Modifier
				mark--;
				for (; ts.tokens[mark].type == TokType.Keyword; --mark) {
					fparams.back.vattr |= isVarAttribute(ts.tokens[mark].value);
				}
				
				if (ts.tokens[i + 1].type == TokType.Assign) {
					// ignore default value
					for (; ts.tokens[i].type != TokType.RParen; ++i) { }
					--i;
				}
			}
			
			if (ts.tokens[i].type != TokType.RParen)
				break;
		}
		++i;
	}
	*offset = i;
	
	return fparams;
}

void parse(string filename) {
	Token[] toks = tokenize(filename);
	
	size_t nvars, narrs, nfuncs;
	
	TokStream ts = TokStream(toks);
	
	Variable[] vars;
	Function[] funcs;
	
	while (!ts.isEof()) {
		// if (ts.peek().type == TokType.Newline || ts.peek().type == TokType.Whitespace) {
			// ts.pop();
			// continue;
		// }
		Token cur = ts.pop();
		
		switch (cur.type) {
			case TokType.Type:
			// case TokType.Identifier:
				Token t = ts.pop();
				
				if (t.type == TokType.Identifier && ts.peek().type == TokType.LParen) {
					debug writeln("Found function ", t.value, " on line ", t.line, " with type ", cur.value);
					size_t idx = ts.index;
					
					/// ReturnType Attributes
					VarAttributes returnVattr;
					for (size_t j = idx - 3; ; j--) {
						debug writeln(" -> ", ts.tokens[j].value);
						if (ts.tokens[j].type != TokType.Keyword)
							break;
						returnVattr |= isVarAttribute(ts.tokens[j].value);
					}
					// if (returnVattr & VarAttributes.none)
						// returnVattr &= ~VarAttributes.none;
					
					/// Mögliche Template Parameter
					string[] params;
					for (; ts.tokens[idx].type != TokType.RParen; ++idx) {
						if (ts.tokens[idx].type == TokType.Type
							|| ts.tokens[idx].type == TokType.Identifier)
						{
							debug writeln(ts.tokens[idx].value);
							if (ts.tokens[idx + 1].type == TokType.Type
								|| ts.tokens[idx + 1].type == TokType.Identifier)
							{
								params ~= ts.tokens[idx].value ~ " " ~ ts.tokens[idx + 1].value;
								idx++;
							} else {
								params ~= ts.tokens[idx].value;
							}
						}
					}
					/// Ist es ein Template?
					/// void foo(...)( <-
					if (ts.tokens[idx + 1].type == TokType.LParen) {
						debug writeln("TEMPLATE");
						debug writeln(params);
						funcs ~= new Function(t.value, t.line, cur.value, params, null);
						idx += 2;
						writeln("@LINE #1: ", t.value, ':', t.line);
						funcs.back.parameters = searchForParams(&idx, ts); ///
					} else {
						funcs ~= new Function(t.value, t.line, cur.value, null, null);
						idx = ts.index;
						writeln("@LINE #2: ", t.value, ':', t.line);
						funcs.back.parameters = searchForParams(&idx, ts); ///
					}
					
					/// Return Type Attributes
					funcs.back.returnVattr = returnVattr;
					
					/// Modifier
					for (; ts.tokens[idx].type != TokType.RParen; idx++) { }
					while (true) {
						idx++;
						debug writeln("=>", ts.tokens[idx].value);
						if (ts.tokens[idx].type != TokType.Keyword)
							break;
						funcs.back.fattr |= isFuncAttribute(ts.tokens[idx].value);
					}
					// if (funcs.back.fattr & FuncAttributes.none)
						// funcs.back.fattr &= ~FuncAttributes.none;
					
					++nfuncs;
				} else {
					switch (t.type) {
						case TokType.LBracket:
							while (ts.pop().type != TokType.RBracket) { }
							
							vars ~= new Variable(ts.pop().value, cur.line, cur.value, false, true);
							vars.back.vattr = searchForVarAttr(ts, 3);
							
							debug writeln("Array name: ", ts.top().value, " line: ", cur.line, " type: ", cur.value);
							++narrs;
						break;
						case TokType.Identifier:
							if (cur.value == "auto") {
								size_t i = ts.index;
								for (; ts.tokens[i].type != TokType.Assign; ++i) { }
								// writeln(" -> ", ts.tokens[i + 1].type);
								const string otype = ts.tokens[i + 1].type == TokType.Identifier ? 
									ts.tokens[i + 1].value : resolveOType(ts.tokens[i + 1].type);
								
								vars ~= new Variable(t.value, t.line, otype);
								debug writeln("Variable name: ", t.value, " line: ", t.line, " type: ",otype);
							} else {
								vars ~= new Variable(t.value, t.line, cur.value);
								debug writeln("Variable name: ", t.value, " line: ", t.line, " type: ", cur.value);
							}
							
							vars.back.vattr = searchForVarAttr(ts, 3);
							
							++nvars;
						break;
						case TokType.Star:
							enforce(ts.pop().type == TokType.Identifier);
							
							vars ~= new Variable(t.value, t.line, cur.value, true);
							vars.back.vattr = searchForVarAttr(ts, 3);
							
							debug writeln("Found pointer variable ", ts.top().value, " on line ", ts.top().line);
							++nvars;
						break;
						default: break;
					}
				}
			break;
			
			case TokType.Keyword:
				// writeln("Found Keyword: ", ts.top().value);
			break;
			
			default: break;
		}
	}
	
	writeln(nvars, " Variables, ", nfuncs, " functions, ", narrs, " Arrays.");
	
	writeln("Functions:");
	foreach (func; funcs)
		writeln(func.documentString());
	
	writeln("Variables:");
	foreach (var; vars)
		writeln(var.type, ' ', var.name, ' ', var.getBitSize());
}

void main() {
	version (Test) {
		parse("rvalue_ref_model.d");
	} else {
		parse("D:/D/dmd2/src/phobos/std/datetime.d");
	}
}