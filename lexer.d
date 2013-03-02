module Puzzle.Lexer;

version (all) {
	import std.stdio : writeln;
	import std.datetime : StopWatch;
}

import std.ascii : isDigit, isAlphaNum, isWhite;
import std.file : readText, exists;
import std.string : format;

enum TokType {
	Assign, /// =
	At, /// @
	BitAnd, /// &
	BitAndAssign, /// &=
	BitOr, /// |
	BitOrAssign, /// |=
	CatAssign, /// ~=
	Colon, /// :
	Comma, /// ,
	Decrement, /// --
	Div, /// /
	DivAssign, /// /=
	Dollar, /// $
	Dot, /// .
	Equals, /// ==
	GoesTo, /// =>
	Greater, /// >
	GreaterEqual, /// >=
	Hash, /// #
	Increment, /// ++
	LBrace, /// {
	LBracket, /// [
	Less, /// <
	LessEqual, /// <=
	LessEqualGreater, /// <>=
	LessOrGreater, /// <>
	LogicAnd, /// &&
	LogicOr, /// ||
	LParen, /// $(LPAREN)
	Minus, /// -
	MinusAssign, /// -=
	Mod, /// %
	ModAssign, /// %=
	MulAssign, /// *=
	Not, /// !
	NotEquals, /// !=
	NotGreater, /// !>
	NotGreaterEqual, /// !>=
	NotLess, /// !<
	NotLessEqual, /// !<=
	NotLessEqualGreater, /// !<>
	Plus, /// +
	PlusAssign, /// +=
	Pow, /// ^^
	PowAssign, /// ^^=
	RBrace, /// }
	RBracket, /// ]
	RParen, /// $(RPAREN)
	Semicolon, /// ;
	ShiftLeft, /// <<
	ShiftLeftAssign, /// <<=
	ShiftRight, /// >>
	ShiftRightAssign, /// >>=
	Slice, /// ..
	Star, /// *
	Ternary, /// ?
	Tilde, /// ~
	Unordered, /// !<>=
	UnsignedShiftRight, /// >>>
	UnsignedShiftRightAssign, /// >>>=
	VarArg, /// ...
	Xor, /// ^
	XorAssign, /// ^=
	
	Backslash, /// \
	
	// Comment, /// $(D_COMMENT /** comment */) or $(D_COMMENT // comment) or $(D_COMMENT ///comment)
	Type, /// primitive types
	Keyword,	 /// Keywords
	Identifier, /// anything else
	
	Whitespace, /// whitespace
	Newline,
	DoubleLiteral, /// 123.456
	FloatLiteral, /// 123.456f or 0x123_45p-3
	// IdoubleLiteral, /// 123.456i
	// IfloatLiteral, /// 123.456fi
	IntLiteral, /// 123 or 0b1101010101
	LongLiteral, /// 123L
	RealLiteral, /// 123.456L
	
	// IrealLiteral, /// 123.456Li
	UintLiteral, /// 123u
	UlongLiteral, /// 123uL
	CharacterLiteral, /// 'a'
	DStringLiteral, /// $(D_STRING 32-bit character stringd)
	StringLiteral, /// $(D_STRING an 8-bit string)
	WStringLiteral, /// $(D_STRING 16-bit character stringw)";
	RegexStringLiteral, /// all string literals which starts with 'r'
	HexLiteral, /// 0xFFFFFF
	BinaryLiteral, /// 0b010011
}

immutable string[63] tokenValues = [
	"=",
	"@",
	"&",
	"&=",
	"|",
	"|=",
	"~=",
	":",
	",",
	"--",
	"/",
	"/=",
	"$",
	".",
	"==",
	"=>",
	">",
	">=",
	"#",
	"++",
	"{",
	"[",
	"<",
	"<=",
	"<>=",
	"<>",
	"&&",
	"||",
	"(",
	"-",
	"-=",
	"%",
	"%=",
	"*=",
	"!",
	"!=",
	"!>",
	"!>=",
	"!<",
	"!<=",
	"!<>",
	"+",
	"+=",
	"^^",
	"^^=",
	"}",
	"]",
	")",
	";",
	"<<",
	"<<=",
	">>",
	">>=",
	"..",
	"*",
	"?",
	"~",
	"!<>=",
	">>>",
	">>>=",
	"...",
	"^",
	"^=",
];

// lookup table for types
immutable string[28] types = [
	"bool",
	"byte",
	"cdouble",
	"cent",
	"cfloat",
	"char",
	"creal",
	"dchar",
	"dstring",
	"double",
	"float",
	"function",
	"idouble",
	"ifloat",
	"int",
	"ireal",
	"long",
	"real",
	"short",
	"string",
	"ubyte",
	"ucent",
	"uint",
	"ulong",
	"ushort",
	"void",
	"wchar",
	"wstring"
];

// lookup table for keywords
immutable string[83] keywords = [
	"__FILE__",
	"__LINE__",
	"__gshared",
	"__traits",
	"__vector",
	"__parameters",
	"abstract",
	"alias",
	"align",
	"asm",
	"assert",
	"auto",
	"body",
	"break",
	"case",
	"cast",
	"catch",
	"class",
	"const",
	"continue",
	"debug",
	"default",
	"delegate",
	"delete",
	"deprecated",
	"do",
	"else",
	"enum",
	"export",
	"extern",
	"false",
	"final",
	"finally",
	"for",
	"foreach",
	"foreach_reverse",
	"goto",
	"if",
	"immutable",
	"import",
	"in",
	"inout",
	"interface",
	"invariant",
	"is",
	"lazy",
	"macro",
	"mixin",
	"module",
	"new",
	"nothrow",
	"null",
	"out",
	"override",
	"package",
	"pragma",
	"private",
	"protected",
	"public",
	"pure",
	"ref",
	"return",
	"scope",
	"shared",
	"static",
	"struct",
	"super",
	"switch",
	"synchronized",
	"template",
	"this",
	"throw",
	"true",
	"try",
	"typedef",
	"typeid",
	"typeof",
	"union",
	"unittest",
	"version",
	"volatile",
	"while",
	"with"
];

immutable ubyte[char] typeMap;
immutable ubyte[char] keywordMap;

static this() {
	typeMap = [
		'd' : 7, 'l' : 16, 'i' : 12, 'u' : 20, 'b' : 0, 'f' : 10, 'r' : 17, 'v' : 25, 'c' : 2, 's' : 18, 'w' : 26
	];
	
	keywordMap = [
		'_' : 0, 'a' : 6, 'b' : 12, 'c' : 14, 'd' : 20, 'e' : 26, 'f' : 30, 'g' : 36, 'i' : 37, 'l' : 45, 
		'm' : 46, 'n' : 49, 'o' : 52, 'p' : 54, 'r' : 60, 's' : 62, 't' : 69, 'u' : 77, 'v' : 79, 'w' : 81
	];
}

bool isKeyword(string value) pure nothrow {
	if (value[0] !in keywordMap) return false;
	
	foreach (string kword; keywords[keywordMap[value[0]] .. $]) {
		if (kword == value) return true;
		if (kword[0] != value[0]) return false;
	}
	
	return false;
}

bool isType(string value) pure nothrow {
	if (value[0] !in typeMap) return false;
	
	foreach (string type; types[typeMap[value[0]] .. $]) {
		if (type == value) return true;
		if (type[0] != value[0]) return false;
	}
	
	return false;
}

string getTokenValue(TokType type) pure nothrow {
	return type < tokenValues.length ? tokenValues[type] : null;
}

struct Token {
public:
	const size_t line;
	const size_t pos;
	const size_t length;
	const char* ptr;
	const TokType type;
	
	@disable
	this();
	
	this(TokType tok, size_t line, size_t pos, const char* ptr = null, size_t length = 1) {
		this.type = tok;
		this.line = line;
		this.pos = pos;
		this.ptr = ptr;
		this.length = length;
	}
	
	// @disable
	// this(this);
	
	const(string) value() const pure nothrow {
		return this.ptr is null ? getTokenValue(this.type) : this.ptr[0 .. this.length];
	}
}

bool isNext(string str, size_t* idx, char c) pure nothrow {
	if (str.length <= *idx + 1) {
		return false;
	}
	
	if (str[*idx + 1] == c) {
		(*idx)++;
		
		return true;
	}
	
	return false;
}

char getNext(string str, size_t* idx) pure nothrow {
	(*idx)++;
	
	if (str.length == *idx) {
		return char.init;
	}
	
	return str[*idx];
}

enum Comment {
	Plus,
	Star,
	Line,
	None
}

Token[] lexical(string filename) {
	if (!exists(filename)) throw new Exception("File does not exist: " ~ filename);
	const string text = readText(filename);
	
	size_t line = 1;
	size_t last, index;
	bool ignore, loop;
	string id;
	
	Comment ctype = Comment.None;
	
	Token[] toks;
	//toks.length = cast(size_t)(text.length * 0.1f);
	
	for (size_t i = 0; i < text.length; ++i) {
		if (ignore && text[i + 1] == '/') {
			if (ctype == Comment.Plus && text[i] == '+') {
				ignore = false;
				ctype = Comment.None;
				
				i += 2;
			} else if (ctype == Comment.Star && text[i] == '*') {
				ignore = false;
				ctype = Comment.None;
				
				i += 2;
			}
		}
		
		if (!ignore && text[i] == '/' && text[i + 1] == '/') { // ignore single comment
			ignore = true;
			ctype = Comment.Line;
			
			i += 2;
		} else if (!ignore && text[i] == '/' && text[i + 1] == '*') { // ignore multi line comments
			ignore = true;
			ctype = Comment.Star;
			
			i += 2;
		} else if (!ignore && text[i] == '/' && text[i + 1] == '+') { // ignore multi line comments
			ignore = true;
			ctype = Comment.Plus;
			
			i += 2;
		}
		
		if (ignore) goto Ldef;
		
		switch (text[i]) {
			case '&':
				switch (getNext(text, &i)) {
					case '&': toks ~= Token(TokType.LogicAnd, line, i); break;
					case '=': toks ~= Token(TokType.BitAndAssign, line, i); break;
					default:
						toks ~= Token(TokType.BitAnd, line, i);
						--i; // Because we have read a character too much.
				}
			break;
			
			case '|':
				switch (getNext(text, &i)) {
					case '|': toks ~= Token(TokType.LogicOr, line, i); break;
					case '=': toks ~= Token(TokType.BitOrAssign, line, i); break;
					default:
						toks ~= Token(TokType.BitOr, line, i);
						--i; // Because we have read a character too much.
				}
			break;
			
			case '=':
				switch (getNext(text, &i)) {
					case '>': toks ~= Token(TokType.GoesTo, line, i); break;
					case '=': toks ~= Token(TokType.Equals, line, i); break;
					default:
						toks ~= Token(TokType.Assign, line, i);
						--i; // Because we have read a character too much.
				}
			break;
			
			case '!':
				if (isNext(text, &i, '=')) {
					toks ~= Token(TokType.NotEquals, line, i);
				} else {
					toks ~= Token(TokType.Not, line, i);
				}
			break;
			
			case '@': toks ~= Token(TokType.At, line, i); break;
			case '?': toks ~= Token(TokType.Ternary, line, i); break;
			case '#': toks ~= Token(TokType.Hash, line, i); break;
			case '$': toks ~= Token(TokType.Dollar, line, i); break;
			
			case ',': toks ~= Token(TokType.Comma, line, i);
			case ':': toks ~= Token(TokType.Colon, line, i);
			case ';': toks ~= Token(TokType.Semicolon, line, i);
			
			case '(': toks ~= Token(TokType.LParen, line, i); break;
			case ')': toks ~= Token(TokType.RParen, line, i); break;
			case '[': toks ~= Token(TokType.LBracket, line, i); break;
			case ']': toks ~= Token(TokType.RBracket, line, i); break;
			case '{': toks ~= Token(TokType.LBrace, line, i); break;
			case '}': toks ~= Token(TokType.RBrace, line, i); break;
			case '\\': toks ~= Token(TokType.Backslash, line, i); break;
			
			case '+':
				switch (getNext(text, &i)) {
					case '+': toks ~= Token(TokType.Increment, line, i); break;
					case '=': toks ~= Token(TokType.PlusAssign, line, i); break;
					default:
						toks ~= Token(TokType.Plus, line, i);
						--i; // Because we have read a character too much.
				}
			break;
			
			case '-':
				switch (getNext(text, &i)) {
					case '-': toks ~= Token(TokType.Decrement, line, i); break;
					case '=': toks ~= Token(TokType.MinusAssign, line, i); break;
					default:
						toks ~= Token(TokType.Minus, line, i);
						--i; // Because we have read a character too much.
				}
			break;
			
			case '*':
				if (isNext(text, &i, '=')) {
					toks ~= Token(TokType.MulAssign, line, i);
				} else {
					toks ~= Token(TokType.Star, line, i);
				}
			break;
			
			case '/':
				if (isNext(text, &i, '=')) {
					toks ~= Token(TokType.DivAssign, line, i);
				} else {
					toks ~= Token(TokType.Div, line, i);
				}
			break;
			
			case '%':
				if (isNext(text, &i, '=')) {
					toks ~= Token(TokType.ModAssign, line, i);
				} else {
					toks ~= Token(TokType.Mod, line, i);
				}
			break;
			
			case '^':
				switch (getNext(text, &i)) {
					case '^': 
						if (isNext(text, &i, '=')) {
							toks ~= Token(TokType.PowAssign, line, i);
						} else {
							toks ~= Token(TokType.Pow, line, i);
						}
					break;
					default:
						--i; // Because we have read a character too much.
						if (isNext(text, &i, '=')) {
							toks ~= Token(TokType.XorAssign, line, i);
						} else {
							toks ~= Token(TokType.Xor, line, i);
						}
				}
			break;
			
			case '<':
				switch (getNext(text, &i)) {
					case '=': toks ~= Token(TokType.LessEqual, line, i); break;
					case '<':
						if (isNext(text, &i, '=')) {
							toks ~= Token(TokType.ShiftLeftAssign, line, i);
						} else {
							toks ~= Token(TokType.ShiftLeft, line, i);
						}
					break;
					case '>': toks ~= Token(TokType.LessOrGreater, line, i); break;
					default:
						toks ~= Token(TokType.Less, line, i);
						--i;
				}
			break;
			
			case '>':
				switch (getNext(text, &i)) {
					case '=': toks ~= Token(TokType.GreaterEqual, line, i); break;
					case '>':
						if (isNext(text, &i, '=')) {
							toks ~= Token(TokType.ShiftRightAssign, line, i);
						} else if (isNext(text, &i, '>')) {
							if (isNext(text, &i, '=')) {
								toks ~= Token(TokType.UnsignedShiftRightAssign, line, i);
							} else {
								toks ~= Token(TokType.UnsignedShiftRight, line, i);
							}
						} else {
							toks ~= Token(TokType.ShiftRight, line, i);
						}
					break;
					default:
						toks ~= Token(TokType.Greater, line, i);
						--i; // Because we have read a character too much.
				}
			break;
			
			case '~':
				if (isNext(text, &i, '=')) {
					toks ~= Token(TokType.CatAssign, line, i);
				} else {
					toks ~= Token(TokType.Tilde, line, i);
				}
			break;
			
			case '.':
				if (isNext(text, &i, '.')) {
					if (isNext(text, &i, '.')) {
						toks ~= Token(TokType.VarArg, line, i);
					} else {
						toks ~= Token(TokType.Slice, line, i);
					}
				} else {
					toks ~= Token(TokType.Dot, line, i);
				}
			break;
			
			case '0': .. case '9':
				last = i;
				
				if (text[i] == '0' && isNext(text, &i, 'x')) {
					loop = true;
					while (loop) {
						switch (text[i + 1]) {
							case 'a': .. case 'f':
							case 'A': .. case 'F':
								++i;
							break;
							default: loop = false;
						}
					}
					
					toks ~= Token(TokType.HexLiteral, line, last, &text[last], i - last);
					
					break;
				}
				
				if (text[i] == '0' && isNext(text, &i, 'b')) {
					loop = true;
					while (loop) {
						switch (text[i + 1]) {
							case '0':
							case '1':
								i++;
							break;
							default: loop = false;
						}
					}
					
					toks ~= Token(TokType.BinaryLiteral, line, last, &text[last], i - last);
					
					break;
				}
				
				index = i;
				while (isDigit(text[index])) {
					++index;
					if (text[index] == '_' && getNext(text, &index).isDigit()) {
						++index;
					}
					// if (text[index] == '\n') throw new Exception("WRONG #1");
				}
				i = index - 1;
				
				switch (text[index]) {
					case '.':
						index++;
						
						while (isDigit(text[index])) {
							++index;
							if (text[index] == '_' && getNext(text, &index).isDigit()) {
								++index;
							}
							// if (text[index] == '\n') throw new Exception("WRONG 1.2");
						}
						i = index - 1;
						
						if (isNext(text, &index, 'f') || isNext(text, &index, 'F')) {
							toks ~= Token(TokType.FloatLiteral, line, last, &text[last], index - last);
						} else if (isNext(text, &index, 'l') || isNext(text, &index, 'L')) {
							toks ~= Token(TokType.RealLiteral, line, last, &text[last], index - last);
						} else {
							toks ~= Token(TokType.DoubleLiteral, line, last, &text[last], index - last);
						}
					break;
					
					case 'l':
					case 'L':
						toks ~= Token(TokType.LongLiteral, line, last, &text[last], index - last);
					break;
					
					case 'u':
					case 'U':
						if (isNext(text, &index, 'l') || isNext(text, &index, 'L')) {
							toks ~= Token(TokType.UlongLiteral, line, last, &text[last], index - last);
						} else {
							toks ~= Token(TokType.UintLiteral, line, last, &text[last], index - last);
						}
					break;
					
					default: toks ~= Token(TokType.IntLiteral, line, last, &text[last], index - last);
				}
			break;
			
			case '_':
			case 'a': .. case 'z':
			case 'A': .. case 'Z':
				if ((text[i] == 'w' || text[i] == 'd' || text[i] == 'r') && isNext(text, &i, '"')) {
					goto case '"';
				}
				
				last = index = i;
				while (text[index].isAlphaNum() || text[index] == '_') {
					index++;
					// if (text[index] == '\n') throw new Exception("WRONG 1.3");
				}
				i = index - 1;
				
				id = text[last .. index];
				if (isType(id)) {
					toks ~= Token(TokType.Type, line, last, &text[last], index - last);
				} else if (isKeyword(id)) {
					toks ~= Token(TokType.Keyword, line, last, &text[last], index - last);
				} else {
					toks ~= Token(TokType.Identifier, line, last, &text[last], index - last);
				}
			break;
			
			case '`':
				last = i;
				while (!isNext(text, &i, '`')) {
					++i;
				}
				
				toks ~= Token(TokType.RegexStringLiteral, line, last, &text[last], i - last);
			break;
			
			case '\'':
				char c;
				if (!isNext(text, &i, '\'')) {
					c = text[i];
					++i;
				}
				
				toks ~= Token(TokType.CharacterLiteral, line, i, &c);
			break;
			
			case '"':
				last = i;
				while (!isNext(text, &i, '"')) {
					++i;
				}
				
				switch (text[last - 1]) {
					case 'w': toks ~= Token(TokType.WStringLiteral, line, last, &text[last], i - last); break;
					case 'd': toks ~= Token(TokType.DStringLiteral, line, last, &text[last], i - last); break;
					case 'r': toks ~= Token(TokType.RegexStringLiteral, line, last, &text[last], i - last); break;
					default: toks ~= Token(TokType.StringLiteral, line, last, &text[last], i - last);
				}
			break;
			
			default:
				Ldef:
				if (text[i].isWhite()) {
					if (text[i] == '\n') {
						if (ctype == Comment.Line) {
							assert(ignore, "No ignore while comment.");
							
							ignore = false;
							ctype = Comment.None;
						}
						
						isNext(text, &i, '\r');
						++line;
						
						toks ~= Token(TokType.Newline, line, i);
					} else {
						toks ~= Token(TokType.Whitespace, line, i);
					}
				} else if (!ignore) {
					throw new Exception("Undefinied Token: " ~ text[i], "", line);
				}
			break;
		}
	}
	
	return toks;
}

void main() {
	// import core.memory : GC;
	// GC.disable();
	
	StopWatch sw;
	sw.start();
	
	string filename;
	version (Test) {
		filename = "rvalue_ref_model.d";
	} else {
		filename = "D:/D/dmd2/src/phobos/std/datetime.d";
	}
	
	Token[] toks = lexical(filename);
	
	sw.stop();
	
	writeln("Duration: ", sw.peek().msecs, " msecs total.");
	// writeln("Duration: ", sw2.peek().msecs, " msecs lexer.");
	
	foreach (ref Token t; toks) {
		if (t.type == TokType.BinaryLiteral) writeln(t.line, ':', t.value());
		version (Test) if (t.type != TokType.Newline && t.type != TokType.Whitespace) writeln(t.type, ':', t.line, ':', t.value());
	}
	
	// GC.enable();
}