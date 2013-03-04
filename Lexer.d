module Puzzle.Lexer;

import std.stdio : writeln;

// import std.algorithm : canFind;
import std.ascii : isDigit, isAlphaNum, isWhite;
import std.file : read, exists;
import std.string : format;

import Puzzle.Appender;

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
immutable string[][ubyte.max] types = [
	//null,
	["auto"],
	["bool", "byte"],
	["cdouble", "cent", "cfloat", "char", "creal"],
	["dchar", "dstring", "double"],
	null,
	["float"],
	null,
	null, 
	["idouble", "ifloat", "int", "ireal"],
	null,
	null,
	["long"],
	null,
	null,
	null,
	null,
	null,
	["real"],
	["short", "size_t", "string"],
	null,
	["ubyte", "ucent", "uint", "ulong", "ushort"],
	["void"],
	["wchar", "wstring"],
	null,
	null,
	null
];

// lookup table for keywords
immutable string[][ubyte.max] keywords = [
	["__FILE__", "__LINE__", "__gshared", "__traits", "__vector", "__parameters"],
	["abstract", "alias", "align", "asm", "assert"/*, "auto"*/],
	["body", "break"],
	["case", "cast", "catch", "class", "const", "continue"],
	["debug", "default", "delegate", "delete", "deprecated", "do"],
	["else", "enum", "export", "extern"],
	["false", "final", "finally", "for", "foreach", "foreach_reverse", "function"],
	["goto"],
	null,
	["if", "immutable", "import", "in", "inout", "interface", "invariant", "is"],
	null,
	null,
	["lazy"],
	["macro", "mixin", "module"],
	["new", "nothrow", "null"],
	["out", "override"],
	["package", "pragma", "private", "protected", "public", "pure"],
	null,
	["ref", "return"],
	["scope", "shared", "static", "struct", "super", "switch", "synchronized"],
	["template", "this", "throw", "true", "try", "typedef", "typeid", "typeof"],
	["union", "unittest"],
	["version", "volatile"],
	["while", "with"],
	null,
	null,
	null
];

enum sub = 'a' - 1;

bool isKeyword(string value) pure nothrow {
	const ubyte idx = value[0] != '_' ? cast(ubyte)(value[0] - sub) : 0;
	
	return keywords[idx] !is null && keywords[idx].canFind(value);
}

bool isType(string value) pure nothrow {
	const ubyte idx = cast(ubyte)(value[0] - 'a');
	
	return types[idx] !is null && types[idx].canFind(value);
}

bool canFind(immutable string[] values, string value) pure nothrow {
	foreach (_val; values) {
		if (_val == value) return true;
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
	
	this(TokType tok, size_t line, size_t pos, const char* ptr = null, size_t length = 1) {
		this.type = tok;
		this.line = line;
		this.pos = pos;
		this.ptr = ptr;
		this.length = length;
	}
	
	// @disable
	// this(this);
	
	@property
	string value() const pure nothrow {
		return this.ptr is null ? getTokenValue(this.type) : this.ptr[0 .. this.length];
	}
}

/**
String stream, used to lex from strings
*/
struct InputStream {
public:
    /// Input string
    const string text;
    /// File name
    const string file;
    /// Current index
    size_t index = 0;
    /// Current line number
	size_t line = 1;

    this(string file) {
		assert(exists(file), "File does not exist: " ~ file);
		
        this.text = cast(string) read(file);
        this.file = file;
    }
	
	void move(int len) pure nothrow {
		this.index += len;
	}
	
	void moveBack() pure nothrow {
		this.index--;
	}
	
    /// Read a character and advance the current index
    char popCh() pure nothrow {
        char ch = this.peekCh();
		this.index++;
		
        return ch;
    }
	
	char topCh() const pure nothrow {
		return this.text[this.index - 1];
	}

    /// Read a character without advancing the index.
    char peekCh() const pure nothrow {
        // return (this.index < this.text.length) ? this.text[this.index] : '\0';
		return this.text[this.index];
    }
	
	/// Read the next character without advancing the index.
    char peekNextCh() const pure nothrow {
		return (this.index + 1 < this.text.length) ? this.text[this.index + 1] : '\0';
		// return this.text[this.index + 1];
    }
	
	/// Test for a match with a given character. The position is moved if matched.
    bool match(char c) pure nothrow {
        if (this.peekCh() == c) {
			this.index++;
			return true;
		}
		
		return false;
    }
	
	/// Test for a match of the next character with the given character.
	bool isNext(char c) pure nothrow {
		return this.peekNextCh() == c;
	}
	
	bool isEof() const pure nothrow {
		return this.index >= this.text.length;
	}
}

enum Comment {
	Plus,
	Star,
	Line,
	None
}

Token[] tokenize(string filename) {
	InputStream instr = InputStream(filename);
	
	size_t last;
	bool ignore, loop;
	string id;
	
	Comment ctype = Comment.None;
	
	Appender!(Token[]) toks;
	toks.reserve(cast(size_t)(instr.text.length * 0.6f));
	
	while (!instr.isEof()) {
		if (ignore && instr.isNext('/')) {
			if (ctype == Comment.Plus && instr.peekCh() == '+') {
				ignore = false;
				ctype = Comment.None;
				
				instr.move(2);
			} else if (ctype == Comment.Star && instr.peekCh() == '*') {
				ignore = false;
				ctype = Comment.None;
				
				instr.move(2);
			}
		}
		
		if (!ignore && instr.peekCh() == '/' && instr.isNext('/')) { // ignore single comment
			ignore = true;
			ctype = Comment.Line;
			
			instr.move(2);
		} else if (!ignore && instr.peekCh() == '/' && instr.isNext('*')) { // ignore multi line comments
			ignore = true;
			ctype = Comment.Star;
			
			instr.move(2);
		} else if (!ignore && instr.peekCh() == '/' && instr.isNext('+')) { // ignore multi line comments
			ignore = true;
			ctype = Comment.Plus;
			
			instr.move(2);
		}
		
		if (ignore) {
			instr.popCh();
			goto Ldef;
		}
		
		switch (instr.popCh()) {
			case '&':
				switch (instr.popCh()) {
					case '&': toks ~= Token(TokType.LogicAnd, instr.line, instr.index); break;
					case '=': toks ~= Token(TokType.BitAndAssign, instr.line, instr.index); break;
					default:
						instr.moveBack(); // Because we have read a character too much.
						toks ~= Token(TokType.BitAnd, instr.line, instr.index);
				}
			break;
			
			case '|':
				switch (instr.popCh()) {
					case '|': toks ~= Token(TokType.LogicOr, instr.line, instr.index); break;
					case '=': toks ~= Token(TokType.BitOrAssign, instr.line, instr.index); break;
					default:
						instr.moveBack(); // Because we have read a character too much.
						toks ~= Token(TokType.BitOr, instr.line, instr.index);
				}
			break;
			
			case '=':
				switch (instr.popCh()) {
					case '>': toks ~= Token(TokType.GoesTo, instr.line, instr.index); break;
					case '=': toks ~= Token(TokType.Equals, instr.line, instr.index); break;
					default:
						instr.moveBack(); // Because we have read a character too much.
						toks ~= Token(TokType.Assign, instr.line, instr.index);
				}
			break;
			
			case '!':
				if (instr.match('=')) {
					toks ~= Token(TokType.NotEquals, instr.line, instr.index);
				} else {
					toks ~= Token(TokType.Not, instr.line, instr.index);
				}
			break;
			
			case '@': toks ~= Token(TokType.At, instr.line, instr.index); break;
			case '?': toks ~= Token(TokType.Ternary, instr.line, instr.index); break;
			case '#': toks ~= Token(TokType.Hash, instr.line, instr.index); break;
			case '$': toks ~= Token(TokType.Dollar, instr.line, instr.index); break;
			
			case ',': toks ~= Token(TokType.Comma, instr.line, instr.index);
			case ':': toks ~= Token(TokType.Colon, instr.line, instr.index);
			case ';': toks ~= Token(TokType.Semicolon, instr.line, instr.index);
			
			case '(': toks ~= Token(TokType.LParen, instr.line, instr.index); break;
			case ')': toks ~= Token(TokType.RParen, instr.line, instr.index); break;
			case '[': toks ~= Token(TokType.LBracket, instr.line, instr.index); break;
			case ']': toks ~= Token(TokType.RBracket, instr.line, instr.index); break;
			case '{': toks ~= Token(TokType.LBrace, instr.line, instr.index); break;
			case '}': toks ~= Token(TokType.RBrace, instr.line, instr.index); break;
			case '\\': toks ~= Token(TokType.Backslash, instr.line, instr.index); break;
			
			case '+':
				switch (instr.popCh()) {
					case '+': toks ~= Token(TokType.Increment, instr.line, instr.index); break;
					case '=': toks ~= Token(TokType.PlusAssign, instr.line, instr.index); break;
					default:
						instr.moveBack(); // Because we have read a character too much.
						toks ~= Token(TokType.Plus, instr.line, instr.index);
				}
			break;
			
			case '-':
				switch (instr.popCh()) {
					case '-': toks ~= Token(TokType.Decrement, instr.line, instr.index); break;
					case '=': toks ~= Token(TokType.MinusAssign, instr.line, instr.index); break;
					default:
						instr.moveBack(); // Because we have read a character too much.
						toks ~= Token(TokType.Minus, instr.line, instr.index);
				}
			break;
			
			case '*':
				if (instr.match('=')) {
					toks ~= Token(TokType.MulAssign, instr.line, instr.index);
				} else {
					toks ~= Token(TokType.Star, instr.line, instr.index);
				}
			break;
			
			case '/':
				if (instr.match('=')) {
					toks ~= Token(TokType.DivAssign, instr.line, instr.index);
				} else {
					toks ~= Token(TokType.Div, instr.line, instr.index);
				}
			break;
			
			case '%':
				if (instr.match('=')) {
					toks ~= Token(TokType.ModAssign, instr.line, instr.index);
				} else {
					toks ~= Token(TokType.Mod, instr.line, instr.index);
				}
			break;
			
			case '^':
				switch (instr.popCh()) {
					case '^': 
						if (instr.match('=')) {
							toks ~= Token(TokType.PowAssign, instr.line, instr.index);
						} else {
							toks ~= Token(TokType.Pow, instr.line, instr.index);
						}
					break;
					default:
						instr.moveBack(); // Because we have read a character too much.
						if (instr.match('=')) {
							toks ~= Token(TokType.XorAssign, instr.line, instr.index);
						} else {
							toks ~= Token(TokType.Xor, instr.line, instr.index);
						}
				}
			break;
			
			case '<':
				switch (instr.popCh()) {
					case '=': toks ~= Token(TokType.LessEqual, instr.line, instr.index); break;
					case '<':
						if (instr.match('=')) {
							toks ~= Token(TokType.ShiftLeftAssign, instr.line, instr.index);
						} else {
							toks ~= Token(TokType.ShiftLeft, instr.line, instr.index);
						}
					break;
					case '>': toks ~= Token(TokType.LessOrGreater, instr.line, instr.index); break;
					default:
						instr.moveBack(); // Because we have read a character too much.
						toks ~= Token(TokType.Less, instr.line, instr.index);
				}
			break;
			
			case '>':
				switch (instr.popCh()) {
					case '=': toks ~= Token(TokType.GreaterEqual, instr.line, instr.index); break;
					case '>':
						if (instr.match('=')) {
							toks ~= Token(TokType.ShiftRightAssign, instr.line, instr.index);
						} else if (instr.match('>')) {
							if (instr.match('=')) {
								toks ~= Token(TokType.UnsignedShiftRightAssign, instr.line, instr.index);
							} else {
								toks ~= Token(TokType.UnsignedShiftRight, instr.line, instr.index);
							}
						} else {
							toks ~= Token(TokType.ShiftRight, instr.line, instr.index);
						}
					break;
					default:
						instr.moveBack(); // Because we have read a character too much.
						toks ~= Token(TokType.Greater, instr.line, instr.index);
				}
			break;
			
			case '~':
				if (instr.match('=')) {
					toks ~= Token(TokType.CatAssign, instr.line, instr.index);
				} else {
					toks ~= Token(TokType.Tilde, instr.line, instr.index);
				}
			break;
			
			case '.':
				if (instr.match('.')) {
					if (instr.match('.')) {
						toks ~= Token(TokType.VarArg, instr.line, instr.index);
					} else {
						toks ~= Token(TokType.Slice, instr.line, instr.index);
					}
				} else {
					toks ~= Token(TokType.Dot, instr.line, instr.index);
				}
			break;
			
			case '0': .. case '9':
				last = instr.index - 1;
				
				if (instr.topCh() == '0' && instr.match('x')) {
					loop = true;
					while (loop) {
						switch (instr.peekCh()) {
							case 'a': .. case 'f':
							case 'A': .. case 'F':
								instr.popCh();
							break;
							default: loop = false;
						}
					}
					
					toks ~= Token(TokType.HexLiteral, instr.line, last, &instr.text[last], instr.index - last);
					
					break;
				}
				
				if (instr.topCh() == '0' && instr.match('b')) {
					loop = true;
					while (loop) {
						switch (instr.peekCh()) {
							case '0':
							case '1':
								instr.popCh();
							break;
							default: loop = false;
						}
					}
					
					toks ~= Token(TokType.BinaryLiteral, instr.line, last, &instr.text[last], instr.index - last);
					
					break;
				}
				
				while (instr.peekCh().isDigit()) {
					if (instr.peekCh() == '_' && instr.peekNextCh().isDigit()) {
						instr.popCh();
					}
					debug if (instr.topCh() == '\n') throw new Exception("WRONG 1.2");
					instr.popCh();
				}
				
				switch (instr.popCh()) {
					case '.':
						while (instr.peekCh().isDigit()) {
							instr.popCh();
							
							if (instr.peekCh() == '_' && instr.peekNextCh().isDigit()) {
								instr.popCh();
							}
							debug if (instr.topCh() == '\n') throw new Exception("WRONG 1.3");
						}
						
						if (instr.match('f') || instr.match('F')) {
							toks ~= Token(TokType.FloatLiteral, instr.line, last, &instr.text[last], instr.index - last);
						} else if (instr.match('l') || instr.match('L')) {
							toks ~= Token(TokType.RealLiteral, instr.line, last, &instr.text[last], instr.index - last);
						} else {
							toks ~= Token(TokType.DoubleLiteral, instr.line, last, &instr.text[last], instr.index - last);
						}
					break;
					
					case 'l':
					case 'L':
						toks ~= Token(TokType.LongLiteral, instr.line, last, &instr.text[last], instr.index - last);
					break;
					
					case 'u':
					case 'U':
						if (instr.match('l') || instr.match('L')) {
							toks ~= Token(TokType.UlongLiteral, instr.line, last, &instr.text[last], instr.index - last);
						} else {
							toks ~= Token(TokType.UintLiteral, instr.line, last, &instr.text[last], instr.index - last);
						}
					break;
					
					default:
						instr.moveBack(); // Because we have read a character too much.
						toks ~= Token(TokType.IntLiteral, instr.line, last, &instr.text[last], instr.index - last);
				}
			break;
			
			case '_':
			case 'a': .. case 'z':
			case 'A': .. case 'Z':
				last = instr.index - 1;
				
				char c = instr.topCh();
				if ((c == 'w' || c == 'd' || c == 'r' || c == 'q') && instr.peekCh() == '"') {
					instr.popCh(); // pop "
					goto case '"';
				}
				
				while (instr.peekCh().isAlphaNum() || instr.peekCh() == '_') {
					instr.popCh();
					debug if (instr.topCh() == '\n') throw new Exception("WRONG 1.4");
				}
				
				id = instr.text[last .. instr.index];
				if (isType(id)) {
					toks ~= Token(TokType.Type, instr.line, last, &instr.text[last], instr.index - last);
				} else if (isKeyword(id)) {
					toks ~= Token(TokType.Keyword, instr.line, last, &instr.text[last], instr.index - last);
				} else {
					toks ~= Token(TokType.Identifier, instr.line, last, &instr.text[last], instr.index - last);
				}
			break;
			
			case '`':
				last = instr.index - 1;
				while (instr.popCh() != '`') { }
				
				toks ~= Token(TokType.RegexStringLiteral, instr.line, last, &instr.text[last], instr.index - last);
			break;
			
			case '\'':
				char c;
				if (instr.peekCh() != '\'') {
					c = instr.popCh();
				}
				instr.popCh();
				
				toks ~= Token(TokType.CharacterLiteral, instr.line, instr.index, &c);
			break;
			
			case '"':
				last = instr.index - 1;
				while (instr.popCh() != '"') {
					if (instr.topCh() == '\n') {
						instr.line++;
						instr.match('\r');
					}
				}
				
				switch (instr.text[last - 1]) {
					case 'w': toks ~= Token(TokType.WStringLiteral, instr.line, last, &instr.text[last], instr.index - last); break;
					case 'd': toks ~= Token(TokType.DStringLiteral, instr.line, last, &instr.text[last], instr.index - last); break;
					case 'r': toks ~= Token(TokType.RegexStringLiteral, instr.line, last, &instr.text[last], instr.index - last); break;
					default:
						if (instr.text[instr.index] == 'w') {
							loop = true;
							goto case 'w';
						} else if (instr.text[instr.index] == 'd') {
							loop = true;
							goto case 'd';
						} else if (instr.text[instr.index] == 'r') {
							loop = true;
							goto case 'r';
						}
						
						toks ~= Token(TokType.StringLiteral, instr.line, last, &instr.text[last], instr.index - last);
				}
				
				if (loop) {
					loop = false;
					instr.popCh();
				}
			break;
			
			default:
				Ldef:
				if (instr.topCh().isWhite()) {
					if (instr.topCh() == '\n') {
						if (ctype == Comment.Line) {
							assert(ignore, "No ignore while comment.");
							
							ignore = false;
							ctype = Comment.None;
						}
						
						do {
							instr.line++;
							instr.match('\r');
						} while (instr.popCh() == '\n');
						instr.moveBack(); // Because we have read a character too much.
						
						// toks ~= Token(TokType.Newline, instr.line, instr.index);
					// } else {
						// toks ~= Token(TokType.Whitespace, instr.line, instr.index);
					}
				} else if (!ignore) {
					throw new Exception("Undefinied Token: [" ~ instr.topCh() ~ ']', "", instr.line);
				}
			break;
		}
	}
	
	return toks.data;
}

void main() {
	string filename;
	version (Test) {
		filename = "rvalue_ref_model.d";
	} else {
		filename = "D:/D/dmd2/src/phobos/std/datetime.d";
	}
	
	Token[] toks = tokenize(filename);
	
	foreach (ref Token t; toks) {
		if (t.type == TokType.BinaryLiteral) writeln(t.line, ':', t.value());
		version (Test) if (t.type != TokType.Newline && t.type != TokType.Whitespace) writeln(t.type, ':', t.line, ':', t.value());
		// version (Test) if (t.type == TokType.WStringLiteral) writeln(t.value);
	}
}