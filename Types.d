module Puzzle.Types;

template uniqueId(int x) {
	enum uniqueId = 1 << (x - 1);
}

enum VarAttributes {
	none = uniqueId!(0),
	const_ = uniqueId!(1),
	immutable_ = uniqueId!(2),
	ref_ = uniqueId!(3),
	out_ = uniqueId!(4),
	scope_ = uniqueId!(5),
	lazy_ = uniqueId!(6),
	shared_ = uniqueId!(7),
	wild = uniqueId!(8)
}

enum FuncAttributes {
	none = uniqueId!(0),
	pure_ = uniqueId!(1),
	nothrow_ = uniqueId!(2),
	ref_ = uniqueId!(3),
	const_ = uniqueId!(4),
	immutable_ = uniqueId!(5),
	deprecated_ = uniqueId!(6),
	static_ = uniqueId!(7),
	extern_ = uniqueId!(8),
	final_ = uniqueId!(9),
	synchronized_ = uniqueId!(10),
	override_ = uniqueId!(11),
	abstract_ = uniqueId!(12),
	auto_ = uniqueId!(13),
	wild = uniqueId!(14),
	
	property = uniqueId!(15),
	trusted = uniqueId!(16),
	safe = uniqueId!(17),
	disable = uniqueId!(18)
}

enum Protection {
	public_  = uniqueId!(0),
	private_ = uniqueId!(1),
	protected_ = uniqueId!(2),
	package_ = uniqueId!(3),
	export_ = uniqueId!(4)
}

/**
 * Attributes common to everything interesting
 */
abstract class Base {
public:
	const string name;
	const size_t line;
	
	const Protection protection;
}

/**
 * Varible declaration
 */
class Variable : Base {
public:
	const VarAttributes vattr;
	const string type;
	const bool isPtr;
	
	string value;
	
	ubyte getBitSize() const pure nothrow {
		if (isPtr) return 32;
		
		switch (type) {
			case "uint":
			case "int": return 32;
			case "size_t": return 64; // TODO
			case "ulong":
			case "long": return 64;
			case "ucent":
			case "cent": return 128;
			case "ushort":
			case "short": return 16;
			case "ubyte":
			case "byte": return 8;
			case "bool": return 8;
			case "void": return 0;
			case "wchar": return 16;
			case "dchar": return 32;
			case "char": return 8;
			case "string": return 64; // TODO
			case "float":
			case "ifloat":
			case "cfloat": return 32;
			case "double":
			case "idouble":
			case "cdouble": return 64;
			case "real":
			case "ireal":
			case "creal": return 80;
			default: return 100; // TODO
		}
	}
}

/**
 * Alias Declaration
 */
class Alias : Base {
public:
	const string aliasedType;
}

/**
 * Base class for any type that can be a template
 */
abstract class Templateable : Base {
public:
	/// Template constraint, which may be null
	const string constraint;
	/// Template parameters, may be empty
	const string[] templateParameters;
}

/**
 * Functions and delegates
 */
class Function : Templateable {
public:
	/// Function return type
	const string returnType;
	/// Parameter list; may be empty
	const Variable[] parameters;
	const FuncAttributes fattr;

	string documentString() const pure nothrow {
		string r = returnType ~ " " ~ name ~ "(";
		foreach (i, param; parameters) {
			r ~= param.type ~ " " ~ param.name;
			if (i + 1 < parameters.length) r ~= ",\\n\\t";
		}
		
		r ~= ")";
		
		return r;
	}
}

/**
 * Stuff common to struct, interface, and class.
 */
class Struct : Templateable {
public:
	/// List of methods
	const Function[] functions;
	/// List of member variables; may be empty
	const Variable[] variables;
	/// List of aliases defined
	const Alias[] aliases;
	/// Source code character position of the beginning of the struct body
	const size_t bodyStart;
	/// Source code character position of the end of the struct body
	const size_t bodyEnd;

	string getMemberType(string name) const pure nothrow {
		foreach (f; functions) {
			if (f.name == name) return f.returnType;
		}
		
		foreach (v; variables) {
			if (v.name == name) return v.type;
		}
		
		return null;
	}
	
	size_t getBitSize() const pure nothrow {
		size_t totalSize;
		foreach (v; variables) {
			totalSize += v.getBitSize();
		}
		
		return totalSize;
	}
}