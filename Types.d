module Puzzle.Types;

template uniqueId(int x) {
	enum uniqueId = 1 << (x - 1);
}

enum VarAttributes {
	none = uniqueId!(1),
	const_ = uniqueId!(2),
	immutable_ = uniqueId!(3),
	ref_ = uniqueId!(4),
	out_ = uniqueId!(5),
	scope_ = uniqueId!(6),
	lazy_ = uniqueId!(7),
	shared_ = uniqueId!(8),
	wild = uniqueId!(9),
	autoRef = uniqueId!(10)
}

VarAttributes isVarAttribute(string val) pure nothrow {
	switch (val) {
		case "const":
			return VarAttributes.const_;
		case "immutable":
			return VarAttributes.immutable_;
		case "ref":
			return VarAttributes.ref_;
		case "out":
			return VarAttributes.out_;
		case "scope":
			return VarAttributes.scope_;
		case "in":
			return VarAttributes.const_|VarAttributes.scope_;
		case "lazy":
			return VarAttributes.lazy_;
		case "shared":
			return VarAttributes.shared_;
		case "inout":
			return VarAttributes.wild;
		default:
			return VarAttributes.none;
	}
}

string varAttribToStr(VarAttributes vattr) pure nothrow {
	string str;
	
	if (vattr & VarAttributes.const_) {
		str ~= "const ";
	}
	if (vattr & VarAttributes.immutable_) {
		str ~= "immutable ";
	}
	if (vattr & VarAttributes.ref_) {
		str ~= "ref ";
	}
	if (vattr & VarAttributes.out_) {	
		str ~= "out ";
	}
	if (vattr & VarAttributes.scope_) {	
		str ~= "scope ";
	}
	if (vattr & VarAttributes.lazy_) {
		str ~= "lazy ";
	}
	if (vattr & VarAttributes.shared_) {
		str ~= "shared ";
	}
	if (vattr & VarAttributes.wild) {
		str ~= "inout ";
	}
	
	return str;
}

enum FuncAttributes {
	none = uniqueId!(1),
	pure_ = uniqueId!(2),
	nothrow_ = uniqueId!(3),
	const_ = uniqueId!(4),
	immutable_ = uniqueId!(5),
	wild = uniqueId!(6),
	// ref_ = uniqueId!(7),
	// deprecated_ = uniqueId!(8),
	// static_ = uniqueId!(9),
	// extern_ = uniqueId!(10),
	// final_ = uniqueId!(11),
	// synchronized_ = uniqueId!(12),
	// override_ = uniqueId!(13),
	// abstract_ = uniqueId!(14),
	// property = uniqueId!(15),
	// trusted = uniqueId!(16),
	// safe = uniqueId!(17),
	// disable = uniqueId!(18)
}

FuncAttributes isFuncAttribute(string value) pure nothrow {
	switch (value) {
		case "pure":
			return FuncAttributes.pure_;
		case "nothrow":
			return FuncAttributes.nothrow_;
		case "const":
			return FuncAttributes.const_;
		case "immutable":
			return FuncAttributes.immutable_;
		case "inout":
			return FuncAttributes.wild;
		default:
			return FuncAttributes.none;
	}
}

string funcAttribToStr(FuncAttributes fattr) pure nothrow {
	string str;
	
	if (fattr & FuncAttributes.pure_) {
		str ~= "pure ";
	}
	if (fattr & FuncAttributes.nothrow_) {
		str ~= "nothrow ";
	}
	if (fattr & FuncAttributes.const_) {
		str ~= "const ";
	}
	if (fattr & FuncAttributes.immutable_) {
		str ~= "immutable ";
	}
	if (fattr & FuncAttributes.wild) {
		str ~= "inout ";
	}
	
	return str;
}

enum Protection {
	public_  = uniqueId!(1),
	private_ = uniqueId!(2),
	protected_ = uniqueId!(6),
	package_ = uniqueId!(4),
	export_ = uniqueId!(5)
}

/**
 * Attributes common to everything interesting
 */
abstract class Base {
public:
	const string name;
	const size_t line;
	
	size_t useCounter;
	
	// const Protection protection;
	
	this(string name, size_t line) {
		this.name = name;
		this.line = line;
	}
}

/**
 * Varible declaration
 */
final class Variable : Base {
public:
	const string type;
	const bool isPtr;
	const bool isArray;
	/*const */
	int arrayDim = -1;
	VarAttributes vattr;
	string value;
	
	this(string name, size_t line, string type, 
		bool isPtr = false, bool isArray = false)
	{
		super(name, line);
		
		this.type = type;
		this.isPtr = isPtr;
		this.isArray = isArray;
	}
	
	ubyte getBitSize() const pure nothrow {
		if (this.isPtr || this.isArray) return 32;
		
		switch (type) {
			case "uint":
			case "int": return 32;
			case "size_t": 
			version (Win32) {
				return 32;
			} else {
				return 64;
			}
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
			case "wstring":
			case "dstring":
			case "string": return 32; // Also an array
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
final class Alias : Base {
public:
	const string aliasedType;
	
	this(string name, size_t line, string aliasedType) {
		super(name, line);
		this.aliasedType = aliasedType;
	}
}

/**
 * Base class for any type that can be a template
 */
class Template : Base {
public:
	/// Template constraint, which may be null
	const string constraint;
	/// Template parameters, may be empty
	const string[] templateParameters;
	
	this(string name, size_t line, string[] tplParams, string constraint = null) {
		super(name, line);
		this.constraint = constraint;
		this.templateParameters = tplParams;
	}
}

/**
 * Functions and delegates
 */
final class Function : Template {
public:
	/// Function return type
	const string returnType;
	/// Function return type Attributes
	VarAttributes returnVattr;
	/// Parameter list; may be empty
	Variable[] parameters;
	/// Function modifiers
	FuncAttributes fattr;
	
	this(string name, size_t line, string returnType, 
		string[] tplParams = null, string tplConstraint = null)
	{
		super(name, line, tplParams, tplConstraint);
		this.returnType = returnType;
	}

	string documentString() const pure nothrow {
		string r = varAttribToStr(this.returnVattr);
		r ~= this.returnType ~ " " ~ this.name ~ "(";
		
		if (super.templateParameters.length != 0) {
			foreach (i, tplParam; super.templateParameters) {
				r ~= tplParam;
				if (i + 1 < super.templateParameters.length)
					r ~= ", ";
			}
			r ~= ")(";
		}
		
		foreach (i, param; this.parameters) {
			r ~= varAttribToStr(param.vattr);
			r ~= param.type ~ " " ~ param.name;
			if (i + 1 < this.parameters.length)
				r ~= ", ";
		}
		
		r ~= ") ";
		r ~= funcAttribToStr(this.fattr);
		
		return r;
	}
}

/**
 * Stuff common to struct, interface, and class.
 */
final class Struct : Template {
public:
	/// List of methods
	Function[] functions;
	/// List of member variables; may be empty
	Variable[] variables;
	/// List of aliases defined
	Alias[] aliases;
	/// Source code character position of the beginning of the struct body
	size_t bodyStart;
	/// Source code character position of the end of the struct body
	size_t bodyEnd;
	
	this(string name, size_t line, 
		string[] tplParams = null, string tplConstraint = null)
	{
		super(name, line, tplParams, tplConstraint);
	}

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

final class EnumMember {
public:
	const string name;
	const string value;
	
	this(string name, string value) {
		this.name = name;
		this.value = value;
	}
}

final class Enum : Base {
public:
	const string type;
	EnumMember[] member;
	
	this(string name, size_t line, string type) {
		super(name, line);
		this.type = type;
	}
}
	