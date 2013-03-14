module Puzzle.Appender;

debug import std.stdio : writeln;
import std.c.stdlib : malloc, realloc, free;
import std.c.string : memcpy;
import core.memory : GC;

static this() {
	GC.disable();
}

static ~this() {
	GC.enable();
}

size_t max(size_t a, size_t b) pure nothrow {
	return a > b ? a : b;
}

enum Usage {
	Memory,
	Speed
}

enum MinNElems = 500;

struct Appender(T, Usage use = Usage.Speed) {
private:
	T* _ptr;

	size_t _capacity;
	size_t _length;
	
static if (use == Usage.Speed) {
	enum Factor = 2;
} else {
	enum Factor = 10;
}

	void _realloc() {
		assert(this._capacity != 0);
		
		this._capacity += this._capacity / Factor;
		this._ptr = cast(T*) realloc(this._ptr, this._capacity * T.sizeof);
		
		assert(this._ptr !is null);
		debug writeln(" -> ", this._capacity);
	}
	
	void _ensureAddable(size_t nelems) {
		if ((this._length + nelems) > this._capacity) {
			if (this._capacity == 0)
				this._capacity = max(nelems, MinNElems);
			
			this._realloc();
		}
	}

public:
	this(size_t len) {
		this.reserve(len);
	}
	
	~this() {
		free(this._ptr);
	}
	
	void reserve(size_t nelems) {
		this._capacity += nelems;
		this._ptr = cast(T*) realloc(this._ptr, this._capacity * T.sizeof);
	}
	
static if (__traits(compiles, { this._ptr[this._length] = elem; })) {
	void put(U : T)(U elem) {
		this._ensureAddable(1);
		
		this._ptr[this._length++] = elem;
	}
} else {
	void put(U : T)(U elem) {
		this._ensureAddable(1);
		
		memcpy(&this._ptr[this._length++], &elem, T.sizeof);
	}
}
	@property
	size_t length() const pure nothrow {
		return this._length;
	}
	
	@property
	size_t capacity() const pure nothrow {
		return this._capacity;
	}
	
	@property
	inout(T[]) data() inout {
		return this._ptr[0 .. this._length];
	}
}
/+
struct Appender(T) {
private:
	T* _ptr;
	size_t _capacity;
	size_t _length;
	
	void _alloc(size_t reqlen) {
		this._capacity += this._capacity != 0 ? this._capacity / 2 : reqlen * 2;
		this._ptr = cast(T*) realloc(this._ptr, this._capacity * T.sizeof);
	}
	
	void _ensureAddable(size_t nelems) {
		const size_t reqlen = this._length + nelems;
		if (reqlen > this._capacity) {
			this._alloc(reqlen);
		}
	}

public:
	~this() {
		free(this._ptr);
	}
	
	void reserve(size_t nelems) {
		if (this._ptr) {
			this._alloc(nelems);
		} else {
			this._ptr = cast(T*) malloc(nelems * T.sizeof);
		}
	}
	
static if (__traits(compiles, { this._ptr[this._length] = elem; })) {
	void put(U : T)(U elem) {
		this._ensureAddable(1);
		
		this._ptr[this._length++] = elem;
	}
} else {
	void put(U : T)(U elem) {
		this._ensureAddable(1);
		
		memcpy(&this._ptr[this._length++], &elem, T.sizeof);
	}
}
	@property
	size_t length() const pure nothrow {
		return this._length;
	}
	
	@property
	size_t capacity() const pure nothrow {
		return this._capacity;
	}
	
	@property
	inout(T[]) data() inout {
		return this._ptr[0 .. this._length];
	}
}
+/
unittest {
	struct A {
	public:
		const int n;
	}
	
	Appender!(A) as;
	as.put(A(42));
	assert(as.data[0].n == 42);
	as ~= A(23);
	assert(as.data[1].n == 23);
}