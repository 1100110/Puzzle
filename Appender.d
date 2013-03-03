module Puzzle.Appender;

import core.memory, core.bitop;
import std.c.string : memcpy, memmove;
import std.traits;
import std.range : isInputRange;
import std.exception : enforce;

struct Appender(A : T[], T) {
private:
	struct Data {
        size_t capacity;
        Unqual!(T)[] arr;
    }

    Data* _data;

public:
	/**
	Construct an appender with a given array.  Note that this does not copy the
	data.  If the array has a larger capacity as determined by arr.capacity,
	it will be used by the appender.  After initializing an appender on an array,
	appending to the original array will reallocate.
	*/
    this(T[] arr) {
        // initialize to a given array.
        _data = new Data();
        _data.arr = cast(Unqual!(T)[])arr;

        if (__ctfe) return;

        // We want to use up as much of the block the array is in as possible.
        // if we consume all the block that we can, then array appending is
        // safe WRT built-in append, and we can use the entire block.
        auto cap = arr.capacity;
        if (cap > arr.length) arr.length = cap;
        // we assume no reallocation occurred
        assert(arr.ptr is _data.arr.ptr);
        _data.capacity = arr.length;
    }

	/**
	Reserve at least newCapacity elements for appending.  Note that more elements
	may be reserved than requested.  If newCapacity < capacity, then nothing is
	done.
	*/
    void reserve(size_t newCapacity) {
        if (!_data) _data = new Data();
        if (_data.capacity < newCapacity) {
            // need to increase capacity
            immutable len = _data.arr.length;
            if (__ctfe) {
                _data.arr.length = newCapacity;
                _data.arr = _data.arr[0..len];
                _data.capacity = newCapacity;
                return;
            }
            immutable growsize = (newCapacity - len) * T.sizeof;
            auto u = GC.extend(_data.arr.ptr, growsize, growsize);
            if (u) {
                // extend worked, update the capacity
                _data.capacity = u / T.sizeof;
            } else {
                // didn't work, must reallocate
                auto bi = GC.qalloc(newCapacity * T.sizeof, (typeid(T[]).next.flags & 1) ? 0 : GC.BlkAttr.NO_SCAN);
                _data.capacity = bi.size / T.sizeof;
                if (len) memcpy(bi.base, _data.arr.ptr, len * T.sizeof);
                _data.arr = (cast(Unqual!(T)*)bi.base)[0..len];
                // leave the old data, for safety reasons
            }
        }
    }
	
	/**
	Returns the capacity of the array (the maximum number of elements the
	managed array can accommodate before triggering a reallocation).  If any
	appending will reallocate, $(D capacity) returns $(D 0).
	*/
    @property
	size_t capacity() const pure nothrow {
        return _data ? _data.capacity : 0;
    }

	/**
	Returns the managed array.
	*/
    @property
	inout(T)[] data() inout {
        return cast(typeof(return))(_data ? _data.arr : null);
    }

private:
    // ensure we can add nelems elements, resizing as necessary
    void ensureAddable(size_t nelems) {
        if (!_data) _data = new Data();
        immutable len = _data.arr.length;
        immutable reqlen = len + nelems;
        if (reqlen > _data.capacity) {
            if (__ctfe) {
                _data.arr.length = reqlen;
                _data.arr = _data.arr[0..len];
                _data.capacity = reqlen;
                return;
            }
            // Time to reallocate.
            // We need to almost duplicate what's in druntime, except we
            // have better access to the capacity field.
            auto newlen = newCapacity(reqlen);
            // first, try extending the current block
            auto u = GC.extend(_data.arr.ptr, nelems * T.sizeof, (newlen - len) * T.sizeof);
            if (u) {
                // extend worked, update the capacity
                _data.capacity = u / T.sizeof;
            } else {
                // didn't work, must reallocate
                auto bi = GC.qalloc(newlen * T.sizeof, (typeid(T[]).next.flags & 1) ? 0 : GC.BlkAttr.NO_SCAN);
                _data.capacity = bi.size / T.sizeof;
                if (len) memcpy(bi.base, _data.arr.ptr, len * T.sizeof);
                _data.arr = (cast(Unqual!(T)*)bi.base)[0..len];
                // leave the old data, for safety reasons
            }
        }
    }

   static size_t newCapacity(size_t newlength) {
        long mult = 100 + (1000L) / (bsr(newlength * T.sizeof) + 1);
        // limit to doubling the length, we don't want to grow too much
        if(mult > 200) mult = 200;
        auto newext = cast(size_t)((newlength * mult + 99) / 100);
        return newext > newlength ? newext : newlength;
    }

    template canPutItem(U) {
        enum bool canPutItem = isImplicitlyConvertible!(U, T) || isSomeChar!T && isSomeChar!U;
    }

public:
	/**
	Appends one item to the managed array.
	*/
    void put(U)(U item) if (canPutItem!U) {
		ensureAddable(1);
		immutable len = _data.arr.length;
		// _data.arr.ptr[len] = cast(Unqual!T) item;
		memmove(&_data.arr.ptr[len], &item, T.sizeof);
		_data.arr = _data.arr.ptr[0 .. len + 1];
    }

	/**
	Appends one item to the managed array.
	 */
    void opOpAssign(string op : "~", U)(U item) if (canPutItem!U) {
        put(item);
	}
}

unittest {
	struct A {
	public:
		const int n;
	}
	
	Appender!(A[]) as;
	as.put(A(42));
	assert(as.data[0].n == 42);
	as ~= A(23);
	assert(as.data[1].n == 23);
}