// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            if(t.x === __updatable) {
                t.x = f();
            } else {
                return f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;
    case 'wheel':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            var mdx = [0,x.deltaX];
            var mdy = [0,x.deltaY];
            var mdz = [0,x.deltaZ];
            B(A(cb,[[0,mx,my],[0,mdx,mdy,mdz],0]));
        };
        break;
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, [0, es[i]], els];
    }
    return els;
}

function jsQuerySelectorAll(elem, query) {
    var els = [0], nl;

    if (!elem || typeof elem.querySelectorAll !== 'function') {
        return els;
    }

    nl = elem.querySelectorAll(query);

    for (var i = nl.length-1; i >= 0; --i) {
        els = [1, [0, nl[i]], els];
    }

    return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, jsRead(obj)];
    case 'string':
        return [1, obj];
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}
window['arr2lst'] = arr2lst;

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}
window['lst2arr'] = lst2arr;

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
    var n = s.length,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=s.length; i+=64) {
        md5cycle(state, md5blk(s.substring(i-64, i)));
    }
    s = s.substring(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<s.length; i++)
        tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s.charCodeAt(i)
            + (s.charCodeAt(i+1) << 8)
            + (s.charCodeAt(i+2) << 16)
            + (s.charCodeAt(i+3) << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s) {
    return hex(md51(s));
}

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return [0, 1, E(w).val];
}

function finalizeWeak(w) {
    return [0, B(A(E(w).fin, [0]))];
}

var _0=function(_1,_2,_){var _3=jsCreateTextNode(toJSStr(E(_1))),_4=_3,_5=jsAppendChild(_4,E(_2)[1]);return [0,_4];},_6=function(_7){var _8=B(A(_7,[_])),_9=_8;return E(_9);},_a=function(_b){return new F(function(){return _6(function(_){var _=0;return new F(function(){return eval(_b);});});});},_c=0,_d=function(_e,_f,_g,_h){return new F(function(){return A(_e,[function(_){var _i=jsSetAttr(E(_f)[1],toJSStr(E(_g)),toJSStr(E(_h)));return _c;}]);});},_j=new T(function(){return B(unCStr("stylesheet"));}),_k=new T(function(){return B(unCStr("rel"));}),_l=new T(function(){return B(unCStr("(function(){return document.head;})"));}),_m=function(_n,_){var _o=B(A(_a,[toJSStr(E(_l)),_])),_p=_o,_q=B(A(_n,[[0,_p],_])),_r=_q;return _c;},_s=new T(function(){return B(unCStr("href"));}),_t=function(_u){return E(_u);},_v=new T(function(){return B(unCStr("link"));}),_w=function(_x,_){var _y=jsCreateElem(toJSStr(E(_v))),_z=_y,_A=jsAppendChild(_z,E(_x)[1]);return [0,_z];},_B=function(_C,_){return new F(function(){return _m(function(_D,_){var _E=B(_w(_D,_)),_F=_E,_G=B(A(_d,[_t,_F,_k,_j,_])),_H=_G,_I=B(A(_d,[_t,_F,_s,_C,_])),_J=_I;return _F;},_);});},_K=function(_L,_){return _L;},_M=new T(function(){return B(unCStr("script"));}),_N=function(_O,_P,_Q,_){var _R=jsCreateElem(toJSStr(E(_M))),_S=_R,_T=jsAppendChild(_S,E(_Q)[1]),_U=[0,_S],_V=B(A(_O,[_P,_U,_])),_W=_V;return _U;},_X=new T(function(){return B(unCStr("src"));}),_Y=function(_Z){return E(_Z);},_10=function(_11,_){return new F(function(){return _m(function(_12,_){var _13=B(_N(_Y,_K,_12,_)),_14=_13,_15=B(A(_d,[_t,_14,_X,_11,_])),_16=_15;return _14;},_);});},_17=new T(function(){return B(unCStr("style"));}),_18=new T(function(){return B(unCStr("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap-theme.min.css"));}),_19=new T(function(){return B(unCStr("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"));}),_1a=2,_1b=new T(function(){return [0,"arr2lst"];}),_1c=function(_1d,_1e){return new F(function(){return _6(function(_){var _=0;return new F(function(){return A(_a,[E(_1b)[1],E(_1d),E(_1e),_]);});});});},_1f=[0],_1g=new T(function(){return B(_a("(function(sel){return document.querySelectorAll(sel);})"));}),_1h=function(_1i,_1j,_1k,_){var _1l=B(A(_1g,[E(toJSStr(E(_1i))),_])),_1m=_1l,_1n=function(_1o,_){var _1p=E(_1o);if(!_1p[0]){return _1f;}else{var _1q=B(A(_1j,[[0,_1p[1]],_])),_1r=_1q,_1s=B(_1n(_1p[2],_)),_1t=_1s;return [1,_1r,_1t];}},_1u=B(_1n(B(_1c(_1m,0)),_)),_1v=_1u;return _1k;},_1w=new T(function(){return B(_a("(function(e,e1){var par=  e.parentNode;par.replaceChild(e1,e);return e1;})"));}),_1x=new T(function(){return B(unCStr("Prelude.undefined"));}),_1y=new T(function(){return B(err(_1x));}),_1z=new T(function(){return B(unCStr("span"));}),_1A=function(_1B,_1C,_1D,_1E,_){var _1F=B(A(_1D,[_1E,_])),_1G=_1F,_1H=E(_1G),_1I=E(_1H[1]),_1J=_1I[1];return [0,[0,function(_1K,_){switch(E(_1C)){case 0:var _1L=B(_1h(_1B,_1J,_1y,_)),_1M=_1L;return _1K;case 1:var _1N=B(_1h(_1B,function(_1O,_){var _1P=E(_1O),_1Q=_1P[1],_1R=jsGetChildren(_1Q),_1S=_1R,_1T=E(_1S);if(!_1T[0]){var _1U=B(A(_1J,[_1P,_])),_1V=_1U;return _1P;}else{var _1W=jsCreateElem(toJSStr(E(_1z))),_1X=_1W,_1Y=jsAddChildBefore(_1X,_1Q,E(_1T[1])[1]),_1Z=B(A(_1J,[[0,_1X],_])),_20=_1Z;return _1P;}},_1y,_)),_21=_1N;return _1K;case 2:var _22=B(_1h(_1B,function(_23,_){var _24=E(_23),_25=jsClearChildren(_24[1]),_26=B(A(_1J,[_24,_])),_27=_26;return _24;},_1y,_)),_28=_22;return _1K;default:var _29=B(_1h(_1B,function(_2a,_){var _2b=B(A(_1J,[_2a,_])),_2c=_2b,_2d=B(A(_1w,[E(E(_2a)[1]),E(E(_2c)[1]),_])),_2e=_2d;return [0,_2e];},_1y,_)),_2f=_29;return _1K;}},_1I[2]],_1H[2]];},_2g=[0],_2h=[0,_1f,_2g],_2i=function(_2j,_2k){var _2l=E(_2j);return _2l[0]==0?E(_2k):[1,_2l[1],new T(function(){return B(_2i(_2l[2],_2k));})];},_2m=function(_2n,_2o){var _2p=jsShowI(_2n),_2q=_2p;return new F(function(){return _2i(fromJSStr(_2q),_2o);});},_2r=[0,41],_2s=[0,40],_2t=function(_2u,_2v,_2w){if(_2v>=0){return new F(function(){return _2m(_2v,_2w);});}else{return _2u<=6?B(_2m(_2v,_2w)):[1,_2s,new T(function(){var _2x=jsShowI(_2v),_2y=_2x;return B(_2i(fromJSStr(_2y),[1,_2r,_2w]));})];}},_2z=[0,112],_2A=function(_2B){var _2C=new T(function(){return E(E(_2B)[2]);});return function(_2D,_){return [0,[1,_2z,new T(function(){return B(_2i(B(_2t(0,E(_2C)[1],_1f)),new T(function(){return E(E(_2B)[1]);},1)));})],new T(function(){var _2E=E(_2B);return [0,_2E[1],new T(function(){return [0,E(_2C)[1]+1|0];}),_2E[3],_2E[4],_2E[5],_2E[6],_2E[7]];})];};},_2F=new T(function(){return B(unCStr("id"));}),_2G=new T(function(){return B(unCStr("noid"));}),_2H=[0,0],_2I=false,_2J=2,_2K=[0],_2L=new T(function(){return B(unCStr("Dynamic"));}),_2M=new T(function(){return B(unCStr("Data.Dynamic"));}),_2N=new T(function(){return B(unCStr("base"));}),_2O=new T(function(){var _2P=hs_wordToWord64(628307645),_2Q=_2P,_2R=hs_wordToWord64(949574464),_2S=_2R;return [0,_2Q,_2S,[0,_2Q,_2S,_2N,_2M,_2L],_1f];}),_2T=new T(function(){return B(unCStr("Haste.HPlay.View"));}),_2U=new T(function(){return B(unCStr("hplayground-0.1.3"));}),_2V=new T(function(){return B(unCStr("EventData"));}),_2W=new T(function(){var _2X=hs_wordToWord64(688714137),_2Y=_2X,_2Z=hs_wordToWord64(2885644110),_30=_2Z;return [0,_2Y,_30,[0,_2Y,_30,_2U,_2T,_2V],_1f];}),_31=[0],_32=new T(function(){return B(unCStr("OnLoad"));}),_33=[0,_32,_31],_34=[0,_2W,_33],_35=[0,_2O,_34],_36=function(_){return _2g;},_37=function(_38,_){return _2g;},_39=[0,_36,_37],_3a=[0,_1f,_2H,_2J,_39,_2I,_35,_2K],_3b=function(_){var _=0,_3c=newMVar(),_3d=_3c,_=putMVar(_3d,_3a);return [0,_3d];},_3e=new T(function(){return B(_6(_3b));}),_3f=function(_3g,_3h,_){var _3i=jsFind(toJSStr(E(_3h))),_3j=_3i,_3k=E(_3j);if(!_3k[0]){var _3l=E(_3e)[1],_3m=takeMVar(_3l),_3n=_3m,_3o=B(A(_3g,[_3n,_])),_3p=_3o,_3q=E(_3p),_=putMVar(_3l,_3q[2]);return E(_3q[1])[2];}else{var _3r=E(_3k[1]),_3s=jsClearChildren(_3r[1]),_3t=E(_3e)[1],_3u=takeMVar(_3t),_3v=_3u,_3w=B(A(_3g,[_3v,_])),_3x=_3w,_3y=E(_3x),_3z=E(_3y[1]),_=putMVar(_3t,_3y[2]),_3A=B(A(_3z[1],[_3r,_])),_3B=_3A;return _3z[2];}},_3C=new T(function(){return B(unCStr("span"));}),_3D=function(_3E,_3F,_3G,_){var _3H=jsCreateElem(toJSStr(E(_3C))),_3I=_3H,_3J=jsAppendChild(_3I,E(_3G)[1]),_3K=[0,_3I],_3L=B(A(_3E,[_3F,_3K,_])),_3M=_3L;return _3K;},_3N=function(_3O,_3P,_3Q,_){var _3R=B(A(_2A,[_3Q,_3Q,_])),_3S=_3R,_3T=E(_3S),_3U=_3T[1],_3V=E(_3T[2]),_3W=_3V[2],_3X=E(_3V[4]),_3Y=B(A(_3O,[[0,_3V[1],_3W,_3V[3],[0,function(_){return new F(function(){return _3f(function(_3Z,_){var _40=B(A(_3O,[new T(function(){var _41=E(_3Z);return [0,_41[1],_3W,_41[3],_41[4],_41[5],_41[6],_41[7]];}),_])),_42=_40;return [0,[0,_K,E(E(_42)[1])[2]],_3Z];},_2G,_);});},function(_43,_){var _44=B(_3f(new T(function(){return B(A(_3P,[_43]));},1),_3U,_)),_45=_44,_46=E(_45);return _46[0]==0?_2g:B(A(_3X[2],[_46[1],_]));}],_3V[5],_3V[6],_3V[7]],_])),_47=_3Y,_48=E(_47),_49=_48[2],_4a=E(_48[1]),_4b=_4a[1],_4c=E(_4a[2]);if(!_4c[0]){return [0,[0,function(_4d,_){var _4e=B(A(_4b,[_4d,_])),_4f=_4e;if(!E(E(_3Q)[5])){var _4g=B(_3D(_Y,_K,_4d,_)),_4h=_4g,_4i=B(A(_d,[_t,_4h,_2F,_3U,_])),_4j=_4i;return _4d;}else{return _4d;}},_2g],new T(function(){var _4k=E(_49);return [0,_4k[1],_4k[2],_4k[3],_3X,_4k[5],_4k[6],_4k[7]];})];}else{var _4l=B(A(_3P,[_4c[1],new T(function(){var _4m=E(_49);return [0,_4m[1],_4m[2],_4m[3],_3X,_4m[5],_4m[6],_4m[7]];}),_])),_4n=_4l,_4o=E(_4n),_4p=E(_4o[1]),_4q=_4p[1];return [0,[0,function(_4r,_){var _4s=B(A(_4b,[_4r,_])),_4t=_4s;if(!E(E(_3Q)[5])){var _4u=B(_3D(_Y,_4q,_4r,_)),_4v=_4u,_4w=B(A(_d,[_t,_4v,_2F,_3U,_])),_4x=_4w;return _4r;}else{var _4y=B(A(_4q,[_4r,_])),_4z=_4y;return _4r;}},_4p[2]],_4o[2]];}},_4A=function(_4B,_4C,_){return [0,[0,_K,[1,[1,_4B]]],_4C];},_4D=function(_4E,_4F,_){return [0,[0,_K,[1,[0,_4E]]],_4F];},_4G=function(_4H,_4I,_4J,_){var _4K=B(_3N(_4H,_4D,_4J,_)),_4L=_4K,_4M=E(_4L),_4N=E(_4M[1]),_4O=B(_3N(_4I,_4A,_4M[2],_)),_4P=_4O,_4Q=E(_4P),_4R=E(_4Q[1]);return [0,[0,function(_4S,_){var _4T=B(A(_4N[1],[_4S,_])),_4U=_4T,_4V=B(A(_4R[1],[_4S,_])),_4W=_4V;return _4S;},new T(function(){var _4X=E(_4N[2]);return _4X[0]==0?E(_4R[2]):E(_4X);})],_4Q[2]];},_4Y=function(_4Z,_50,_51,_){var _52=E(_50),_53=B(A(_4Z,[_51,_])),_54=_53,_55=B(A(_d,[_t,_54,_52[1],_52[2],_])),_56=_55;return _54;},_57=function(_58,_59){while(1){var _5a=(function(_5b,_5c){var _5d=E(_5c);if(!_5d[0]){return E(_5b);}else{_58=function(_5e,_){return new F(function(){return _4Y(_5b,_5d[1],_5e,_);});};_59=_5d[2];return null;}})(_58,_59);if(_5a!=null){return _5a;}}},_5f=new T(function(){return B(unCStr("div"));}),_5g=function(_5h,_5i,_5j,_){var _5k=jsCreateElem(toJSStr(E(_5f))),_5l=_5k,_5m=jsAppendChild(_5l,E(_5j)[1]),_5n=[0,_5l],_5o=B(A(_5h,[_5i,_5n,_])),_5p=_5o;return _5n;},_5q=new T(function(){return B(unCStr("class"));}),_5r=new T(function(){return B(unCStr("row"));}),_5s=new T(function(){return B(unCStr("col-md-4 col-md-offset-5"));}),_5t=function(_,_5u,_5v,_5w){return [0,[0,function(_5x,_){var _5y=B(_5g(_Y,function(_5z,_){var _5A=B(_5g(_Y,_5u,_5z,_)),_5B=_5A,_5C=B(A(_d,[_t,_5B,_5q,_5s,_])),_5D=_5C;return _5B;},_5x,_)),_5E=_5y,_5F=B(A(_d,[_t,_5E,_5q,_5r,_])),_5G=_5F;return _5E;},_5v],_5w];},_5H=1,_5I=[8,_],_5J=new T(function(){return B(unCStr("button"));}),_5K=function(_5L,_5M,_5N,_){var _5O=jsCreateElem(toJSStr(E(_5J))),_5P=_5O,_5Q=jsAppendChild(_5P,E(_5N)[1]),_5R=[0,_5P],_5S=B(A(_5L,[_5M,_5R,_])),_5T=_5S;return _5R;},_5U=new T(function(){return B(unCStr("margin-right: 10px; margin-left: 10px; margin-top: 3px"));}),_5V=new T(function(){return B(unCStr("button"));}),_5W=new T(function(){return B(unCStr("type"));}),_5X=new T(function(){return B(unCStr("btn btn-primary"));}),_5Y=new T(function(){return B(unCStr("class"));}),_5Z=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_60=new T(function(){return B(err(_5Z));}),_61=function(_62,_63,_64,_){var _65=B(A(_62,[_64,_])),_66=_65,_67=E(_66),_68=E(_67[1]);return [0,[0,function(_69,_){var _6a=B(A(_68[1],[_69,_])),_6b=_6a,_6c=jsFind(toJSStr(E(_63))),_6d=_6c;return new T(function(){var _6e=E(_6d);return _6e[0]==0?E(_60):E(_6e[1]);});},_68[2]],_67[2]];},_6f=new T(function(){return B(unCStr("wheel"));}),_6g=new T(function(){return B(unCStr("mouseout"));}),_6h=new T(function(){return B(unCStr("mouseover"));}),_6i=new T(function(){return B(unCStr("mousemove"));}),_6j=new T(function(){return B(unCStr("blur"));}),_6k=new T(function(){return B(unCStr("focus"));}),_6l=new T(function(){return B(unCStr("change"));}),_6m=new T(function(){return B(unCStr("unload"));}),_6n=new T(function(){return B(unCStr("load"));}),_6o=new T(function(){return B(unCStr("submit"));}),_6p=new T(function(){return B(unCStr("keydown"));}),_6q=new T(function(){return B(unCStr("keyup"));}),_6r=new T(function(){return B(unCStr("keypress"));}),_6s=new T(function(){return B(unCStr("mouseup"));}),_6t=new T(function(){return B(unCStr("mousedown"));}),_6u=new T(function(){return B(unCStr("dblclick"));}),_6v=new T(function(){return B(unCStr("click"));}),_6w=function(_6x){switch(E(_6x)[0]){case 0:return E(_6n);case 1:return E(_6m);case 2:return E(_6l);case 3:return E(_6k);case 4:return E(_6j);case 5:return E(_6i);case 6:return E(_6h);case 7:return E(_6g);case 8:return E(_6v);case 9:return E(_6u);case 10:return E(_6t);case 11:return E(_6s);case 12:return E(_6r);case 13:return E(_6q);case 14:return E(_6p);case 15:return E(_6o);default:return E(_6f);}},_6y=new T(function(){return B(unCStr("Control.Exception.Base"));}),_6z=new T(function(){return B(unCStr("base"));}),_6A=new T(function(){return B(unCStr("PatternMatchFail"));}),_6B=new T(function(){var _6C=hs_wordToWord64(18445595),_6D=_6C,_6E=hs_wordToWord64(52003073),_6F=_6E;return [0,_6D,_6F,[0,_6D,_6F,_6z,_6y,_6A],_1f];}),_6G=function(_6H){return E(_6B);},_6I=function(_6J){return E(E(_6J)[1]);},_6K=function(_6L,_6M,_6N){var _6O=B(A(_6L,[_])),_6P=B(A(_6M,[_])),_6Q=hs_eqWord64(_6O[1],_6P[1]),_6R=_6Q;if(!E(_6R)){return [0];}else{var _6S=hs_eqWord64(_6O[2],_6P[2]),_6T=_6S;return E(_6T)==0?[0]:[1,_6N];}},_6U=function(_6V){var _6W=E(_6V);return new F(function(){return _6K(B(_6I(_6W[1])),_6G,_6W[2]);});},_6X=function(_6Y){return E(E(_6Y)[1]);},_6Z=function(_70,_71){return new F(function(){return _2i(E(_70)[1],_71);});},_72=[0,44],_73=[0,93],_74=[0,91],_75=function(_76,_77,_78){var _79=E(_77);return _79[0]==0?B(unAppCStr("[]",_78)):[1,_74,new T(function(){return B(A(_76,[_79[1],new T(function(){var _7a=function(_7b){var _7c=E(_7b);return _7c[0]==0?E([1,_73,_78]):[1,_72,new T(function(){return B(A(_76,[_7c[1],new T(function(){return B(_7a(_7c[2]));})]));})];};return B(_7a(_79[2]));})]));})];},_7d=function(_7e,_7f){return new F(function(){return _75(_6Z,_7e,_7f);});},_7g=function(_7h,_7i,_7j){return new F(function(){return _2i(E(_7i)[1],_7j);});},_7k=[0,_7g,_6X,_7d],_7l=new T(function(){return [0,_6G,_7k,_7m,_6U];}),_7m=function(_7n){return [0,_7l,_7n];},_7o=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_7p=function(_7q,_7r){return new F(function(){return die(new T(function(){return B(A(_7r,[_7q]));}));});},_7s=function(_7t,_7u){var _7v=E(_7u);if(!_7v[0]){return [0,_1f,_1f];}else{var _7w=_7v[1];if(!B(A(_7t,[_7w]))){return [0,_1f,_7v];}else{var _7x=new T(function(){var _7y=B(_7s(_7t,_7v[2]));return [0,_7y[1],_7y[2]];});return [0,[1,_7w,new T(function(){return E(E(_7x)[1]);})],new T(function(){return E(E(_7x)[2]);})];}}},_7z=[0,32],_7A=[0,10],_7B=[1,_7A,_1f],_7C=function(_7D){return E(E(_7D)[1])==124?false:true;},_7E=function(_7F,_7G){var _7H=B(_7s(_7C,B(unCStr(_7F)))),_7I=_7H[1],_7J=function(_7K,_7L){return new F(function(){return _2i(_7K,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_2i(_7G,new T(function(){return B(_2i(_7L,_7B));},1)));})));},1));});},_7M=E(_7H[2]);if(!_7M[0]){return new F(function(){return _7J(_7I,_1f);});}else{return E(E(_7M[1])[1])==124?B(_7J(_7I,[1,_7z,_7M[2]])):B(_7J(_7I,_1f));}},_7N=function(_7O){return new F(function(){return _7p([0,new T(function(){return B(_7E(_7O,_7o));})],_7m);});},_7P=new T(function(){return B(_7N("src/Haste/HPlay/View.hs:(1065,9)-(1099,63)|case"));}),_7Q=[0,_6n,_31],_7R=[0,_2W,_7Q],_7S=[0,_6m,_31],_7T=[0,_2W,_7S],_7U=[0,_6l,_31],_7V=[0,_2W,_7U],_7W=[0,_6k,_31],_7X=[0,_2W,_7W],_7Y=[0,_6j,_31],_7Z=[0,_2W,_7Y],_80=[3],_81=[0,_6g,_80],_82=[0,_2W,_81],_83=function(_84,_85){switch(E(_84)[0]){case 0:return function(_){var _86=E(_3e)[1],_87=takeMVar(_86),_88=_87,_=putMVar(_86,new T(function(){var _89=E(_88);return [0,_89[1],_89[2],_89[3],_89[4],_89[5],_7R,_89[7]];}));return new F(function(){return A(_85,[_]);});};case 1:return function(_){var _8a=E(_3e)[1],_8b=takeMVar(_8a),_8c=_8b,_=putMVar(_8a,new T(function(){var _8d=E(_8c);return [0,_8d[1],_8d[2],_8d[3],_8d[4],_8d[5],_7T,_8d[7]];}));return new F(function(){return A(_85,[_]);});};case 2:return function(_){var _8e=E(_3e)[1],_8f=takeMVar(_8e),_8g=_8f,_=putMVar(_8e,new T(function(){var _8h=E(_8g);return [0,_8h[1],_8h[2],_8h[3],_8h[4],_8h[5],_7V,_8h[7]];}));return new F(function(){return A(_85,[_]);});};case 3:return function(_){var _8i=E(_3e)[1],_8j=takeMVar(_8i),_8k=_8j,_=putMVar(_8i,new T(function(){var _8l=E(_8k);return [0,_8l[1],_8l[2],_8l[3],_8l[4],_8l[5],_7X,_8l[7]];}));return new F(function(){return A(_85,[_]);});};case 4:return function(_){var _8m=E(_3e)[1],_8n=takeMVar(_8m),_8o=_8n,_=putMVar(_8m,new T(function(){var _8p=E(_8o);return [0,_8p[1],_8p[2],_8p[3],_8p[4],_8p[5],_7Z,_8p[7]];}));return new F(function(){return A(_85,[_]);});};case 5:return function(_8q){return function(_){var _8r=E(_3e)[1],_8s=takeMVar(_8r),_8t=_8s,_=putMVar(_8r,new T(function(){var _8u=E(_8t);return [0,_8u[1],_8u[2],_8u[3],_8u[4],_8u[5],[0,_2W,[0,_6i,[2,E(_8q)]]],_8u[7]];}));return new F(function(){return A(_85,[_]);});};};case 6:return function(_8v){return function(_){var _8w=E(_3e)[1],_8x=takeMVar(_8w),_8y=_8x,_=putMVar(_8w,new T(function(){var _8z=E(_8y);return [0,_8z[1],_8z[2],_8z[3],_8z[4],_8z[5],[0,_2W,[0,_6h,[2,E(_8v)]]],_8z[7]];}));return new F(function(){return A(_85,[_]);});};};case 7:return function(_){var _8A=E(_3e)[1],_8B=takeMVar(_8A),_8C=_8B,_=putMVar(_8A,new T(function(){var _8D=E(_8C);return [0,_8D[1],_8D[2],_8D[3],_8D[4],_8D[5],_82,_8D[7]];}));return new F(function(){return A(_85,[_]);});};case 8:return function(_8E,_8F){return function(_){var _8G=E(_3e)[1],_8H=takeMVar(_8G),_8I=_8H,_=putMVar(_8G,new T(function(){var _8J=E(_8I);return [0,_8J[1],_8J[2],_8J[3],_8J[4],_8J[5],[0,_2W,[0,_6v,[1,_8E,E(_8F)]]],_8J[7]];}));return new F(function(){return A(_85,[_]);});};};case 9:return function(_8K,_8L){return function(_){var _8M=E(_3e)[1],_8N=takeMVar(_8M),_8O=_8N,_=putMVar(_8M,new T(function(){var _8P=E(_8O);return [0,_8P[1],_8P[2],_8P[3],_8P[4],_8P[5],[0,_2W,[0,_6u,[1,_8K,E(_8L)]]],_8P[7]];}));return new F(function(){return A(_85,[_]);});};};case 10:return function(_8Q,_8R){return function(_){var _8S=E(_3e)[1],_8T=takeMVar(_8S),_8U=_8T,_=putMVar(_8S,new T(function(){var _8V=E(_8U);return [0,_8V[1],_8V[2],_8V[3],_8V[4],_8V[5],[0,_2W,[0,_6t,[1,_8Q,E(_8R)]]],_8V[7]];}));return new F(function(){return A(_85,[_]);});};};case 11:return function(_8W,_8X){return function(_){var _8Y=E(_3e)[1],_8Z=takeMVar(_8Y),_90=_8Z,_=putMVar(_8Y,new T(function(){var _91=E(_90);return [0,_91[1],_91[2],_91[3],_91[4],_91[5],[0,_2W,[0,_6s,[1,_8W,E(_8X)]]],_91[7]];}));return new F(function(){return A(_85,[_]);});};};case 12:return function(_92,_){var _93=E(_3e)[1],_94=takeMVar(_93),_95=_94,_=putMVar(_93,new T(function(){var _96=E(_95);return [0,_96[1],_96[2],_96[3],_96[4],_96[5],[0,_2W,[0,_6r,[4,_92]]],_96[7]];}));return new F(function(){return A(_85,[_]);});};case 13:return function(_97,_){var _98=E(_3e)[1],_99=takeMVar(_98),_9a=_99,_=putMVar(_98,new T(function(){var _9b=E(_9a);return [0,_9b[1],_9b[2],_9b[3],_9b[4],_9b[5],[0,_2W,[0,_6q,[4,_97]]],_9b[7]];}));return new F(function(){return A(_85,[_]);});};case 14:return function(_9c,_){var _9d=E(_3e)[1],_9e=takeMVar(_9d),_9f=_9e,_=putMVar(_9d,new T(function(){var _9g=E(_9f);return [0,_9g[1],_9g[2],_9g[3],_9g[4],_9g[5],[0,_2W,[0,_6p,[4,_9c]]],_9g[7]];}));return new F(function(){return A(_85,[_]);});};default:return E(_7P);}},_9h=[0,_6w,_83],_9i=function(_9j,_9k){while(1){var _9l=E(_9j);if(!_9l[0]){return E(_9k)[0]==0?true:false;}else{var _9m=E(_9k);if(!_9m[0]){return false;}else{if(E(_9l[1])[1]!=E(_9m[1])[1]){return false;}else{_9j=_9l[2];_9k=_9m[2];continue;}}}}},_9n=[1,_c],_9o=function(_9p){return E(_2W);},_9q=function(_9r,_9s,_9t,_9u){var _9v=B(A(_9r,[_])),_9w=hs_eqWord64(_9s,_9v[1]),_9x=_9w;if(!E(_9x)){return [0];}else{var _9y=hs_eqWord64(_9t,_9v[2]),_9z=_9y;return E(_9z)==0?[0]:[1,_9u];}},_9A=function(_9B,_){return [0,[0,_K,new T(function(){var _9C=E(E(_9B)[6]),_9D=E(_9C[1]);return B(_9q(_9o,_9D[1],_9D[2],_9C[2]));})],_9B];},_9E=function(_9F,_){return [0,[0,_K,[1,_9F]],_9F];},_9G=[0,_K,_9n],_9H=new T(function(){return B(unCStr("Onload"));}),_9I=[0,_9H,_31],_9J=[0,_2W,_9I],_9K=function(_9L,_9M,_){return [0,_9G,new T(function(){var _9N=E(_9L);return [0,_9N[1],_9N[2],_9N[3],_9N[4],_9N[5],_9J,_9N[7]];})];},_9O=function(_5e,_){return new F(function(){return _3N(_9E,_9K,_5e,_);});},_9P=[0,_K,_2g],_9Q=true,_9R=function(_9S,_9T,_){var _9U=B(A(_9S,[new T(function(){var _9V=E(_9T);return [0,_9V[1],_9V[2],_9V[3],_9V[4],_9Q,_9V[6],_9V[7]];}),_])),_9W=_9U;return [0,new T(function(){return E(E(_9W)[1]);}),new T(function(){var _9X=E(E(_9W)[2]);return [0,_9X[1],_9X[2],_9X[3],_9X[4],new T(function(){return E(E(_9T)[5]);}),_9X[6],_9X[7]];})];},_9Y=function(_9Z){return E(E(_9Z)[2]);},_a0=function(_a1){return E(E(_a1)[1]);},_a2=function(_a3,_a4,_a5){return function(_a6,_){var _a7=B(A(_a4,[_a6,_])),_a8=_a7,_a9=E(_a8),_aa=E(_a9[1]);return [0,[0,function(_ab,_){var _ac=B(A(_aa[1],[_ab,_])),_ad=_ac,_ae=E(_ad),_af=jsSetCB(_ae[1],E(new T(function(){return [0,toJSStr(B(A(_a0,[_a3,_a5])))];}))[1],E(new T(function(){return B(A(new T(function(){return B(_9Y(_a3));}),[_a5,function(_){var _ag=E(E(_a6)[4]),_ah=B(A(_ag[1],[_])),_ai=_ah,_aj=E(_ai);if(!_aj[0]){return _c;}else{var _ak=B(A(_ag[2],[_aj[1],_])),_al=_ak;return _c;}}]));}))),_am=_af;return _ae;},_aa[2]],_a9[2]];};},_an=function(_ao,_ap){return function(_aq,_ar){return new F(function(){return _9R(function(_5e,_){return new F(function(){return _3N(_9O,function(_as,_5e,_){return new F(function(){return (function(_5e,_){return new F(function(){return _3N(new T(function(){return B(_a2(_9h,function(_at,_){return [0,[0,_ao,_9n],_at];},_ap));}),function(_au){return function(_5e,_){return new F(function(){return _3N(_9A,function(_av){var _aw=E(_av);return new F(function(){return (function(_ax,_ay){return function(_az,_){return !E(new T(function(){return B(_9i(new T(function(){return B(_6w(_ap));}),_ax));}))?[0,_9P,_az]:[0,[0,_K,[1,[0,_ax,_ay]]],_az];};})(_aw[1],_aw[2]);});},_5e,_);});};},_5e,_);});})(_5e,_);});},_5e,_);});},_aq,_ar);});};},_aA=function(_aB,_aC){return function(_aq,_ar){return new F(function(){return _9R(function(_aD,_){return new F(function(){return _61(function(_aE,_){return new F(function(){return _3N(new T(function(){return B(_an(function(_aF,_){var _aG=B(_5K(_0,_aC,_aF,_)),_aH=_aG,_aI=B(A(_d,[_t,_aH,_2F,_aC,_])),_aJ=_aI,_aK=B(A(_d,[_t,_aH,_5Y,_5X,_])),_aL=_aK,_aM=B(A(_d,[_t,_aH,_5W,_5V,_])),_aN=_aM,_aO=B(A(_d,[_t,_aH,_17,_5U,_])),_aP=_aO;return _aH;},_5I));}),function(_aQ,_aR,_){return [0,[0,_K,[1,_aB]],_aR];},_aE,_);});},_aC,_aD,_);});},_aq,_ar);});};},_aS=new T(function(){return B(unCStr("\u0421\u043e\u0437\u0434\u0430\u0442\u044c \u0440\u0430\u0441\u043a\u043b\u0430\u0434\u043a\u0443"));}),_aT=new T(function(){return B(_aA(_5H,_aS));}),_aU=0,_aV=new T(function(){return B(unCStr("\u041d\u0430\u0437\u0430\u0434"));}),_aW=new T(function(){return B(_aA(_aU,_aV));}),_aX=2,_aY=new T(function(){return B(unCStr("\u041e\u0442\u043a\u0440\u044b\u0442\u044c \u0440\u0430\u0441\u043a\u043b\u0430\u0434\u043a\u0443"));}),_aZ=new T(function(){return B(_aA(_aX,_aY));}),_b0=new T(function(){return B(unCStr("btn btn-primary btn-lg"));}),_b1=[0,_5q,_b0],_b2=[1,_b1,_1f],_b3=function(_b4,_b5,_){var _b6=E(_b4);if(!_b6[0]){if(!E(_b6[2])[0]){var _b7=B(A(_aT,[_b5,_])),_b8=_b7,_b9=E(_b8),_ba=E(_b9[1]);return new F(function(){return _5t(_,new T(function(){return B(_57(_ba[1],_b2));}),_ba[2],_b9[2]);});}else{var _bb=B(A(_aZ,[_b5,_])),_bc=_bb,_bd=E(_bc),_be=E(_bd[1]),_bf=B(A(_aT,[_bd[2],_])),_bg=_bf,_bh=E(_bg),_bi=E(_bh[1]);return new F(function(){return _5t(_,function(_bj,_){var _bk=B(A(new T(function(){return B(_57(_be[1],_b2));}),[_bj,_])),_bl=_bk,_bm=B(A(new T(function(){return B(_57(_bi[1],_b2));}),[_bj,_])),_bn=_bm;return _bj;},new T(function(){var _bo=E(_be[2]);return _bo[0]==0?E(_bi[2]):E(_bo);}),_bh[2]);});}}else{var _bp=B(A(_aW,[_b5,_])),_bq=_bp,_br=E(_bq),_bs=E(_br[1]);return new F(function(){return _5t(_,new T(function(){return B(_57(_bs[1],_b2));}),_bs[2],_br[2]);});}},_bt=[0,_K,_2g],_bu=function(_bv,_){return [0,_bt,_bv];},_bw=function(_bx,_by,_bz,_){return new F(function(){return _3N(_bu,function(_bA,_bB,_){return [0,[0,_K,[1,[0,_bx,_by]]],_bB];},_bz,_);});},_bC=new T(function(){return B(_7N("src/Ration/Application.hs:(27,5)-(33,80)|function go"));}),_bD=[1,_2g],_bE=[1,_bD],_bF=[0,_K,_bE],_bG=new T(function(){return B(unCStr("color:red"));}),_bH=new T(function(){return B(unCStr("style"));}),_bI=[0,_bH,_bG],_bJ=[1,_bI,_1f],_bK=[0,98],_bL=[1,_bK,_1f],_bM=function(_bN,_bO,_){var _bP=jsCreateElem(toJSStr(E(_bN))),_bQ=_bP,_bR=jsAppendChild(_bQ,E(_bO)[1]);return [0,_bQ];},_bS=function(_bT){return new F(function(){return _57(function(_bU,_){var _bV=B(_bM(_bL,_bU,_)),_bW=_bV,_bX=B(A(_bT,[_bW,_])),_bY=_bX;return _bW;},_bJ);});},_bZ=new T(function(){return B(unCStr("RouteLoad"));}),_c0=new T(function(){return B(unAppCStr("invalid route in config state ",_bZ));}),_c1=function(_c2,_){return new F(function(){return _0(_c0,_c2,_);});},_c3=new T(function(){return B(_bS(_c1));}),_c4=[0,_c3,_2g],_c5=function(_c6,_c7,_){var _c8=E(_c6);if(!_c8[0]){var _c9=_c8[2];return new F(function(){return _3N(function(_c2,_){return new F(function(){return _4G(function(_c2,_){return new F(function(){return _bw(_c8[1],_c9,_c2,_);});},function(_c2,_){return new F(function(){return _b3(_c8,_c2,_);});},_c2,_);});},function(_ca,_cb,_){var _cc=E(_ca);if(!_cc[0]){var _cd=E(_cc[1]);return [0,[0,_K,[1,[0,_cd[1],_cd[2]]]],_cb];}else{switch(E(_cc[1])){case 0:return [0,_c4,_cb];case 1:return [0,_bF,_cb];default:return [0,[0,_K,[1,[1,_c9]]],_cb];}}},_c7,_);});}else{return E(_bC);}},_ce=[0,35],_cf=new T(function(){return B(unCStr("id"));}),_cg=function(_ch,_ci,_cj,_){var _ck=B(A(_2A,[_cj,_cj,_])),_cl=_ck,_cm=new T(function(){return E(E(_cl)[1]);}),_cn=function(_co,_cp){return function(_aq,_ar){return new F(function(){return _3N(function(_cq,_){return new F(function(){return _1A([1,_ce,_co],_1a,new T(function(){return B(A(_ci,[_cp]));}),_cq,_);});},function(_cr){return new F(function(){return _cn(_co,_cr);});},_aq,_ar);});};},_cs=B(A(_cn,[_cm,_ch,new T(function(){return E(E(_cl)[2]);}),_])),_ct=_cs,_cu=E(_ct),_cv=E(_cu[1]);return [0,[0,function(_cw,_){var _cx=B(_3D(_Y,_K,_cw,_)),_cy=_cx,_cz=B(A(_d,[_t,_cy,_cf,_cm,_])),_cA=_cz,_cB=B(A(_cv[1],[_cw,_])),_cC=_cB;return _cw;},_cv[2]],_cu[2]];},_cD=function(_cE,_){return new F(function(){return _cg(_2h,_c5,_cE,_);});},_cF=[0,1000],_cG=function(_cH,_cI){while(1){var _cJ=E(_cH);if(!_cJ[0]){return E(_cI)[0]==0?true:false;}else{var _cK=E(_cI);if(!_cK[0]){return false;}else{if(E(_cJ[1])[1]!=E(_cK[1])[1]){return false;}else{_cH=_cJ[2];_cI=_cK[2];continue;}}}}},_cL=function(_cM,_cN){return !B(_cG(_cM,_cN))?true:false;},_cO=[0,_cG,_cL],_cP=function(_cQ,_){return [0,[0,_K,[1,_cQ]],_cQ];},_cR=function(_cS,_cT,_){return [0,[0,_K,[1,new T(function(){return E(E(_cS)[4]);})]],_cT];},_cU=function(_aD,_){return new F(function(){return _3N(_cP,_cR,_aD,_);});},_cV=function(_){var _=0,_cW=nMV(_1f),_cX=_cW;return [0,_cX];},_cY=new T(function(){return B(_6(_cV));}),_cZ=function(_d0,_){var _d1=rMV(E(_cY)[1]),_d2=_d1;return [0,[0,_K,[1,_d2]],_d0];},_d3=[1,_c],_d4=[0,_K,_d3],_d5=function(_d6,_){return [0,_d4,_d6];},_d7=[0,_K,_2g],_d8=function(_d9,_){return [0,_d7,_d9];},_da=function(_db){return E(E(_db)[1]);},_dc=function(_dd,_de,_df){while(1){var _dg=E(_df);if(!_dg[0]){return false;}else{if(!B(A(_da,[_dd,_de,_dg[1]]))){_df=_dg[2];continue;}else{return true;}}}},_dh=function(_di,_dj){while(1){var _dk=(function(_dl,_dm){var _dn=E(_dm);if(!_dn[0]){return [0];}else{var _do=_dn[1],_dp=_dn[2];if(!B(A(_dl,[_do]))){var _dq=_dl;_dj=_dp;_di=_dq;return null;}else{return [1,_do,new T(function(){return B(_dh(_dl,_dp));})];}}})(_di,_dj);if(_dk!=null){return _dk;}}},_dr=function(_ds){var _dt=new T(function(){return E(E(_ds)[2]);});return function(_aq,_ar){return new F(function(){return _3N(function(_du,_){return [0,_9G,new T(function(){var _dv=E(_ds);return [0,_dv[1],new T(function(){return [0,E(_dt)[1]+1|0];}),_dv[3],_dv[4],_dv[5],_dv[6],_dv[7]];})];},function(_dw,_dx,_){return [0,[0,_K,[1,[1,_2z,new T(function(){return B(_2i(B(_2t(0,E(_dt)[1],_1f)),new T(function(){return E(E(_ds)[1]);},1)));})]]],_dx];},_aq,_ar);});};},_dy=function(_5e,_){return new F(function(){return _3N(_9E,_dr,_5e,_);});},_dz=function(_dA,_dB,_dC,_){return new F(function(){return _3N(_dy,function(_dD,_dE,_){return new F(function(){return _3N(function(_dF,_){var _dG=E(_cY)[1],_dH=rMV(_dG),_dI=_dH,_=wMV(_dG,[1,_dD,_dI]);return [0,_d4,_dF];},function(_dJ,_aD,_){return new F(function(){return (function(_aD,_){return new F(function(){return _3N(_cU,function(_dK,_dL,_){return new F(function(){return _3N(function(_dM,_){var _dN=rMV(E(_cY)[1]),_dO=_dN;return [0,[0,_K,[1,new T(function(){return B(_dc(_cO,_dD,_dO));})]],_dM];},function(_dP,_dQ,_){return new F(function(){return _3N(new T(function(){return !E(_dP)?E(_d5):function(_dR,_){var _dS=jsSetTimeout(E(_dA)[1],function(_){var _dT=E(_cY)[1],_dU=rMV(_dT),_dV=_dU;if(!B(_dc(_cO,_dD,_dV))){return _c;}else{var _dW=rMV(_dT),_dX=_dW,_=wMV(_dT,new T(function(){return B(_dh(function(_dY){return new F(function(){return _cL(_dY,_dD);});},_dX));})),_dZ=E(_dK),_e0=B(A(_dZ[1],[_])),_e1=_e0,_e2=E(_e1);if(!_e2[0]){return _c;}else{var _e3=B(A(_dZ[2],[_e2[1],_])),_e4=_e3;return _c;}}});return [0,_d4,_dR];};}),function(_e5,_e6,_){return new F(function(){return _3N(function(_aD,_){return new F(function(){return _3N(_cZ,function(_e7){return !B(_dc(_cO,_dD,_e7))?E(_d5):E(_d8);},_aD,_);});},function(_e8){return E(_dB);},_e6,_);});},_dQ,_);});},_dL,_);});},_aD,_);});})(_aD,_);});},_dE,_);});},_dC,_);});},_e9=function(_ea,_){return new F(function(){return _dz(_cF,_cD,_ea,_);});},_eb=new T(function(){return B(unCStr("main-content"));}),_ec=[1,_ce,_eb],_ed=function(_ee,_){return new F(function(){return _1A(_ec,_1a,_e9,_ee,_);});},_ef=new T(function(){return B(unCStr("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"));}),_eg=new T(function(){return B(unCStr("https://code.jquery.com/jquery-1.11.2.min.js"));}),_eh=[0,125],_ei=[1,_eh,_1f],_ej=new T(function(){return B(unAppCStr("  justify-content: center; \n",_ei));}),_ek=new T(function(){return B(unAppCStr("  align-items: center;\n",_ej));}),_el=new T(function(){return B(unAppCStr("  display: flex;\n",_ek));}),_em=new T(function(){return B(unAppCStr(".vertical-align > [class*=\" col-\"] {\n",_el));}),_en=new T(function(){return B(unAppCStr(".vertical-align > [class^=\"col-\"],\n",_em));}),_eo=new T(function(){return B(unAppCStr("}\n",_en));}),_ep=new T(function(){return B(unAppCStr("  flex-direction: row;\n",_eo));}),_eq=new T(function(){return B(unAppCStr("  display: flex;\n",_ep));}),_er=new T(function(){return B(unAppCStr(".vertical-align {\n",_eq));}),_es=new T(function(){return B(unCStr("var mouse = {x: 0, y: 0};\ndocument.addEventListener(\'mousemove\', function(e){\nmouse.x = e.clientX || e.pageX;\nmouse.y = e.clientY || e.pageY\n}, false);"));}),_et=new T(function(){return B(unCStr("(function(){return document.body;})"));}),_eu=function(_ev,_){var _ew=B(A(_a,[toJSStr(E(_et)),_])),_ex=_ew,_ey=E(_3e)[1],_ez=takeMVar(_ey),_eA=_ez,_eB=B(A(_ev,[_eA,_])),_eC=_eB,_eD=E(_eC),_eE=E(_eD[1]),_=putMVar(_ey,_eD[2]),_eF=B(A(_eE[1],[[0,_ex],_])),_eG=_eF;return _eE[2];},_eH=function(_){var _eI=B(_B(_19,_)),_eJ=_eI,_eK=B(_B(_18,_)),_eL=_eK,_eM=toJSStr(E(_l)),_eN=B(A(_a,[_eM,_])),_eO=_eN,_eP=B(_bM(_17,[0,_eO],_)),_eQ=_eP,_eR=B(_0(_er,_eQ,_)),_eS=_eR,_eT=B(_10(_eg,_)),_eU=_eT,_eV=B(_10(_ef,_)),_eW=_eV,_eX=B(A(_a,[_eM,_])),_eY=_eX,_eZ=B(_N(_0,_es,[0,_eY],_)),_f0=_eZ;return new F(function(){return _eu(_ed,_);});},_f1=function(_){return new F(function(){return _eH(_);});};
var hasteMain = function() {B(A(_f1, [0]));};window.onload = hasteMain;