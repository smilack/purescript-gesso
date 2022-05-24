(() => {
  // output/Control.Bind/foreign.js
  var arrayBind = function(arr) {
    return function(f) {
      var result = [];
      for (var i2 = 0, l = arr.length; i2 < l; i2++) {
        Array.prototype.push.apply(result, f(arr[i2]));
      }
      return result;
    };
  };

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i2 = 0; i2 < l; i2++) {
        var f = fs[i2];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };
  var compose = function(dict) {
    return dict.compose;
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var on = function(f) {
    return function(g) {
      return function(x) {
        return function(y) {
          return f(g(x))(g(y));
        };
      };
    };
  };
  var flip = function(f) {
    return function(b2) {
      return function(a2) {
        return f(a2)(b2);
      };
    };
  };
  var $$const = function(a2) {
    return function(v) {
      return a2;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(arr[i2]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    return function(f) {
      return function(x) {
        return map(dictFunctor)($$const(x))(f);
      };
    };
  };
  var functorFn = {
    map: /* @__PURE__ */ compose(semigroupoidFn)
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    return function(a2) {
      return function(b2) {
        return apply(dictApply)(map(dictApply.Functor0())($$const(identity(categoryFn)))(a2))(b2);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure(dictApplicative)(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure(dictApplicative)(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    return function(f) {
      return function(a2) {
        return apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a2);
      };
    };
  };
  var applicativeArray = {
    pure: function(x) {
      return [x];
    },
    Apply0: function() {
      return applyArray;
    }
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    return function(f) {
      return function(g) {
        return function(a2) {
          return bindFlipped(dictBind)(f)(g(a2));
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Data.Array/foreign.js
  var range = function(start2) {
    return function(end) {
      var step4 = start2 > end ? -1 : 1;
      var result = new Array(step4 * (end - start2) + 1);
      var i2 = start2, n = 0;
      while (i2 !== end) {
        result[n++] = i2;
        i2 += step4;
      }
      result[n] = i2;
      return result;
    };
  };
  var replicateFill = function(count) {
    return function(value13) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value13);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value13) {
      var result = [];
      var n = 0;
      for (var i2 = 0; i2 < count; i2++) {
        result[n++] = value13;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head4) {
      return function(tail2) {
        return new Cons3(head4, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr3) {
      return function(xs) {
        return listToArray(foldr3(curryCons)(emptyList)(xs));
      };
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty7) {
    return function(next) {
      return function(xs) {
        return xs.length === 0 ? empty7({}) : next(xs[0])(xs.slice(1));
      };
    };
  };
  var findIndexImpl = function(just) {
    return function(nothing) {
      return function(f) {
        return function(xs) {
          for (var i2 = 0, l = xs.length; i2 < l; i2++) {
            if (f(xs[i2]))
              return just(i2);
          }
          return nothing;
        };
      };
    };
  };
  var _deleteAt = function(just) {
    return function(nothing) {
      return function(i2) {
        return function(l) {
          if (i2 < 0 || i2 >= l.length)
            return nothing;
          var l1 = l.slice();
          l1.splice(i2, 1);
          return just(l1);
        };
      };
    };
  };
  var sortByImpl = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from2 + (to - from2 >> 1);
      if (mid - from2 > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
      if (to - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
      i2 = from2;
      j = mid;
      k = from2;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();
  var slice = function(s) {
    return function(e) {
      return function(l) {
        return l.slice(s, e);
      };
    };
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupString = {
    append: concatString
  };
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var altArray = {
    alt: /* @__PURE__ */ append(semigroupArray),
    Functor0: function() {
      return functorArray;
    }
  };
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    return function(mb) {
      return function(m) {
        return bind(dictMonad.Bind1())(mb)(function(b2) {
          return unless(dictMonad.Applicative0())(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    return function(f) {
      return function(a2) {
        return bind(dictMonad.Bind1())(f)(function(f$prime) {
          return bind(dictMonad.Bind1())(a2)(function(a$prime) {
            return pure(dictMonad.Applicative0())(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq2) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq2 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordArrayImpl = function(f) {
    return function(xs) {
      return function(ys) {
        var i2 = 0;
        var xlen = xs.length;
        var ylen = ys.length;
        while (i2 < xlen && i2 < ylen) {
          var x = xs[i2];
          var y = ys[i2];
          var o = f(x)(y);
          if (o !== 0) {
            return o;
          }
          i2++;
        }
        if (xlen === ylen) {
          return 0;
        } else if (xlen > ylen) {
          return -1;
        } else {
          return 1;
        }
      };
    };
  };

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;
  var eqStringImpl = refEq;
  var eqArrayImpl = function(f) {
    return function(xs) {
      return function(ys) {
        if (xs.length !== ys.length)
          return false;
        for (var i2 = 0; i2 < xs.length; i2++) {
          if (!f(xs[i2])(ys[i2]))
            return false;
        }
        return true;
      };
    };
  };

  // output/Data.Eq/index.js
  var eqString = {
    eq: eqStringImpl
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eqArray = function(dictEq) {
    return {
      eq: eqArrayImpl(eq(dictEq))
    };
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Ord/index.js
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };
  var ordArray = function(dictOrd) {
    return {
      compare: function() {
        var toDelta = function(x) {
          return function(y) {
            var v = compare(dictOrd)(x)(y);
            if (v instanceof EQ) {
              return 0;
            }
            ;
            if (v instanceof LT) {
              return 1;
            }
            ;
            if (v instanceof GT) {
              return -1 | 0;
            }
            ;
            throw new Error("Failed pattern match at Data.Ord (line 79, column 7 - line 82, column 17): " + [v.constructor.name]);
          };
        };
        return function(xs) {
          return function(ys) {
            return compare(ordInt)(0)(ordArrayImpl(toDelta)(xs)(ys));
          };
        };
      }(),
      Eq0: function() {
        return eqArray(dictOrd.Eq0());
      }
    };
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };

  // output/Data.Show/index.js
  var showNumber = {
    show: showNumberImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
  };

  // output/Data.Maybe/index.js
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var semigroupMaybe = function(dictSemigroup) {
    return {
      append: function(v) {
        return function(v1) {
          if (v instanceof Nothing) {
            return v1;
          }
          ;
          if (v1 instanceof Nothing) {
            return v;
          }
          ;
          if (v instanceof Just && v1 instanceof Just) {
            return new Just(append(dictSemigroup)(v.value0)(v1.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Maybe (line 182, column 1 - line 185, column 43): " + [v.constructor.name, v1.constructor.name]);
        };
      }
    };
  };
  var monoidMaybe = function(dictSemigroup) {
    return {
      mempty: Nothing.value,
      Semigroup0: function() {
        return semigroupMaybe(dictSemigroup);
      }
    };
  };
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var fromMaybe = function(a2) {
    return maybe(a2)(identity(categoryFn));
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var eqMaybe = function(dictEq) {
    return {
      eq: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return true;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return eq(dictEq)(x.value0)(y.value0);
          }
          ;
          return false;
        };
      }
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map(functorMaybe)(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Data.Monoid/index.js
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var monoidArray = {
    mempty: [],
    Semigroup0: function() {
      return semigroupArray;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/foreign.js
  var pureE = function(a2) {
    return function() {
      return a2;
    };
  };
  var bindE = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref2) {
    return function() {
      return ref2.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref2) {
      return function() {
        var t = f(ref2.value);
        ref2.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(val) {
    return function(ref2) {
      return function() {
        ref2.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$new = _new;
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var modify_ = function(f) {
    return function(s) {
      return $$void(functorEffect)(modify(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var monadRecEffect = {
    tailRecM: function(f) {
      return function(a2) {
        var fromDone = function(v) {
          if (v instanceof Done) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 113, column 30 - line 113, column 44): " + [v.constructor.name]);
        };
        return function __do2() {
          var r = bindFlipped(bindEffect)($$new)(f(a2))();
          (function() {
            while (!function __do3() {
              var v = read(r)();
              if (v instanceof Loop) {
                var e = f(v.value0)();
                write(e)(r)();
                return false;
              }
              ;
              if (v instanceof Done) {
                return true;
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 104, column 22 - line 109, column 28): " + [v.constructor.name]);
            }()) {
            }
            ;
            return {};
          })();
          return map(functorEffect)(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
    }
  };

  // output/Data.Array.ST/foreign.js
  var sortByImpl2 = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from2 + (to - from2 >> 1);
      if (mid - from2 > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
      if (to - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
      i2 = from2;
      j = mid;
      k = from2;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b2) {
    return !b2;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var implies = function(dict) {
    return dict.implies;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a2) {
      return function(b2) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };
  var heytingAlgebraFunction = function(dictHeytingAlgebra) {
    return {
      ff: function(v) {
        return ff(dictHeytingAlgebra);
      },
      tt: function(v) {
        return tt(dictHeytingAlgebra);
      },
      implies: function(f) {
        return function(g) {
          return function(a2) {
            return implies(dictHeytingAlgebra)(f(a2))(g(a2));
          };
        };
      },
      conj: function(f) {
        return function(g) {
          return function(a2) {
            return conj(dictHeytingAlgebra)(f(a2))(g(a2));
          };
        };
      },
      disj: function(f) {
        return function(g) {
          return function(a2) {
            return disj(dictHeytingAlgebra)(f(a2))(g(a2));
          };
        };
      },
      not: function(f) {
        return function(a2) {
          return not(dictHeytingAlgebra)(f(a2));
        };
      }
    };
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var plusArray = {
    empty: [],
    Alt0: function() {
      return altArray;
    }
  };
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var uncurry = function(f) {
    return function(v) {
      return f(v.value0)(v.value1);
    };
  };
  var snd = function(v) {
    return v.value1;
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Data.Maybe.First/index.js
  var semigroupFirst = {
    append: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v;
        }
        ;
        return v1;
      };
    }
  };
  var monoidFirst = /* @__PURE__ */ function() {
    return {
      mempty: Nothing.value,
      Semigroup0: function() {
        return semigroupFirst;
      }
    };
  }();

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var unwrap = coerce;

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    return function(dictFoldable) {
      return function(f) {
        return foldr(dictFoldable)(function() {
          var $316 = applySecond(dictApplicative.Apply0());
          return function($317) {
            return $316(f($317));
          };
        }())(pure(dictApplicative)(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    return function(dictFoldable) {
      return flip(traverse_(dictApplicative)(dictFoldable));
    };
  };
  var sequence_ = function(dictApplicative) {
    return function(dictFoldable) {
      return traverse_(dictApplicative)(dictFoldable)(identity(categoryFn));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var intercalate2 = function(dictFoldable) {
    return function(dictMonoid) {
      return function(sep) {
        return function(xs) {
          var go2 = function(v) {
            return function(x) {
              if (v.init) {
                return {
                  init: false,
                  acc: x
                };
              }
              ;
              return {
                init: false,
                acc: append(dictMonoid.Semigroup0())(v.acc)(append(dictMonoid.Semigroup0())(sep)(x))
              };
            };
          };
          return foldl(dictFoldable)(go2)({
            init: true,
            acc: mempty(dictMonoid)
          })(xs).acc;
        };
      };
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(z) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return z;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0)(z);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(z) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return z;
          }
          ;
          if (v1 instanceof Just) {
            return v(z)(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty(dictMonoid);
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    return function(dictMonoid) {
      return function(f) {
        return foldr(dictFoldable)(function(x) {
          return function(acc) {
            return append(dictMonoid.Semigroup0())(f(x))(acc);
          };
        })(mempty(dictMonoid));
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var lookup = function(dictFoldable) {
    return function(dictEq) {
      return function(a2) {
        var $323 = unwrap();
        var $324 = foldMap(dictFoldable)(monoidFirst)(function(v) {
          var $305 = eq(dictEq)(a2)(v.value0);
          if ($305) {
            return new Just(v.value1);
          }
          ;
          return Nothing.value;
        });
        return function($325) {
          return $323($324($325));
        };
      };
    };
  };
  var fold = function(dictFoldable) {
    return function(dictMonoid) {
      return foldMap(dictFoldable)(dictMonoid)(identity(categoryFn));
    };
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a2) {
      return [a2];
    }
    function array2(a2) {
      return function(b2) {
        return [a2, b2];
      };
    }
    function array3(a2) {
      return function(b2) {
        return function(c) {
          return [a2, b2, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply2) {
      return function(map3) {
        return function(pure2) {
          return function(f) {
            return function(array) {
              function go2(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure2([]);
                  case 1:
                    return map3(array1)(f(array[bot]));
                  case 2:
                    return apply2(map3(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply2(apply2(map3(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply2(map3(concat2)(go2(bot, pivot)))(go2(pivot, top3));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var traverse = function(dict) {
    return dict.traverse;
  };
  var traversableMaybe = {
    traverse: function(dictApplicative) {
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return pure(dictApplicative)(Nothing.value);
          }
          ;
          if (v1 instanceof Just) {
            return map(dictApplicative.Apply0().Functor0())(Just.create)(v(v1.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Traversable (line 115, column 1 - line 119, column 33): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    },
    sequence: function(dictApplicative) {
      return function(v) {
        if (v instanceof Nothing) {
          return pure(dictApplicative)(Nothing.value);
        }
        ;
        if (v instanceof Just) {
          return map(dictApplicative.Apply0().Functor0())(Just.create)(v.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Traversable (line 115, column 1 - line 119, column 33): " + [v.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    },
    Foldable1: function() {
      return foldableMaybe;
    }
  };
  var sequence = function(dict) {
    return dict.sequence;
  };

  // output/Data.Semigroup.Foldable/index.js
  var foldl1 = function(dict) {
    return dict.foldl1;
  };

  // output/Data.Array/index.js
  var uncons = /* @__PURE__ */ function() {
    return unconsImpl($$const(Nothing.value))(function(x) {
      return function(xs) {
        return new Just({
          head: x,
          tail: xs
        });
      };
    });
  }();
  var sortBy = function(comp) {
    return sortByImpl(comp)(function(v) {
      if (v instanceof GT) {
        return 1;
      }
      ;
      if (v instanceof EQ) {
        return 0;
      }
      ;
      if (v instanceof LT) {
        return -1 | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 829, column 31 - line 832, column 11): " + [v.constructor.name]);
    });
  };
  var sort = function(dictOrd) {
    return function(xs) {
      return sortBy(compare(dictOrd))(xs);
    };
  };
  var singleton2 = function(a2) {
    return [a2];
  };
  var $$null = function(xs) {
    return length(xs) === 0;
  };
  var findIndex = /* @__PURE__ */ function() {
    return findIndexImpl(Just.create)(Nothing.value);
  }();
  var drop = function(n) {
    return function(xs) {
      var $79 = n < 1;
      if ($79) {
        return xs;
      }
      ;
      return slice(n)(length(xs))(xs);
    };
  };
  var deleteAt = /* @__PURE__ */ function() {
    return _deleteAt(Just.create)(Nothing.value);
  }();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i2) {
          return fromJust()(deleteAt(i2)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };
  var cons2 = function(x) {
    return function(xs) {
      return append(semigroupArray)([x])(xs);
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap(function() {
      var $99 = maybe([])(singleton2);
      return function($100) {
        return $99(f($100));
      };
    }());
  };

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var floor = Math.floor;
  var round = Math.round;

  // output/Data.Number/index.js
  var tau = 6.283185307179586;

  // output/Data.Int/index.js
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber(top(boundedInt))) {
      return top(boundedInt);
    }
    ;
    if (x <= toNumber(bottom(boundedInt))) {
      return bottom(boundedInt);
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var round2 = function($23) {
    return unsafeClamp(round($23));
  };
  var floor2 = function($25) {
    return unsafeClamp(floor($25));
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error4) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error4) {
        setTimeout(function() {
          throw error4;
        }, 0);
      }
    }
    function runSync(left2, right, eff) {
      try {
        return right(eff());
      } catch (error4) {
        return left2(error4);
      }
    }
    function runAsync(left2, eff, k) {
      try {
        return eff(k)();
      } catch (error4) {
        k(left2(error4))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size4 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk4;
        draining = true;
        while (size4 !== 0) {
          size4--;
          thunk4 = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk4();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i2, tmp;
          if (size4 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size4) % limit] = cb;
          size4++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill2(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill2(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error4) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step4 = aff;
      var fail2 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run4(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step4 = bhead(step4);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail2 = util.left(e);
                step4 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step4)) {
                status = RETURN;
                fail2 = step4;
                step4 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step4 = util.fromRight(step4);
              }
              break;
            case CONTINUE:
              switch (step4.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step4._2;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step4 = util.right(step4._1);
                  } else {
                    status = STEP_BIND;
                    step4 = step4._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step4 = runSync(util.left, util.right, step4._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step4 = runAsync(util.left, step4._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step4 = result2;
                        run4(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail2 = util.left(step4._1);
                  step4 = null;
                  break;
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step4, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step4, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step4._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step4._1) {
                    tmp.run();
                  }
                  step4 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step4 = sequential2(util, supervisor, step4._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step4 = interrupt || fail2 || step4;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail2) {
                      status = CONTINUE;
                      step4 = attempt._2(util.fromLeft(fail2));
                      fail2 = null;
                    }
                    break;
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step4 = util.fromRight(step4);
                    }
                    break;
                  case BRACKET:
                    bracketCount--;
                    if (fail2 === null) {
                      result = util.fromRight(step4);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step4 = attempt._3(result);
                      }
                    }
                    break;
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail2), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step4 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail2) {
                      step4 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                    } else {
                      step4 = attempt._1.completed(util.fromRight(step4))(attempt._2);
                    }
                    fail2 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail2), attempts, interrupt);
                    status = CONTINUE;
                    step4 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step4 = attempt._1;
                    fail2 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step4));
                }
              }
              joins = null;
              if (interrupt && fail2) {
                setTimeout(function() {
                  throw util.fromLeft(fail2);
                }, 0);
              } else if (util.isLeft(step4) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step4);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join4) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join4.rethrow;
            join4.handler(step4)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join4;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill2(error4, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error4);
              status = COMPLETED;
              step4 = interrupt;
              run4(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step4(error4)), attempts, interrupt);
                }
                status = RETURN;
                step4 = null;
                fail2 = null;
                run4(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step4 = null;
                fail2 = null;
              }
          }
          return canceler;
        };
      }
      function join3(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run4(runTick);
          }
          return canceler;
        };
      }
      return {
        kill: kill2,
        join: join3,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run4(runTick);
              });
            } else {
              run4(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill2(error4, par2, cb2) {
        var step4 = par2;
        var head4 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop:
          while (true) {
            tmp = null;
            switch (step4.tag) {
              case FORKED:
                if (step4._3 === EMPTY) {
                  tmp = fibers[step4._1];
                  kills2[count++] = tmp.kill(error4, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head4 === null) {
                  break loop;
                }
                step4 = head4._2;
                if (tail2 === null) {
                  head4 = null;
                } else {
                  head4 = tail2._1;
                  tail2 = tail2._2;
                }
                break;
              case MAP:
                step4 = step4._2;
                break;
              case APPLY:
              case ALT:
                if (head4) {
                  tail2 = new Aff2(CONS, head4, tail2);
                }
                head4 = step4;
                step4 = step4._1;
                break;
            }
          }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join3(result, head4, tail2) {
        var fail2, step4, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail2 = result;
          step4 = null;
        } else {
          step4 = result;
          fail2 = null;
        }
        loop:
          while (true) {
            lhs = null;
            rhs = null;
            tmp = null;
            kid = null;
            if (interrupt !== null) {
              return;
            }
            if (head4 === null) {
              cb(fail2 || step4)();
              return;
            }
            if (head4._3 !== EMPTY) {
              return;
            }
            switch (head4.tag) {
              case MAP:
                if (fail2 === null) {
                  head4._3 = util.right(head4._1(util.fromRight(step4)));
                  step4 = head4._3;
                } else {
                  head4._3 = fail2;
                }
                break;
              case APPLY:
                lhs = head4._1._3;
                rhs = head4._2._3;
                if (fail2) {
                  head4._3 = fail2;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, fail2 === lhs ? head4._2 : head4._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(fail2, null, null);
                      } else {
                        join3(fail2, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                } else if (lhs === EMPTY || rhs === EMPTY) {
                  return;
                } else {
                  step4 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                  head4._3 = step4;
                }
                break;
              case ALT:
                lhs = head4._1._3;
                rhs = head4._2._3;
                if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                  fail2 = step4 === lhs ? rhs : lhs;
                  step4 = null;
                  head4._3 = fail2;
                } else {
                  head4._3 = step4;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, step4 === lhs ? head4._2 : head4._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(step4, null, null);
                      } else {
                        join3(step4, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                }
                break;
            }
            if (tail2 === null) {
              head4 = null;
            } else {
              head4 = tail2._1;
              tail2 = tail2._2;
            }
          }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join3(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run4() {
        var status = CONTINUE;
        var step4 = par;
        var head4 = null;
        var tail2 = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status) {
              case CONTINUE:
                switch (step4.tag) {
                  case MAP:
                    if (head4) {
                      tail2 = new Aff2(CONS, head4, tail2);
                    }
                    head4 = new Aff2(MAP, step4._1, EMPTY, EMPTY);
                    step4 = step4._2;
                    break;
                  case APPLY:
                    if (head4) {
                      tail2 = new Aff2(CONS, head4, tail2);
                    }
                    head4 = new Aff2(APPLY, EMPTY, step4._2, EMPTY);
                    step4 = step4._1;
                    break;
                  case ALT:
                    if (head4) {
                      tail2 = new Aff2(CONS, head4, tail2);
                    }
                    head4 = new Aff2(ALT, EMPTY, step4._2, EMPTY);
                    step4 = step4._1;
                    break;
                  default:
                    fid = fiberId++;
                    status = RETURN;
                    tmp = step4;
                    step4 = new Aff2(FORKED, fid, new Aff2(CONS, head4, tail2), EMPTY);
                    tmp = Fiber(util, supervisor, tmp);
                    tmp.onComplete({
                      rethrow: false,
                      handler: resolve(step4)
                    })();
                    fibers[fid] = tmp;
                    if (supervisor) {
                      supervisor.register(tmp);
                    }
                }
                break;
              case RETURN:
                if (head4 === null) {
                  break loop;
                }
                if (head4._1 === EMPTY) {
                  head4._1 = step4;
                  status = CONTINUE;
                  step4 = head4._2;
                  head4._2 = EMPTY;
                } else {
                  head4._2 = step4;
                  step4 = head4;
                  if (tail2 === null) {
                    head4 = null;
                  } else {
                    head4 = tail2._1;
                    tail2 = tail2._2;
                  }
                }
            }
          }
        root = step4;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util.left(error4);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill2(error4, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run4();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential2(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value13) {
          return Aff.Pure(f(value13));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  function _fork(immediate) {
    return function(aff) {
      return Aff.Fork(immediate, aff);
    };
  }
  var _liftEffect = Aff.Sync;
  function _parAffMap(f) {
    return function(aff) {
      return Aff.ParMap(f, aff);
    };
  }
  function _parAffApply(aff1) {
    return function(aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  }
  var makeAff = Aff.Async;
  function generalBracket(acquire) {
    return function(options2) {
      return function(k) {
        return Aff.Bracket(acquire, options2, k);
      };
    };
  }
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _delay = function() {
    function setDelay(n, k) {
      if (n === 0 && typeof setImmediate !== "undefined") {
        return setImmediate(k);
      } else {
        return setTimeout(k, n);
      }
    }
    function clearDelay(n, t) {
      if (n === 0 && typeof clearImmediate !== "undefined") {
        return clearImmediate(t);
      } else {
        return clearTimeout(t);
      }
    }
    return function(right, ms) {
      return Aff.Async(function(cb) {
        return function() {
          var timer = setDelay(ms, cb(right()));
          return function() {
            return Aff.Sync(function() {
              return right(clearDelay(ms, timer));
            });
          };
        };
      });
    };
  }();
  var _sequential = Aff.Seq;

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($2) {
    return throwException(error($2));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    return function(a2) {
      return catchError(dictMonadError)(map(dictMonadError.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Right.create)(a2))(function() {
        var $21 = pure(dictMonadError.MonadThrow0().Monad0().Applicative0());
        return function($22) {
          return $21(Left.create($22));
        };
      }());
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var modify_2 = function(dictMonadState) {
    return function(f) {
      return state(dictMonadState)(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var gets = function(dictMonadState) {
    return function(f) {
      return state(dictMonadState)(function(s) {
        return new Tuple(f(s), s);
      });
    };
  };
  var get = function(dictMonadState) {
    return state(dictMonadState)(function(s) {
      return new Tuple(s, s);
    });
  };

  // output/Effect.Class/index.js
  var monadEffectEffect = {
    liftEffect: /* @__PURE__ */ identity(categoryFn),
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Writer.Class/index.js
  var tell = function(dict) {
    return dict.tell;
  };

  // output/Control.Monad.Writer.Trans/index.js
  var WriterT = function(x) {
    return x;
  };
  var runWriterT = function(v) {
    return v;
  };
  var mapWriterT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorWriterT = function(dictFunctor) {
    return {
      map: function(f) {
        return mapWriterT(map(dictFunctor)(function(v) {
          return new Tuple(f(v.value0), v.value1);
        }));
      }
    };
  };
  var applyWriterT = function(dictSemigroup) {
    return function(dictApply) {
      return {
        apply: function(v) {
          return function(v1) {
            var k = function(v3) {
              return function(v4) {
                return new Tuple(v3.value0(v4.value0), append(dictSemigroup)(v3.value1)(v4.value1));
              };
            };
            return apply(dictApply)(map(dictApply.Functor0())(k)(v))(v1);
          };
        },
        Functor0: function() {
          return functorWriterT(dictApply.Functor0());
        }
      };
    };
  };
  var bindWriterT = function(dictSemigroup) {
    return function(dictBind) {
      return {
        bind: function(v) {
          return function(k) {
            return bind(dictBind)(v)(function(v1) {
              var v2 = k(v1.value0);
              return map(dictBind.Apply0().Functor0())(function(v3) {
                return new Tuple(v3.value0, append(dictSemigroup)(v1.value1)(v3.value1));
              })(v2);
            });
          };
        },
        Apply0: function() {
          return applyWriterT(dictSemigroup)(dictBind.Apply0());
        }
      };
    };
  };
  var applicativeWriterT = function(dictMonoid) {
    return function(dictApplicative) {
      return {
        pure: function(a2) {
          return pure(dictApplicative)(new Tuple(a2, mempty(dictMonoid)));
        },
        Apply0: function() {
          return applyWriterT(dictMonoid.Semigroup0())(dictApplicative.Apply0());
        }
      };
    };
  };
  var monadWriterT = function(dictMonoid) {
    return function(dictMonad) {
      return {
        Applicative0: function() {
          return applicativeWriterT(dictMonoid)(dictMonad.Applicative0());
        },
        Bind1: function() {
          return bindWriterT(dictMonoid.Semigroup0())(dictMonad.Bind1());
        }
      };
    };
  };
  var monadTellWriterT = function(dictMonoid) {
    return function(dictMonad) {
      return {
        tell: function() {
          var $125 = pure(dictMonad.Applicative0());
          var $126 = Tuple.create(unit);
          return function($127) {
            return WriterT($125($126($127)));
          };
        }(),
        Semigroup0: dictMonoid.Semigroup0,
        Monad1: function() {
          return monadWriterT(dictMonoid)(dictMonad);
        }
      };
    };
  };

  // output/Data.Profunctor/index.js
  var profunctorFn = {
    dimap: function(a2b) {
      return function(c2d) {
        return function(b2c) {
          return function($8) {
            return c2d(b2c(a2b($8)));
          };
        };
      };
    }
  };

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Control.Parallel/index.js
  var parTraverse_ = function(dictParallel) {
    return function(dictFoldable) {
      return function(f) {
        var $17 = sequential(dictParallel);
        var $18 = traverse_(dictParallel.Applicative1())(dictFoldable)(function() {
          var $20 = parallel(dictParallel);
          return function($21) {
            return $20(f($21));
          };
        }());
        return function($19) {
          return $17($18($19));
        };
      };
    };
  };
  var parSequence_ = function(dictParallel) {
    return function(dictFoldable) {
      return parTraverse_(dictParallel)(dictFoldable)(identity(categoryFn));
    };
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith()(msg);
    });
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy2 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Canceler = function(x) {
    return x;
  };
  var suspendAff = /* @__PURE__ */ _fork(false);
  var functorParAff = {
    map: _parAffMap
  };
  var functorAff = {
    map: _map
  };
  var forkAff = /* @__PURE__ */ _fork(true);
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do2() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var bracket = function(acquire) {
    return function(completed) {
      return generalBracket(acquire)({
        killed: $$const(completed),
        failed: $$const(completed),
        completed: $$const(completed)
      });
    };
  };
  var applyParAff = {
    apply: _parAffApply,
    Functor0: function() {
      return functorParAff;
    }
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy2("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var $$finally = function(fin) {
    return function(a2) {
      return bracket(pure(applicativeAff)(unit))($$const(fin))($$const(a2));
    };
  };
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var effectCanceler = /* @__PURE__ */ function() {
    var $41 = liftEffect(monadEffectAff);
    return function($42) {
      return Canceler($$const($41($42)));
    };
  }();
  var joinFiber = function(v) {
    return makeAff(function(k) {
      return map(functorEffect)(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map(functorAff)(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind(bindAff)(liftEffect(monadEffectAff)(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect(monadEffectAff)($$void(functorEffect)(v.kill(e, $$const(pure(applicativeEffect)(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map(functorEffect)(effectCanceler)(v.kill(e, k));
        });
      });
    };
  };
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped(bindAff)(function() {
        var $45 = liftEffect(monadEffectAff);
        return function($46) {
          return $45(k($46));
        };
      }())($$try(monadErrorAff)(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void(functorEffect)(runAff(k)(aff));
    };
  };
  var parallelAff = {
    parallel: unsafeCoerce2,
    sequential: _sequential,
    Monad0: function() {
      return monadAff;
    },
    Applicative1: function() {
      return $lazy_applicativeParAff(0);
    }
  };
  var $lazy_applicativeParAff = /* @__PURE__ */ $runtime_lazy2("applicativeParAff", "Effect.Aff", function() {
    return {
      pure: function() {
        var $49 = parallel(parallelAff);
        var $50 = pure(applicativeAff);
        return function($51) {
          return $49($50($51));
        };
      }(),
      Apply0: function() {
        return applyParAff;
      }
    };
  });
  var applicativeParAff = /* @__PURE__ */ $lazy_applicativeParAff(131);
  var monadRecAff = {
    tailRecM: function(k) {
      var go2 = function(a2) {
        return bind(bindAff)(k(a2))(function(res) {
          if (res instanceof Done) {
            return pure(applicativeAff)(res.value0);
          }
          ;
          if (res instanceof Loop) {
            return go2(res.value0);
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 102, column 7 - line 104, column 23): " + [res.constructor.name]);
        });
      };
      return go2;
    },
    Monad0: function() {
      return monadAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeAff)(unit));

  // output/Effect.Aff.Class/index.js
  var monadAffAff = {
    liftAff: /* @__PURE__ */ identity(categoryFn),
    MonadEffect0: function() {
      return monadEffectAff;
    }
  };

  // output/CSS.String/index.js
  var isStringString = {
    fromString: /* @__PURE__ */ identity(categoryFn)
  };
  var fromString = function(dict) {
    return dict.fromString;
  };

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    };
    function finalCell(head4) {
      return new ConsCell(head4, emptyList);
    }
    function consList(x) {
      return function(xs) {
        return new ConsCell(x, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply2) {
      return function(map3) {
        return function(f) {
          var buildFrom = function(x, ys) {
            return apply2(map3(consList)(f(x)))(ys);
          };
          var go2 = function(acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last3 = xs[currentLen - 1];
              return new Cont(function() {
                var built = go2(buildFrom(last3, acc), currentLen - 1, xs);
                return built;
              });
            }
          };
          return function(array) {
            var acc = map3(finalCell)(f(array[array.length - 1]));
            var result = go2(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map3(listToArray)(result);
          };
        };
      };
    };
  }();

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var singleton3 = function(dictPlus) {
    return function(a2) {
      return new NonEmpty(a2, empty(dictPlus));
    };
  };
  var oneOf2 = function(dictAlternative) {
    return function(v) {
      return alt(dictAlternative.Plus1().Alt0())(pure(dictAlternative.Applicative0())(v.value0))(v.value1);
    };
  };
  var functorNonEmpty = function(dictFunctor) {
    return {
      map: function(f) {
        return function(m) {
          return new NonEmpty(f(m.value0), map(dictFunctor)(f)(m.value1));
        };
      }
    };
  };
  var foldableNonEmpty = function(dictFoldable) {
    return {
      foldMap: function(dictMonoid) {
        return function(f) {
          return function(v) {
            return append(dictMonoid.Semigroup0())(f(v.value0))(foldMap(dictFoldable)(dictMonoid)(f)(v.value1));
          };
        };
      },
      foldl: function(f) {
        return function(b2) {
          return function(v) {
            return foldl(dictFoldable)(f)(f(b2)(v.value0))(v.value1);
          };
        };
      },
      foldr: function(f) {
        return function(b2) {
          return function(v) {
            return f(v.value0)(foldr(dictFoldable)(f)(b2)(v.value1));
          };
        };
      }
    };
  };
  var foldable1NonEmpty = function(dictFoldable) {
    return {
      foldMap1: function(dictSemigroup) {
        return function(f) {
          return function(v) {
            return foldl(dictFoldable)(function(s) {
              return function(a1) {
                return append(dictSemigroup)(s)(f(a1));
              };
            })(f(v.value0))(v.value1);
          };
        };
      },
      foldr1: function(f) {
        return function(v) {
          return maybe(v.value0)(f(v.value0))(foldr(dictFoldable)(function(a1) {
            var $164 = maybe(a1)(f(a1));
            return function($165) {
              return Just.create($164($165));
            };
          })(Nothing.value)(v.value1));
        };
      },
      foldl1: function(f) {
        return function(v) {
          return foldl(dictFoldable)(f)(v.value0)(v.value1);
        };
      },
      Foldable0: function() {
        return foldableNonEmpty(dictFoldable);
      }
    };
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";

  // output/Control.Alternative/index.js
  var alternativeArray = {
    Applicative0: function() {
      return applicativeArray;
    },
    Plus1: function() {
      return plusArray;
    }
  };

  // output/Data.Profunctor.Strong/index.js
  var strongFn = {
    first: function(a2b) {
      return function(v) {
        return new Tuple(a2b(v.value0), v.value1);
      };
    },
    second: /* @__PURE__ */ map(functorTuple),
    Profunctor0: function() {
      return profunctorFn;
    }
  };
  var second = function(dict) {
    return dict.second;
  };

  // output/CSS.Property/index.js
  var Prefixed = /* @__PURE__ */ function() {
    function Prefixed2(value0) {
      this.value0 = value0;
    }
    ;
    Prefixed2.create = function(value0) {
      return new Prefixed2(value0);
    };
    return Prefixed2;
  }();
  var Plain = /* @__PURE__ */ function() {
    function Plain2(value0) {
      this.value0 = value0;
    }
    ;
    Plain2.create = function(value0) {
      return new Plain2(value0);
    };
    return Plain2;
  }();
  var Value = function(x) {
    return x;
  };
  var Key = function(x) {
    return x;
  };
  var value = function(dict) {
    return dict.value;
  };
  var valValue = {
    value: /* @__PURE__ */ identity(categoryFn)
  };
  var semigroupPrefixed = {
    append: function(v) {
      return function(v1) {
        if (v instanceof Plain && v1 instanceof Plain) {
          return new Plain(v.value0 + v1.value0);
        }
        ;
        if (v instanceof Plain && v1 instanceof Prefixed) {
          return new Prefixed(map(functorArray)(second(strongFn)(function(v2) {
            return v.value0 + v2;
          }))(v1.value0));
        }
        ;
        if (v instanceof Prefixed && v1 instanceof Plain) {
          return new Prefixed(map(functorArray)(second(strongFn)(function(v2) {
            return v1.value0 + v2;
          }))(v.value0));
        }
        ;
        if (v instanceof Prefixed && v1 instanceof Prefixed) {
          return new Prefixed(append(semigroupArray)(v.value0)(v1.value0));
        }
        ;
        throw new Error("Failed pattern match at CSS.Property (line 23, column 1 - line 27, column 59): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var semigroupValue = {
    append: function(v) {
      return function(v1) {
        return append(semigroupPrefixed)(v)(v1);
      };
    }
  };
  var plain = function(v) {
    if (v instanceof Prefixed) {
      return fromMaybe("")(lookup(foldableArray)(eqString)("")(v.value0));
    }
    ;
    if (v instanceof Plain) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at CSS.Property (line 32, column 1 - line 32, column 28): " + [v.constructor.name]);
  };
  var monoidPrefixed = /* @__PURE__ */ function() {
    return {
      mempty: new Plain(mempty(monoidString)),
      Semigroup0: function() {
        return semigroupPrefixed;
      }
    };
  }();
  var monoidValue = {
    mempty: /* @__PURE__ */ mempty(monoidPrefixed),
    Semigroup0: function() {
      return semigroupValue;
    }
  };
  var isStringPrefixed = /* @__PURE__ */ function() {
    return {
      fromString: Plain.create
    };
  }();
  var isStringValue = {
    fromString: /* @__PURE__ */ function() {
      var $103 = fromString(isStringPrefixed);
      return function($104) {
        return Value($103($104));
      };
    }()
  };
  var valList = function(dictVal) {
    return {
      value: function() {
        var $109 = intercalate2(foldableArray)(monoidValue)(fromString(isStringValue)(", "));
        return function($110) {
          return $109(function(v) {
            return map(functorArray)(value(dictVal))(v);
          }($110));
        };
      }()
    };
  };
  var valNumber = {
    value: /* @__PURE__ */ function() {
      var $114 = fromString(isStringValue);
      var $115 = show(showNumber);
      return function($116) {
        return $114($115($116));
      };
    }()
  };
  var valString = {
    value: /* @__PURE__ */ fromString(isStringValue)
  };
  var isStringKey = {
    fromString: /* @__PURE__ */ function() {
      var $117 = fromString(isStringPrefixed);
      return function($118) {
        return Key($117($118));
      };
    }()
  };
  var cast = function(v) {
    return v;
  };

  // output/CSS.Selector/index.js
  var Id = /* @__PURE__ */ function() {
    function Id2(value0) {
      this.value0 = value0;
    }
    ;
    Id2.create = function(value0) {
      return new Id2(value0);
    };
    return Id2;
  }();
  var Class = /* @__PURE__ */ function() {
    function Class2(value0) {
      this.value0 = value0;
    }
    ;
    Class2.create = function(value0) {
      return new Class2(value0);
    };
    return Class2;
  }();
  var Attr = /* @__PURE__ */ function() {
    function Attr2(value0) {
      this.value0 = value0;
    }
    ;
    Attr2.create = function(value0) {
      return new Attr2(value0);
    };
    return Attr2;
  }();
  var AttrVal = /* @__PURE__ */ function() {
    function AttrVal2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AttrVal2.create = function(value0) {
      return function(value1) {
        return new AttrVal2(value0, value1);
      };
    };
    return AttrVal2;
  }();
  var AttrBegins = /* @__PURE__ */ function() {
    function AttrBegins2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AttrBegins2.create = function(value0) {
      return function(value1) {
        return new AttrBegins2(value0, value1);
      };
    };
    return AttrBegins2;
  }();
  var AttrEnds = /* @__PURE__ */ function() {
    function AttrEnds2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AttrEnds2.create = function(value0) {
      return function(value1) {
        return new AttrEnds2(value0, value1);
      };
    };
    return AttrEnds2;
  }();
  var AttrContains = /* @__PURE__ */ function() {
    function AttrContains2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AttrContains2.create = function(value0) {
      return function(value1) {
        return new AttrContains2(value0, value1);
      };
    };
    return AttrContains2;
  }();
  var AttrSpace = /* @__PURE__ */ function() {
    function AttrSpace2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AttrSpace2.create = function(value0) {
      return function(value1) {
        return new AttrSpace2(value0, value1);
      };
    };
    return AttrSpace2;
  }();
  var AttrHyph = /* @__PURE__ */ function() {
    function AttrHyph2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AttrHyph2.create = function(value0) {
      return function(value1) {
        return new AttrHyph2(value0, value1);
      };
    };
    return AttrHyph2;
  }();
  var Pseudo = /* @__PURE__ */ function() {
    function Pseudo2(value0) {
      this.value0 = value0;
    }
    ;
    Pseudo2.create = function(value0) {
      return new Pseudo2(value0);
    };
    return Pseudo2;
  }();
  var PseudoFunc = /* @__PURE__ */ function() {
    function PseudoFunc2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    PseudoFunc2.create = function(value0) {
      return function(value1) {
        return new PseudoFunc2(value0, value1);
      };
    };
    return PseudoFunc2;
  }();
  var Star = /* @__PURE__ */ function() {
    function Star2() {
    }
    ;
    Star2.value = new Star2();
    return Star2;
  }();
  var Elem = /* @__PURE__ */ function() {
    function Elem3(value0) {
      this.value0 = value0;
    }
    ;
    Elem3.create = function(value0) {
      return new Elem3(value0);
    };
    return Elem3;
  }();
  var PathChild = /* @__PURE__ */ function() {
    function PathChild2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    PathChild2.create = function(value0) {
      return function(value1) {
        return new PathChild2(value0, value1);
      };
    };
    return PathChild2;
  }();
  var Deep = /* @__PURE__ */ function() {
    function Deep2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Deep2.create = function(value0) {
      return function(value1) {
        return new Deep2(value0, value1);
      };
    };
    return Deep2;
  }();
  var Adjacent = /* @__PURE__ */ function() {
    function Adjacent2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Adjacent2.create = function(value0) {
      return function(value1) {
        return new Adjacent2(value0, value1);
      };
    };
    return Adjacent2;
  }();
  var Combined = /* @__PURE__ */ function() {
    function Combined2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Combined2.create = function(value0) {
      return function(value1) {
        return new Combined2(value0, value1);
      };
    };
    return Combined2;
  }();
  var Selector = /* @__PURE__ */ function() {
    function Selector2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Selector2.create = function(value0) {
      return function(value1) {
        return new Selector2(value0, value1);
      };
    };
    return Selector2;
  }();
  var $$with = function(v) {
    return function(v1) {
      return new Selector(append(semigroupArray)(v.value0)(v1), v.value1);
    };
  };
  var star = /* @__PURE__ */ function() {
    return new Selector([], Star.value);
  }();
  var eqPredicate = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Id && y instanceof Id) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof Class && y instanceof Class) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof Attr && y instanceof Attr) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof AttrVal && y instanceof AttrVal) {
          return x.value0 === y.value0 && x.value1 === y.value1;
        }
        ;
        if (x instanceof AttrBegins && y instanceof AttrBegins) {
          return x.value0 === y.value0 && x.value1 === y.value1;
        }
        ;
        if (x instanceof AttrEnds && y instanceof AttrEnds) {
          return x.value0 === y.value0 && x.value1 === y.value1;
        }
        ;
        if (x instanceof AttrContains && y instanceof AttrContains) {
          return x.value0 === y.value0 && x.value1 === y.value1;
        }
        ;
        if (x instanceof AttrSpace && y instanceof AttrSpace) {
          return x.value0 === y.value0 && x.value1 === y.value1;
        }
        ;
        if (x instanceof AttrHyph && y instanceof AttrHyph) {
          return x.value0 === y.value0 && x.value1 === y.value1;
        }
        ;
        if (x instanceof Pseudo && y instanceof Pseudo) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof PseudoFunc && y instanceof PseudoFunc) {
          return x.value0 === y.value0 && eq(eqArray(eqString))(x.value1)(y.value1);
        }
        ;
        return false;
      };
    }
  };
  var ordPredicate = {
    compare: function(x) {
      return function(y) {
        if (x instanceof Id && y instanceof Id) {
          return compare(ordString)(x.value0)(y.value0);
        }
        ;
        if (x instanceof Id) {
          return LT.value;
        }
        ;
        if (y instanceof Id) {
          return GT.value;
        }
        ;
        if (x instanceof Class && y instanceof Class) {
          return compare(ordString)(x.value0)(y.value0);
        }
        ;
        if (x instanceof Class) {
          return LT.value;
        }
        ;
        if (y instanceof Class) {
          return GT.value;
        }
        ;
        if (x instanceof Attr && y instanceof Attr) {
          return compare(ordString)(x.value0)(y.value0);
        }
        ;
        if (x instanceof Attr) {
          return LT.value;
        }
        ;
        if (y instanceof Attr) {
          return GT.value;
        }
        ;
        if (x instanceof AttrVal && y instanceof AttrVal) {
          var v = compare(ordString)(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare(ordString)(x.value1)(y.value1);
        }
        ;
        if (x instanceof AttrVal) {
          return LT.value;
        }
        ;
        if (y instanceof AttrVal) {
          return GT.value;
        }
        ;
        if (x instanceof AttrBegins && y instanceof AttrBegins) {
          var v = compare(ordString)(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare(ordString)(x.value1)(y.value1);
        }
        ;
        if (x instanceof AttrBegins) {
          return LT.value;
        }
        ;
        if (y instanceof AttrBegins) {
          return GT.value;
        }
        ;
        if (x instanceof AttrEnds && y instanceof AttrEnds) {
          var v = compare(ordString)(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare(ordString)(x.value1)(y.value1);
        }
        ;
        if (x instanceof AttrEnds) {
          return LT.value;
        }
        ;
        if (y instanceof AttrEnds) {
          return GT.value;
        }
        ;
        if (x instanceof AttrContains && y instanceof AttrContains) {
          var v = compare(ordString)(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare(ordString)(x.value1)(y.value1);
        }
        ;
        if (x instanceof AttrContains) {
          return LT.value;
        }
        ;
        if (y instanceof AttrContains) {
          return GT.value;
        }
        ;
        if (x instanceof AttrSpace && y instanceof AttrSpace) {
          var v = compare(ordString)(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare(ordString)(x.value1)(y.value1);
        }
        ;
        if (x instanceof AttrSpace) {
          return LT.value;
        }
        ;
        if (y instanceof AttrSpace) {
          return GT.value;
        }
        ;
        if (x instanceof AttrHyph && y instanceof AttrHyph) {
          var v = compare(ordString)(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare(ordString)(x.value1)(y.value1);
        }
        ;
        if (x instanceof AttrHyph) {
          return LT.value;
        }
        ;
        if (y instanceof AttrHyph) {
          return GT.value;
        }
        ;
        if (x instanceof Pseudo && y instanceof Pseudo) {
          return compare(ordString)(x.value0)(y.value0);
        }
        ;
        if (x instanceof Pseudo) {
          return LT.value;
        }
        ;
        if (y instanceof Pseudo) {
          return GT.value;
        }
        ;
        if (x instanceof PseudoFunc && y instanceof PseudoFunc) {
          var v = compare(ordString)(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare(ordArray(ordString))(x.value1)(y.value1);
        }
        ;
        throw new Error("Failed pattern match at CSS.Selector (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqPredicate;
    }
  };
  var element = function(e) {
    return new Selector([], new Elem(e));
  };
  var deep = function(a2) {
    return function(b2) {
      return new Selector([], new Deep(a2, b2));
    };
  };
  var child = function(a2) {
    return function(b2) {
      return new Selector([], new PathChild(a2, b2));
    };
  };

  // output/Control.Monad.Writer/index.js
  var runWriter = /* @__PURE__ */ function() {
    var $2 = unwrap();
    return function($3) {
      return $2(runWriterT($3));
    };
  }();
  var execWriter = function(m) {
    return snd(runWriter(m));
  };

  // output/CSS.Stylesheet/index.js
  var Self = /* @__PURE__ */ function() {
    function Self2(value0) {
      this.value0 = value0;
    }
    ;
    Self2.create = function(value0) {
      return new Self2(value0);
    };
    return Self2;
  }();
  var Root = /* @__PURE__ */ function() {
    function Root2(value0) {
      this.value0 = value0;
    }
    ;
    Root2.create = function(value0) {
      return new Root2(value0);
    };
    return Root2;
  }();
  var Pop = /* @__PURE__ */ function() {
    function Pop2(value0) {
      this.value0 = value0;
    }
    ;
    Pop2.create = function(value0) {
      return new Pop2(value0);
    };
    return Pop2;
  }();
  var Child = /* @__PURE__ */ function() {
    function Child2(value0) {
      this.value0 = value0;
    }
    ;
    Child2.create = function(value0) {
      return new Child2(value0);
    };
    return Child2;
  }();
  var Sub = /* @__PURE__ */ function() {
    function Sub2(value0) {
      this.value0 = value0;
    }
    ;
    Sub2.create = function(value0) {
      return new Sub2(value0);
    };
    return Sub2;
  }();
  var Property = /* @__PURE__ */ function() {
    function Property3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Property3.create = function(value0) {
      return function(value1) {
        return new Property3(value0, value1);
      };
    };
    return Property3;
  }();
  var Nested = /* @__PURE__ */ function() {
    function Nested2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Nested2.create = function(value0) {
      return function(value1) {
        return new Nested2(value0, value1);
      };
    };
    return Nested2;
  }();
  var Query = /* @__PURE__ */ function() {
    function Query3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Query3.create = function(value0) {
      return function(value1) {
        return new Query3(value0, value1);
      };
    };
    return Query3;
  }();
  var Face = /* @__PURE__ */ function() {
    function Face2(value0) {
      this.value0 = value0;
    }
    ;
    Face2.create = function(value0) {
      return new Face2(value0);
    };
    return Face2;
  }();
  var Keyframe = /* @__PURE__ */ function() {
    function Keyframe2(value0) {
      this.value0 = value0;
    }
    ;
    Keyframe2.create = function(value0) {
      return new Keyframe2(value0);
    };
    return Keyframe2;
  }();
  var Import = /* @__PURE__ */ function() {
    function Import2(value0) {
      this.value0 = value0;
    }
    ;
    Import2.create = function(value0) {
      return new Import2(value0);
    };
    return Import2;
  }();
  var S = function(x) {
    return x;
  };
  var runS = function(v) {
    return execWriter(v);
  };
  var rule = /* @__PURE__ */ function() {
    var $282 = tell(monadTellWriterT(monoidArray)(monadIdentity));
    return function($283) {
      return S($282(singleton2($283)));
    };
  }();
  var key = function(dictVal) {
    return function(k) {
      return function(v) {
        return rule(new Property(cast(k), value(dictVal)(v)));
      };
    };
  };
  var functorStyleM = {
    map: function(f) {
      return function(v) {
        return map(functorWriterT(functorIdentity))(f)(v);
      };
    }
  };
  var applyStyleM = {
    apply: function(v) {
      return function(v1) {
        return apply(applyWriterT(semigroupArray)(applyIdentity))(v)(v1);
      };
    },
    Functor0: function() {
      return functorStyleM;
    }
  };
  var bindStyleM = {
    bind: function(v) {
      return function(f) {
        return bind(bindWriterT(semigroupArray)(bindIdentity))(v)(function($288) {
          return function(v1) {
            return v1;
          }(f($288));
        });
      };
    },
    Apply0: function() {
      return applyStyleM;
    }
  };

  // output/Data.These/index.js
  var This = /* @__PURE__ */ function() {
    function This2(value0) {
      this.value0 = value0;
    }
    ;
    This2.create = function(value0) {
      return new This2(value0);
    };
    return This2;
  }();
  var That = /* @__PURE__ */ function() {
    function That2(value0) {
      this.value0 = value0;
    }
    ;
    That2.create = function(value0) {
      return new That2(value0);
    };
    return That2;
  }();
  var Both = /* @__PURE__ */ function() {
    function Both2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Both2.create = function(value0) {
      return function(value1) {
        return new Both2(value0, value1);
      };
    };
    return Both2;
  }();
  var theseRight = function(v) {
    if (v instanceof Both) {
      return new Just(v.value1);
    }
    ;
    if (v instanceof That) {
      return new Just(v.value0);
    }
    ;
    return Nothing.value;
  };
  var theseLeft = function(v) {
    if (v instanceof Both) {
      return new Just(v.value0);
    }
    ;
    if (v instanceof This) {
      return new Just(v.value0);
    }
    ;
    return Nothing.value;
  };
  var semigroupThese = function(dictSemigroup) {
    return function(dictSemigroup1) {
      return {
        append: function(v) {
          return function(v1) {
            if (v instanceof This && v1 instanceof This) {
              return new This(append(dictSemigroup)(v.value0)(v1.value0));
            }
            ;
            if (v instanceof This && v1 instanceof That) {
              return new Both(v.value0, v1.value0);
            }
            ;
            if (v instanceof This && v1 instanceof Both) {
              return new Both(append(dictSemigroup)(v.value0)(v1.value0), v1.value1);
            }
            ;
            if (v instanceof That && v1 instanceof This) {
              return new Both(v1.value0, v.value0);
            }
            ;
            if (v instanceof That && v1 instanceof That) {
              return new That(append(dictSemigroup1)(v.value0)(v1.value0));
            }
            ;
            if (v instanceof That && v1 instanceof Both) {
              return new Both(v1.value0, append(dictSemigroup1)(v.value0)(v1.value1));
            }
            ;
            if (v instanceof Both && v1 instanceof This) {
              return new Both(append(dictSemigroup)(v.value0)(v1.value0), v.value1);
            }
            ;
            if (v instanceof Both && v1 instanceof That) {
              return new Both(v.value0, append(dictSemigroup1)(v.value1)(v1.value0));
            }
            ;
            if (v instanceof Both && v1 instanceof Both) {
              return new Both(append(dictSemigroup)(v.value0)(v1.value0), append(dictSemigroup1)(v.value1)(v1.value1));
            }
            ;
            throw new Error("Failed pattern match at Data.These (line 23, column 1 - line 32, column 56): " + [v.constructor.name, v1.constructor.name]);
          };
        }
      };
    };
  };

  // output/Effect.Console/foreign.js
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/CSS.Render/index.js
  var Sheet = function(x) {
    return x;
  };
  var Inline = function(x) {
    return x;
  };
  var sepWith = function(s) {
    return function(a2) {
      return function(b2) {
        return a2 + (s + b2);
      };
    };
  };
  var semigroupInline = {
    append: function(v) {
      return function(v1) {
        return v + v1;
      };
    }
  };
  var semigroupFile = {
    append: function(v) {
      return function(v1) {
        return v + v1;
      };
    }
  };
  var properties = function(xs) {
    var sheetRules = either(function(v) {
      return mempty(monoidString);
    })(function(v) {
      return fold(foldableArray)(monoidString)([v.value0, ": ", v.value1]);
    });
    return intercalate2(foldableArray)(monoidString)("; ")(map(functorArray)(sheetRules)(xs));
  };
  var predicate = function(v) {
    if (v instanceof Id) {
      return "#" + v.value0;
    }
    ;
    if (v instanceof Class) {
      return "." + v.value0;
    }
    ;
    if (v instanceof Attr) {
      return "[" + (v.value0 + "]");
    }
    ;
    if (v instanceof AttrVal) {
      return "[" + (v.value0 + ("='" + (v.value1 + "']")));
    }
    ;
    if (v instanceof AttrBegins) {
      return "[" + (v.value0 + ("^='" + (v.value1 + "']")));
    }
    ;
    if (v instanceof AttrEnds) {
      return "[" + (v.value0 + ("$='" + (v.value1 + "']")));
    }
    ;
    if (v instanceof AttrContains) {
      return "[" + (v.value0 + ("*='" + (v.value1 + "']")));
    }
    ;
    if (v instanceof AttrSpace) {
      return "[" + (v.value0 + ("~='" + (v.value1 + "']")));
    }
    ;
    if (v instanceof AttrHyph) {
      return "[" + (v.value0 + ("|='" + (v.value1 + "']")));
    }
    ;
    if (v instanceof Pseudo) {
      return ":" + v.value0;
    }
    ;
    if (v instanceof PseudoFunc) {
      return ":" + (v.value0 + ("(" + (intercalate2(foldableArray)(monoidString)(",")(v.value1) + ")")));
    }
    ;
    throw new Error("Failed pattern match at CSS.Render (line 178, column 1 - line 178, column 33): " + [v.constructor.name]);
  };
  var selector$prime$prime = function(v) {
    return function(v1) {
      if (v.length === 0 && v1 instanceof Star) {
        return ["*"];
      }
      ;
      if (v1 instanceof Star) {
        return [""];
      }
      ;
      if (v1 instanceof Elem) {
        return [v1.value0];
      }
      ;
      if (v1 instanceof PathChild) {
        return apply(applyArray)(map(functorArray)(sepWith(" > "))(selector$prime(v1.value0)))(selector$prime(v1.value1));
      }
      ;
      if (v1 instanceof Deep) {
        return apply(applyArray)(map(functorArray)(sepWith(" "))(selector$prime(v1.value0)))(selector$prime(v1.value1));
      }
      ;
      if (v1 instanceof Adjacent) {
        return apply(applyArray)(map(functorArray)(sepWith(" + "))(selector$prime(v1.value0)))(selector$prime(v1.value1));
      }
      ;
      if (v1 instanceof Combined) {
        return append(semigroupArray)(selector$prime(v1.value0))(selector$prime(v1.value1));
      }
      ;
      throw new Error("Failed pattern match at CSS.Render (line 143, column 1 - line 143, column 63): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var selector$prime = function(v) {
    return map(functorArray)(function(v1) {
      return v1 + foldMap(foldableArray)(monoidString)(predicate)(sort(ordPredicate)(v.value0));
    })(selector$prime$prime(v.value0)(v.value1));
  };
  var selector = /* @__PURE__ */ function() {
    var $167 = intercalate2(foldableArray)(monoidString)(", ");
    return function($168) {
      return $167(selector$prime($168));
    };
  }();
  var nel = function(v) {
    if (v.length === 0) {
      return Nothing.value;
    }
    ;
    return map(functorMaybe)(function(v1) {
      return new NonEmpty(v1.head, v1.tail);
    })(uncons(v));
  };
  var merger = function(v) {
    if (v.value0 instanceof Child) {
      return maybe(v.value0.value0)(function(xs$prime) {
        return child(merger(xs$prime))(v.value0.value0);
      })(nel(v.value1));
    }
    ;
    if (v.value0 instanceof Sub) {
      return maybe(v.value0.value0)(function(xs$prime) {
        return deep(merger(xs$prime))(v.value0.value0);
      })(nel(v.value1));
    }
    ;
    if (v.value0 instanceof Root) {
      return maybe(v.value0.value0)(function(xs$prime) {
        return deep(v.value0.value0)(merger(xs$prime));
      })(nel(v.value1));
    }
    ;
    if (v.value0 instanceof Pop) {
      return maybe(element("TODO"))(merger)(nel(drop(v.value0.value0)(cons2(v.value0)(v.value1))));
    }
    ;
    if (v.value0 instanceof Self) {
      return maybe($$with(star)(v.value0.value0))(function(xs$prime) {
        return $$with(merger(xs$prime))(v.value0.value0);
      })(nel(v.value1));
    }
    ;
    throw new Error("Failed pattern match at CSS.Render (line 171, column 3 - line 176, column 102): " + [v.value0.constructor.name]);
  };
  var mediaType = function(v) {
    return plain(v);
  };
  var imp = function(t) {
    return Just.create(That.create(Sheet(fromString(isStringString)("@import url(" + (t + ");\n")))));
  };
  var getSheet = function(v) {
    return v;
  };
  var renderedSheet = function(v) {
    return bind(bindMaybe)(v)(function() {
      var $169 = map(functorMaybe)(getSheet);
      return function($170) {
        return $169(theseRight($170));
      };
    }());
  };
  var getInline = function(v) {
    return v;
  };
  var renderedInline = function(v) {
    return bind(bindMaybe)(v)(function() {
      var $171 = map(functorMaybe)(getInline);
      return function($172) {
        return $171(theseLeft($172));
      };
    }());
  };
  var feature = function(v) {
    return maybe(v.value0)(function(v1) {
      return "(" + (v.value0 + (": " + (plain(v1) + ")")));
    })(v.value1);
  };
  var mediaQuery = function(v) {
    return "@media " + (mediaType(v.value1) + foldl1(foldable1NonEmpty(foldableArray))(append(semigroupString))(map(functorNonEmpty(functorArray))(function($173) {
      return function(v1) {
        return " and " + v1;
      }(feature($173));
    })(v.value2)));
  };
  var collect$prime = function(v) {
    return function(v1) {
      if (v instanceof Plain && v1 instanceof Plain) {
        return [new Right(new Tuple(v.value0, v1.value0))];
      }
      ;
      if (v instanceof Prefixed && v1 instanceof Plain) {
        return map(functorArray)(function(v3) {
          return new Right(new Tuple(v3.value0 + v3.value1, v1.value0));
        })(v.value0);
      }
      ;
      if (v instanceof Plain && v1 instanceof Prefixed) {
        return map(functorArray)(function(v2) {
          return new Right(new Tuple(v.value0, v2.value0 + v2.value1));
        })(v1.value0);
      }
      ;
      if (v instanceof Prefixed && v1 instanceof Prefixed) {
        return map(functorArray)(function(v2) {
          return maybe(new Left(v2.value0 + v2.value1))(function() {
            var $174 = Tuple.create(v2.value0 + v2.value1);
            return function($175) {
              return Right.create($174(function(v3) {
                return v2.value0 + v3;
              }($175)));
            };
          }())(lookup(foldableArray)(eqString)(v2.value0)(v1.value0));
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at CSS.Render (line 158, column 1 - line 158, column 80): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var collect2 = function(v) {
    return collect$prime(v.value0)(v.value1);
  };
  var rule$prime = function(sel) {
    return function(props) {
      var p2 = bind(bindArray)(props)(collect2);
      var q2 = map(functorMaybe)(function() {
        var $176 = oneOf2(alternativeArray);
        return function($177) {
          return This.create(Inline(properties($176($177))));
        };
      }())(nel(p2));
      var o = function(sel$prime) {
        return Just.create(That.create(Sheet(intercalate2(foldableArray)(monoidString)(" ")([selector(merger(sel$prime)), "{", properties(p2), "}\n"]))));
      };
      return maybe(q2)(o)(nel(sel));
    };
  };
  var rules = function(sel) {
    return function(rs) {
      var queries = function(v) {
        if (v instanceof Query) {
          return new Just(new Tuple(v.value0, v.value1));
        }
        ;
        return Nothing.value;
      };
      var queryRules = foldMap(foldableArray)(monoidMaybe(semigroupThese(semigroupInline)(semigroupFile)))(uncurry(flip(query$prime)(sel)))(mapMaybe(queries)(rs));
      var property = function(v) {
        if (v instanceof Property) {
          return new Just(new Tuple(v.value0, v.value1));
        }
        ;
        return Nothing.value;
      };
      var topRules = function() {
        var rs$prime = mapMaybe(property)(rs);
        var $154 = not(heytingAlgebraFunction(heytingAlgebraBoolean))($$null)(rs$prime);
        if ($154) {
          return rule$prime(sel)(rs$prime);
        }
        ;
        return Nothing.value;
      }();
      var nestedRules = function(a2) {
        return rules(cons2(a2)(sel));
      };
      var nested = function(v) {
        if (v instanceof Nested) {
          return new Just(new Tuple(v.value0, v.value1));
        }
        ;
        return Nothing.value;
      };
      var nestedSheets = fold(foldableArray)(monoidMaybe(semigroupThese(semigroupInline)(semigroupFile)))(map(functorArray)(uncurry(nestedRules))(mapMaybe(nested)(rs)));
      var kframes = function(v) {
        if (v instanceof Keyframe) {
          return new Just(v.value0);
        }
        ;
        return Nothing.value;
      };
      var keyframeRules = foldMap(foldableArray)(monoidMaybe(semigroupThese(semigroupInline)(semigroupFile)))(kframe)(mapMaybe(kframes)(rs));
      var imports = function(v) {
        if (v instanceof Import) {
          return new Just(v.value0);
        }
        ;
        return Nothing.value;
      };
      var importRules = foldMap(foldableArray)(monoidMaybe(semigroupThese(semigroupInline)(semigroupFile)))(imp)(mapMaybe(imports)(rs));
      var faces = function(v) {
        if (v instanceof Face) {
          return new Just(v.value0);
        }
        ;
        return Nothing.value;
      };
      var faceRules = foldMap(foldableArray)(monoidMaybe(semigroupThese(semigroupInline)(semigroupFile)))(face)(mapMaybe(faces)(rs));
      return append(semigroupMaybe(semigroupThese(semigroupInline)(semigroupFile)))(topRules)(append(semigroupMaybe(semigroupThese(semigroupInline)(semigroupFile)))(importRules)(append(semigroupMaybe(semigroupThese(semigroupInline)(semigroupFile)))(keyframeRules)(append(semigroupMaybe(semigroupThese(semigroupInline)(semigroupFile)))(faceRules)(append(semigroupMaybe(semigroupThese(semigroupInline)(semigroupFile)))(nestedSheets)(queryRules)))));
    };
  };
  var query$prime = function(q2) {
    return function(sel) {
      return function(rs) {
        return Just.create(That.create(Sheet(mediaQuery(q2) + (" { " + (fromMaybe("")(renderedSheet(rules(sel)(rs))) + " }\n")))));
      };
    };
  };
  var kframe = function(v) {
    var renderContent = " " + (v.value0 + (" { " + (intercalate2(foldableNonEmpty(foldableArray))(monoidString)(" ")(map(functorNonEmpty(functorArray))(uncurry(frame))(v.value1)) + " }\n")));
    var keywords = ["@keyframes", "@-webkit-keyframes", "@-moz-keyframes", "@-o-keyframes"];
    var allKeywordsWithContent = fold(foldableArray)(monoidString)(map(functorArray)(function(v1) {
      return v1 + renderContent;
    })(keywords));
    return new Just(new That(allKeywordsWithContent));
  };
  var frame = function(p2) {
    return function(rs) {
      var x = fromMaybe("")(renderedInline(rules([])(rs)));
      return show(showNumber)(p2) + ("% " + ("{ " + (x + " }")));
    };
  };
  var face = function(rs) {
    return Just.create(That.create(Sheet("@font-face { " + (fromMaybe("")(renderedInline(rules([])(rs))) + " }\n"))));
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var NonEmptyList = function(x) {
    return x;
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_chunksAcc) {
      return function($copy_v) {
        var $tco_var_chunksAcc = $copy_chunksAcc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(chunksAcc, v) {
          if (v instanceof Cons && (v.value1 instanceof Cons && v.value1.value1 instanceof Cons)) {
            $tco_var_chunksAcc = new Cons(v, chunksAcc);
            $copy_v = v.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v1) {
            if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
              return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
            }
            ;
            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
              return new Cons(f(v1.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v1) {
            return function($copy_acc) {
              var $tco_var_v1 = $copy_v1;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v1, acc) {
                if (v1 instanceof Cons && (v1.value0 instanceof Cons && (v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v1 = v1.value1;
                  $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                  return;
                }
                ;
                $tco_done1 = true;
                return acc;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v1, $copy_acc);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var foldableList = {
    foldr: function(f) {
      return function(b2) {
        var rev3 = function() {
          var go2 = function($copy_acc) {
            return function($copy_v) {
              var $tco_var_acc = $copy_acc;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(acc, v) {
                if (v instanceof Nil) {
                  $tco_done = true;
                  return acc;
                }
                ;
                if (v instanceof Cons) {
                  $tco_var_acc = new Cons(v.value0, acc);
                  $copy_v = v.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [acc.constructor.name, v.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_acc, $copy_v);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $205 = foldl(foldableList)(flip(f))(b2);
        return function($206) {
          return $205(rev3($206));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b2, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b2;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b2)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $207 = append(dictMonoid.Semigroup0())(acc);
          return function($208) {
            return $207(f($208));
          };
        })(mempty(dictMonoid));
      };
    }
  };
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr(foldableList)(Cons.create)(ys)(xs);
      };
    }
  };
  var altList = {
    alt: /* @__PURE__ */ append(semigroupList),
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();

  // output/CSS.Common/index.js
  var browsers = /* @__PURE__ */ function() {
    return new Prefixed([new Tuple("-webkit-", ""), new Tuple("-moz-", ""), new Tuple("-ms-", ""), new Tuple("-o-", ""), new Tuple("", "")]);
  }();

  // output/CSS.Display/index.js
  var valPosition = {
    value: function(v) {
      return v;
    }
  };
  var position = /* @__PURE__ */ key(valPosition)(/* @__PURE__ */ fromString(isStringKey)("position"));
  var absolute = /* @__PURE__ */ fromString(isStringValue)("absolute");

  // output/Data.Exists/index.js
  var runExists = unsafeCoerce2;
  var mkExists = unsafeCoerce2;

  // output/CSS.Size/index.js
  var BasicSize = /* @__PURE__ */ function() {
    function BasicSize2(value0) {
      this.value0 = value0;
    }
    ;
    BasicSize2.create = function(value0) {
      return new BasicSize2(value0);
    };
    return BasicSize2;
  }();
  var SumSize = /* @__PURE__ */ function() {
    function SumSize2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SumSize2.create = function(value0) {
      return function(value1) {
        return new SumSize2(value0, value1);
      };
    };
    return SumSize2;
  }();
  var DiffSize = /* @__PURE__ */ function() {
    function DiffSize2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    DiffSize2.create = function(value0) {
      return function(value1) {
        return new DiffSize2(value0, value1);
      };
    };
    return DiffSize2;
  }();
  var MultSize = /* @__PURE__ */ function() {
    function MultSize2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MultSize2.create = function(value0) {
      return function(value1) {
        return new MultSize2(value0, value1);
      };
    };
    return MultSize2;
  }();
  var DivSize = /* @__PURE__ */ function() {
    function DivSize2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    DivSize2.create = function(value0) {
      return function(value1) {
        return new DivSize2(value0, value1);
      };
    };
    return DivSize2;
  }();
  var sizeToString = function(v) {
    if (v instanceof BasicSize) {
      return plain(v.value0);
    }
    ;
    if (v instanceof SumSize) {
      return runExists(function(a$prime) {
        return runExists(function(b$prime) {
          return "(" + (sizeToString(a$prime) + (" + " + (sizeToString(b$prime) + ")")));
        })(v.value1);
      })(v.value0);
    }
    ;
    if (v instanceof DiffSize) {
      return runExists(function(a$prime) {
        return runExists(function(b$prime) {
          return "(" + (sizeToString(a$prime) + (" - " + (sizeToString(b$prime) + ")")));
        })(v.value1);
      })(v.value0);
    }
    ;
    if (v instanceof MultSize) {
      return runExists(function(b$prime) {
        return "(" + (show(showNumber)(v.value0) + (" * " + (sizeToString(b$prime) + ")")));
      })(v.value1);
    }
    ;
    if (v instanceof DivSize) {
      return runExists(function(b$prime) {
        return "(" + (sizeToString(b$prime) + (" / " + (show(showNumber)(v.value0) + ")")));
      })(v.value1);
    }
    ;
    throw new Error("Failed pattern match at CSS.Size (line 29, column 1 - line 29, column 43): " + [v.constructor.name]);
  };
  var valSize = {
    value: function(v) {
      if (v instanceof BasicSize) {
        return v.value0;
      }
      ;
      return append(semigroupPrefixed)(browsers)(new Plain("calc" + sizeToString(v)));
    }
  };
  var px = function(i2) {
    return new BasicSize(append(semigroupValue)(value(valNumber)(i2))(fromString(isStringValue)("px")));
  };
  var pct = function(i2) {
    return new BasicSize(append(semigroupValue)(value(valNumber)(i2))(fromString(isStringValue)("%")));
  };

  // output/CSS.Geometry/index.js
  var width = /* @__PURE__ */ key(valSize)(/* @__PURE__ */ fromString(isStringKey)("width"));
  var top2 = /* @__PURE__ */ key(valSize)(/* @__PURE__ */ fromString(isStringKey)("top"));
  var left = /* @__PURE__ */ key(valSize)(/* @__PURE__ */ fromString(isStringKey)("left"));
  var height = /* @__PURE__ */ key(valSize)(/* @__PURE__ */ fromString(isStringKey)("height"));

  // output/CSS.Transform/index.js
  var valTransformation = {
    value: function(v) {
      return v;
    }
  };
  var translate = function(x) {
    return function(y) {
      return append(semigroupValue)(fromString(isStringValue)("translate("))(append(semigroupValue)(value(valList(valValue))([value(valSize)(x), value(valSize)(y)]))(fromString(isStringValue)(")")));
    };
  };
  var transform = /* @__PURE__ */ key(valTransformation)(/* @__PURE__ */ fromString(isStringKey)("transform"));

  // output/Gesso.AspectRatio/index.js
  var AspectRatio = /* @__PURE__ */ function() {
    function AspectRatio2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AspectRatio2.create = function(value0) {
      return function(value1) {
        return new AspectRatio2(value0, value1);
      };
    };
    return AspectRatio2;
  }();
  var width2 = function(forHeight) {
    return function(v) {
      return forHeight * v.value0 / v.value1;
    };
  };
  var height2 = function(forWidth) {
    return function(v) {
      return forWidth * v.value1 / v.value0;
    };
  };
  var eqAspectRatio = {
    eq: function(x) {
      return function(y) {
        return x.value0 === y.value0 && x.value1 === y.value1;
      };
    }
  };
  var custom = function(w) {
    return function(h) {
      return new AspectRatio(w, h);
    };
  };

  // output/Halogen.Query.Input/index.js
  var RefUpdate = /* @__PURE__ */ function() {
    function RefUpdate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RefUpdate2.create = function(value0) {
      return function(value1) {
        return new RefUpdate2(value0, value1);
      };
    };
    return RefUpdate2;
  }();
  var Action = /* @__PURE__ */ function() {
    function Action3(value0) {
      this.value0 = value0;
    }
    ;
    Action3.create = function(value0) {
      return new Action3(value0);
    };
    return Action3;
  }();

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a2, r, f) {
    return a2 == null ? r : f(a2);
  }
  function notNull(x) {
    return x;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Halogen.VDom.Machine/index.js
  var Step = /* @__PURE__ */ function() {
    function Step3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Step3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Step3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Step3;
  }();
  var unStep = unsafeCoerce2;
  var step = function(v, a2) {
    return v.value2(v.value1, a2);
  };
  var mkStep = unsafeCoerce2;
  var halt = function(v) {
    return v.value3(v.value1);
  };
  var extract2 = /* @__PURE__ */ unStep(function(v) {
    return v.value0;
  });

  // output/Halogen.VDom.Types/index.js
  var Text = /* @__PURE__ */ function() {
    function Text2(value0) {
      this.value0 = value0;
    }
    ;
    Text2.create = function(value0) {
      return new Text2(value0);
    };
    return Text2;
  }();
  var Elem2 = /* @__PURE__ */ function() {
    function Elem3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Elem3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Elem3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Elem3;
  }();
  var Keyed = /* @__PURE__ */ function() {
    function Keyed2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Keyed2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Keyed2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Keyed2;
  }();
  var Widget = /* @__PURE__ */ function() {
    function Widget2(value0) {
      this.value0 = value0;
    }
    ;
    Widget2.create = function(value0) {
      return new Widget2(value0);
    };
    return Widget2;
  }();
  var Grafted = /* @__PURE__ */ function() {
    function Grafted2(value0) {
      this.value0 = value0;
    }
    ;
    Grafted2.create = function(value0) {
      return new Grafted2(value0);
    };
    return Grafted2;
  }();
  var Graft = /* @__PURE__ */ function() {
    function Graft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Graft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Graft2(value0, value1, value22);
        };
      };
    };
    return Graft2;
  }();
  var unGraft = function(f) {
    return function($55) {
      return f($55);
    };
  };
  var graft = unsafeCoerce2;
  var bifunctorGraft = {
    bimap: function(f) {
      return function(g) {
        return unGraft(function(v) {
          return graft(new Graft(function($57) {
            return f(v.value0($57));
          }, function($58) {
            return g(v.value1($58));
          }, v.value2));
        });
      };
    }
  };
  var runGraft = /* @__PURE__ */ unGraft(function(v) {
    var go2 = function(v2) {
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }
      ;
      if (v2 instanceof Elem2) {
        return new Elem2(v2.value0, v2.value1, v.value0(v2.value2), map(functorArray)(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map(functorArray)(map(functorTuple)(go2))(v2.value3));
      }
      ;
      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }
      ;
      if (v2 instanceof Grafted) {
        return new Grafted(bimap(bifunctorGraft)(v.value0)(v.value1)(v2.value0));
      }
      ;
      throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
    };
    return go2(v.value2);
  });

  // output/Halogen.VDom.Util/foreign.js
  function unsafeGetAny(key2, obj) {
    return obj[key2];
  }
  function unsafeHasAny(key2, obj) {
    return obj.hasOwnProperty(key2);
  }
  function unsafeSetAny(key2, val, obj) {
    obj[key2] = val;
  }
  function forE2(a2, f) {
    var b2 = [];
    for (var i2 = 0; i2 < a2.length; i2++) {
      b2.push(f(i2, a2[i2]));
    }
    return b2;
  }
  function forEachE(a2, f) {
    for (var i2 = 0; i2 < a2.length; i2++) {
      f(a2[i2]);
    }
  }
  function forInE(o, f) {
    var ks = Object.keys(o);
    for (var i2 = 0; i2 < ks.length; i2++) {
      var k = ks[i2];
      f(k, o[k]);
    }
  }
  function diffWithIxE(a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i2 = 0;
    while (1) {
      if (i2 < l1) {
        if (i2 < l2) {
          a3.push(f1(i2, a1[i2], a2[i2]));
        } else {
          f2(i2, a1[i2]);
        }
      } else if (i2 < l2) {
        a3.push(f3(i2, a2[i2]));
      } else {
        break;
      }
      i2++;
    }
    return a3;
  }
  function strMapWithIxE(as, fk, f) {
    var o = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      o[k] = f(k, i2, a2);
    }
    return o;
  }
  function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
    var o2 = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i2, o1[k], a2);
      } else {
        o2[k] = f3(k, i2, a2);
      }
    }
    for (var k in o1) {
      if (k in o2) {
        continue;
      }
      f2(k, o1[k]);
    }
    return o2;
  }
  function refEq2(a2, b2) {
    return a2 === b2;
  }
  function createTextNode(s, doc) {
    return doc.createTextNode(s);
  }
  function setTextContent(s, n) {
    n.textContent = s;
  }
  function createElement(ns, name15, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name15);
    } else {
      return doc.createElement(name15);
    }
  }
  function insertChildIx(i2, a2, b2) {
    var n = b2.childNodes.item(i2) || null;
    if (n !== a2) {
      b2.insertBefore(a2, n);
    }
  }
  function removeChild(a2, b2) {
    if (b2 && a2.parentNode === b2) {
      b2.removeChild(a2);
    }
  }
  function parentNode(a2) {
    return a2.parentNode;
  }
  function setAttribute(ns, attr3, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr3, val);
    } else {
      el.setAttribute(attr3, val);
    }
  }
  function removeAttribute(ns, attr3, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr3);
    } else {
      el.removeAttribute(attr3);
    }
  }
  function hasAttribute(ns, attr3, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr3);
    } else {
      return el.hasAttribute(attr3);
    }
  }
  function addEventListener(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

  // output/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };

  // output/Halogen.VDom.Util/index.js
  var unsafeLookup = unsafeGetAny;
  var unsafeFreeze2 = unsafeCoerce2;
  var pokeMutMap = unsafeSetAny;
  var newMutMap = newImpl;

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name15) {
    return function(doctype) {
      return doctype[name15];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");
  function getBoundingClientRect(el) {
    return function() {
      var rect2 = el.getBoundingClientRect();
      return {
        top: rect2.top,
        right: rect2.right,
        bottom: rect2.bottom,
        left: rect2.left,
        width: rect2.width,
        height: rect2.height,
        x: rect2.x,
        y: rect2.y
      };
    };
  }

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var children = getEffProp("children");
  var _firstElementChild = getEffProp("firstElementChild");
  var _lastElementChild = getEffProp("lastElementChild");
  var childElementCount = getEffProp("childElementCount");
  function _querySelector(selector2) {
    return function(node) {
      return function() {
        return node.querySelector(selector2);
      };
    };
  }

  // output/Web.DOM.ParentNode/index.js
  var querySelector = function(qs) {
    var $0 = map(functorEffect)(toMaybe);
    var $1 = _querySelector(qs);
    return function($2) {
      return $0($1($2));
    };
  };

  // output/Web.DOM.Element/index.js
  var toNode = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy3 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy3("patchWidget", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Widget) {
        var res = step(state3.widget, vdom.value0);
        var res$prime = unStep(function(v) {
          return mkStep(new Step(v.value0, {
            build: state3.build,
            widget: res
          }, $lazy_patchWidget(296), haltWidget));
        })(res);
        return res$prime;
      }
      ;
      haltWidget(state3);
      return state3.build(vdom);
    };
  });
  var patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
  var haltText = function(v) {
    var parent2 = parentNode(v.node);
    return removeChild(v.node, parent2);
  };
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy3("patchText", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchText(82)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Text) {
        if (state3.value === vdom.value0) {
          return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
        }
        ;
        if (otherwise) {
          var nextState = {
            build: state3.build,
            node: state3.node,
            value: vdom.value0
          };
          setTextContent(vdom.value0, state3.node);
          return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
        }
        ;
      }
      ;
      haltText(state3);
      return state3.build(vdom);
    };
  });
  var patchText = /* @__PURE__ */ $lazy_patchText(77);
  var haltKeyed = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forInE(v.children, function(v1, s) {
      return halt(s);
    });
    return halt(v.attrs);
  };
  var haltElem = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forEachE(v.children, halt);
    return halt(v.attrs);
  };
  var eqElemSpec = function(ns1, v, ns2, v1) {
    var $58 = v === v1;
    if ($58) {
      if (ns1 instanceof Just && (ns2 instanceof Just && ns1.value0 === ns2.value0)) {
        return true;
      }
      ;
      if (ns1 instanceof Nothing && ns2 instanceof Nothing) {
        return true;
      }
      ;
      return false;
    }
    ;
    return false;
  };
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy3("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem2 && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        var v1 = length(state3.children);
        if (v1 === 0 && v === 0) {
          var attrs2 = step(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(ix, s, v2) {
          var res = step(s, v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var onThat = function(ix, v2) {
          var res = state3.build(v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
      }
      ;
      haltElem(state3);
      return state3.build(vdom);
    };
  });
  var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy3("patchKeyed", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        if (state3.length === 0 && v === 0) {
          var attrs2 = step(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children,
            length: 0
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(v2, ix$prime, s, v3) {
          var res = step(s, v3.value1);
          insertChildIx(ix$prime, extract2(res), state3.node);
          return res;
        };
        var onThat = function(v2, ix, v3) {
          var res = state3.build(v3.value1);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2,
          length: v
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
      }
      ;
      haltKeyed(state3);
      return state3.build(vdom);
    };
  });
  var patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
  var buildWidget = function(v, build, w) {
    var res = v.buildWidget(v)(w);
    var res$prime = unStep(function(v1) {
      return mkStep(new Step(v1.value0, {
        build,
        widget: res
      }, patchWidget, haltWidget));
    })(res);
    return res$prime;
  };
  var buildText = function(v, build, s) {
    var node = createTextNode(s, v.document);
    var state3 = {
      build,
      node,
      value: s
    };
    return mkStep(new Step(node, state3, patchText, haltText));
  };
  var buildKeyed = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode(el);
    var onChild = function(v1, ix, v2) {
      var res = build(v2.value1);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = strMapWithIxE(ch1, fst, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2,
      length: length(ch1)
    };
    return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
  };
  var buildElem = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode(el);
    var onChild = function(ix, child2) {
      var res = build(child2);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = forE2(ch1, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2
    };
    return mkStep(new Step(node, state3, patchElem, haltElem));
  };
  var buildVDom = function(spec) {
    var $lazy_build = $runtime_lazy3("build", "Halogen.VDom.DOM", function() {
      return function(v) {
        if (v instanceof Text) {
          return buildText(spec, $lazy_build(59), v.value0);
        }
        ;
        if (v instanceof Elem2) {
          return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Keyed) {
          return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Widget) {
          return buildWidget(spec, $lazy_build(62), v.value0);
        }
        ;
        if (v instanceof Grafted) {
          return $lazy_build(63)(runGraft(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
      };
    });
    var build = $lazy_build(58);
    return build;
  };

  // output/Foreign/foreign.js
  function typeOf(value13) {
    return typeof value13;
  }
  var isArray = Array.isArray || function(value13) {
    return Object.prototype.toString.call(value13) === "[object Array]";
  };

  // output/Data.List/index.js
  var reverse2 = /* @__PURE__ */ function() {
    var go2 = function($copy_acc) {
      return function($copy_v) {
        var $tco_var_acc = $copy_acc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(acc, v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return acc;
          }
          ;
          if (v instanceof Cons) {
            $tco_var_acc = new Cons(v.value0, acc);
            $copy_v = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [acc.constructor.name, v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_acc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  }();
  var $$null2 = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };

  // output/Data.List.NonEmpty/index.js
  var singleton5 = /* @__PURE__ */ function() {
    var $169 = singleton3(plusList);
    return function($170) {
      return NonEmptyList($169($170));
    };
  }();
  var cons3 = function(y) {
    return function(v) {
      return new NonEmpty(y, new Cons(v.value0, v.value1));
    };
  };

  // output/Foreign.Object/foreign.js
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Data.Function.Uncurried/foreign.js
  var mkFn2 = function(fn) {
    return function(a2, b2) {
      return fn(a2)(b2);
    };
  };
  var runFn4 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return function(d) {
            return fn(a2, b2, c, d);
          };
        };
      };
    };
  };

  // output/Foreign.Object/index.js
  var lookup2 = /* @__PURE__ */ function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  }();

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener2(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener2(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy4 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Created = /* @__PURE__ */ function() {
    function Created2(value0) {
      this.value0 = value0;
    }
    ;
    Created2.create = function(value0) {
      return new Created2(value0);
    };
    return Created2;
  }();
  var Removed = /* @__PURE__ */ function() {
    function Removed2(value0) {
      this.value0 = value0;
    }
    ;
    Removed2.create = function(value0) {
      return new Removed2(value0);
    };
    return Removed2;
  }();
  var Attribute = /* @__PURE__ */ function() {
    function Attribute2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Attribute2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Attribute2(value0, value1, value22);
        };
      };
    };
    return Attribute2;
  }();
  var Property2 = /* @__PURE__ */ function() {
    function Property3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Property3.create = function(value0) {
      return function(value1) {
        return new Property3(value0, value1);
      };
    };
    return Property3;
  }();
  var Handler = /* @__PURE__ */ function() {
    function Handler2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Handler2.create = function(value0) {
      return function(value1) {
        return new Handler2(value0, value1);
      };
    };
    return Handler2;
  }();
  var Ref = /* @__PURE__ */ function() {
    function Ref2(value0) {
      this.value0 = value0;
    }
    ;
    Ref2.create = function(value0) {
      return new Ref2(value0);
    };
    return Ref2;
  }();
  var unsafeGetProperty = unsafeGetAny;
  var setProperty = unsafeSetAny;
  var removeProperty = function(key2, el) {
    var v = hasAttribute(nullImpl, key2, el);
    if (v) {
      return removeAttribute(nullImpl, key2, el);
    }
    ;
    var v1 = typeOf(unsafeGetAny(key2, el));
    if (v1 === "string") {
      return unsafeSetAny(key2, "", el);
    }
    ;
    if (key2 === "rowSpan") {
      return unsafeSetAny(key2, 1, el);
    }
    ;
    if (key2 === "colSpan") {
      return unsafeSetAny(key2, 1, el);
    }
    ;
    return unsafeSetAny(key2, jsUndefined, el);
  };
  var propToStrKey = function(v) {
    if (v instanceof Attribute && v.value0 instanceof Just) {
      return "attr/" + (v.value0.value0 + (":" + v.value1));
    }
    ;
    if (v instanceof Attribute) {
      return "attr/:" + v.value1;
    }
    ;
    if (v instanceof Property2) {
      return "prop/" + v.value0;
    }
    ;
    if (v instanceof Handler) {
      return "handler/" + v.value0;
    }
    ;
    if (v instanceof Ref) {
      return "ref";
    }
    ;
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
  };
  var propFromString = unsafeCoerce2;
  var propFromInt = unsafeCoerce2;
  var buildProp = function(emit) {
    return function(el) {
      var removeProp = function(prevEvents) {
        return function(v, v1) {
          if (v1 instanceof Attribute) {
            return removeAttribute(toNullable(v1.value0), v1.value1, el);
          }
          ;
          if (v1 instanceof Property2) {
            return removeProperty(v1.value0, el);
          }
          ;
          if (v1 instanceof Handler) {
            var handler3 = unsafeLookup(v1.value0, prevEvents);
            return removeEventListener(v1.value0, fst(handler3), el);
          }
          ;
          if (v1 instanceof Ref) {
            return unit;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
        };
      };
      var mbEmit = function(v) {
        if (v instanceof Just) {
          return emit(v.value0)();
        }
        ;
        return unit;
      };
      var haltProp = function(state3) {
        var v = lookup2("ref")(state3.props);
        if (v instanceof Just && v.value0 instanceof Ref) {
          return mbEmit(v.value0.value0(new Removed(el)));
        }
        ;
        return unit;
      };
      var diffProp = function(prevEvents, events) {
        return function(v, v1, v11, v2) {
          if (v11 instanceof Attribute && v2 instanceof Attribute) {
            var $57 = v11.value2 === v2.value2;
            if ($57) {
              return v2;
            }
            ;
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v11 instanceof Property2 && v2 instanceof Property2) {
            var v4 = refEq2(v11.value1, v2.value1);
            if (v4) {
              return v2;
            }
            ;
            if (v2.value0 === "value") {
              var elVal = unsafeGetProperty("value", el);
              var $66 = refEq2(elVal, v2.value1);
              if ($66) {
                return v2;
              }
              ;
              setProperty(v2.value0, v2.value1, el);
              return v2;
            }
            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v11 instanceof Handler && v2 instanceof Handler) {
            var handler3 = unsafeLookup(v2.value0, prevEvents);
            write(v2.value1)(snd(handler3))();
            pokeMutMap(v2.value0, handler3, events);
            return v2;
          }
          ;
          return v2;
        };
      };
      var applyProp = function(events) {
        return function(v, v1, v2) {
          if (v2 instanceof Attribute) {
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v2 instanceof Property2) {
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v2 instanceof Handler) {
            var v3 = unsafeGetAny(v2.value0, events);
            if (unsafeHasAny(v2.value0, events)) {
              write(v2.value1)(snd(v3))();
              return v2;
            }
            ;
            var ref2 = $$new(v2.value1)();
            var listener = eventListener(function(ev) {
              return function __do2() {
                var f$prime = read(ref2)();
                return mbEmit(f$prime(ev));
              };
            })();
            pokeMutMap(v2.value0, new Tuple(listener, ref2), events);
            addEventListener(v2.value0, listener, el);
            return v2;
          }
          ;
          if (v2 instanceof Ref) {
            mbEmit(v2.value0(new Created(el)));
            return v2;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
        };
      };
      var $lazy_patchProp = $runtime_lazy4("patchProp", "Halogen.VDom.DOM.Prop", function() {
        return function(state3, ps2) {
          var events = newMutMap();
          var onThis = removeProp(state3.events);
          var onThese = diffProp(state3.events, events);
          var onThat = applyProp(events);
          var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
          var nextState = {
            events: unsafeFreeze2(events),
            props
          };
          return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
        };
      });
      var patchProp = $lazy_patchProp(87);
      var renderProp = function(ps1) {
        var events = newMutMap();
        var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
        var state3 = {
          events: unsafeFreeze2(events),
          props: ps1$prime
        };
        return mkStep(new Step(unit, state3, patchProp, haltProp));
      };
      return renderProp;
    };
  };

  // output/Halogen.HTML.Core/index.js
  var HTML = function(x) {
    return x;
  };
  var widget = function($19) {
    return HTML(Widget.create($19));
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var prop = function(dictIsProp) {
    return function(v) {
      var $22 = Property2.create(v);
      var $23 = toPropValue(dictIsProp);
      return function($24) {
        return $22($23($24));
      };
    };
  };
  var isPropString = {
    toPropValue: propFromString
  };
  var isPropInt = {
    toPropValue: propFromInt
  };
  var handler = /* @__PURE__ */ function() {
    return Handler.create;
  }();
  var element2 = function(ns) {
    return function(name15) {
      return function(props) {
        return function(children2) {
          return new Elem2(ns, name15, props, children2);
        };
      };
    };
  };
  var attr = function(ns) {
    return function(v) {
      return Attribute.create(ns)(v);
    };
  };

  // output/Halogen.HTML.Properties/index.js
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var tabIndex = /* @__PURE__ */ prop2(isPropInt)("tabIndex");
  var width3 = /* @__PURE__ */ prop2(isPropInt)("width");
  var id2 = /* @__PURE__ */ prop2(isPropString)("id");
  var height3 = /* @__PURE__ */ prop2(isPropInt)("height");
  var attr2 = /* @__PURE__ */ function() {
    return attr(Nothing.value);
  }();

  // output/Web.UIEvent.MouseEvent/foreign.js
  function pageX(e) {
    return e.pageX;
  }
  function pageY(e) {
    return e.pageY;
  }

  // output/Gesso.Dimensions/index.js
  var Point = function(x) {
    return x;
  };
  var WidthAndHeight = /* @__PURE__ */ function() {
    function WidthAndHeight2(value0) {
      this.value0 = value0;
    }
    ;
    WidthAndHeight2.create = function(value0) {
      return new WidthAndHeight2(value0);
    };
    return WidthAndHeight2;
  }();
  var WidthAndRatio = /* @__PURE__ */ function() {
    function WidthAndRatio2(value0) {
      this.value0 = value0;
    }
    ;
    WidthAndRatio2.create = function(value0) {
      return new WidthAndRatio2(value0);
    };
    return WidthAndRatio2;
  }();
  var HeightAndRatio = /* @__PURE__ */ function() {
    function HeightAndRatio2(value0) {
      this.value0 = value0;
    }
    ;
    HeightAndRatio2.create = function(value0) {
      return new HeightAndRatio2(value0);
    };
    return HeightAndRatio2;
  }();
  var Dimensions = /* @__PURE__ */ function() {
    function Dimensions2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Dimensions2.create = function(value0) {
      return function(value1) {
        return new Dimensions2(value0, value1);
      };
    };
    return Dimensions2;
  }();
  var sizedSize = {
    getWidth: function(v) {
      if (v instanceof WidthAndHeight) {
        return v.value0.width;
      }
      ;
      if (v instanceof WidthAndRatio) {
        return v.value0.width;
      }
      ;
      if (v instanceof HeightAndRatio) {
        return width2(v.value0.height)(v.value0.aspectRatio);
      }
      ;
      throw new Error("Failed pattern match at Gesso.Dimensions (line 169, column 14 - line 172, column 74): " + [v.constructor.name]);
    },
    getHeight: function(v) {
      if (v instanceof WidthAndHeight) {
        return v.value0.height;
      }
      ;
      if (v instanceof WidthAndRatio) {
        return height2(v.value0.width)(v.value0.aspectRatio);
      }
      ;
      if (v instanceof HeightAndRatio) {
        return v.value0.height;
      }
      ;
      throw new Error("Failed pattern match at Gesso.Dimensions (line 173, column 15 - line 176, column 40): " + [v.constructor.name]);
    },
    getRatio: function(v) {
      if (v instanceof WidthAndHeight) {
        return custom(v.value0.width)(v.value0.height);
      }
      ;
      if (v instanceof WidthAndRatio) {
        return v.value0.aspectRatio;
      }
      ;
      if (v instanceof HeightAndRatio) {
        return v.value0.aspectRatio;
      }
      ;
      throw new Error("Failed pattern match at Gesso.Dimensions (line 177, column 14 - line 180, column 50): " + [v.constructor.name]);
    }
  };
  var positionedPoint = {
    getX: function(v) {
      return v.x;
    },
    getY: function(v) {
      return v.y;
    }
  };
  var getY = function(dict) {
    return dict.getY;
  };
  var getX = function(dict) {
    return dict.getX;
  };
  var positionedDimensions = {
    getX: function(v) {
      return getX(positionedPoint)(v.value0);
    },
    getY: function(v) {
      return getY(positionedPoint)(v.value0);
    }
  };
  var getWidth = function(dict) {
    return dict.getWidth;
  };
  var getRatio = function(dict) {
    return dict.getRatio;
  };
  var getHeight = function(dict) {
    return dict.getHeight;
  };
  var sizedDimensions = {
    getWidth: function(v) {
      return getWidth(sizedSize)(v.value1);
    },
    getHeight: function(v) {
      return getHeight(sizedSize)(v.value1);
    },
    getRatio: function(v) {
      return getRatio(sizedSize)(v.value1);
    }
  };
  var toSizeCss = function(dictSized) {
    return function(sized) {
      return discard(discardUnit)(bindStyleM)(width(px(getWidth(dictSized)(sized))))(function() {
        return height(px(getHeight(dictSized)(sized)));
      });
    };
  };
  var toSizeProps = function(dictSized) {
    return function(sized) {
      return [width3(round2(getWidth(dictSized)(sized))), height3(round2(getHeight(dictSized)(sized)))];
    };
  };
  var fromXAndY = Point;
  var origin = /* @__PURE__ */ fromXAndY({
    x: 0,
    y: 0
  });
  var fromWidthAndRatio = /* @__PURE__ */ function() {
    return WidthAndRatio.create;
  }();
  var fromWidthAndHeight = /* @__PURE__ */ function() {
    return WidthAndHeight.create;
  }();
  var sizeless = /* @__PURE__ */ fromWidthAndHeight({
    width: 0,
    height: 0
  });
  var fromPointAndSize = /* @__PURE__ */ function() {
    return Dimensions.create;
  }();
  var fromMouseEvent = function(me) {
    return fromXAndY({
      x: toNumber(pageX(me)),
      y: toNumber(pageY(me))
    });
  };
  var fromHeightAndRatio = /* @__PURE__ */ function() {
    return HeightAndRatio.create;
  }();
  var largestContainedArea = function(aspectRatio) {
    return function(clientRect) {
      var width11 = getWidth(sizedDimensions)(clientRect);
      var keepWidth = fromWidthAndRatio({
        width: width11,
        aspectRatio
      });
      var height11 = getHeight(sizedDimensions)(clientRect);
      var keepHeight = fromHeightAndRatio({
        height: height11,
        aspectRatio
      });
      var go2 = function() {
        if (getHeight(sizedSize)(keepWidth) <= getHeight(sizedDimensions)(clientRect)) {
          return keepWidth;
        }
        ;
        if (otherwise) {
          return keepHeight;
        }
        ;
        throw new Error("Failed pattern match at Gesso.Dimensions (line 156, column 3 - line 158, column 29): " + []);
      }();
      return go2;
    };
  };
  var fromDOMRect = function(v) {
    return new Dimensions(fromXAndY({
      x: v.left,
      y: v.top
    }), fromWidthAndHeight({
      width: v.width,
      height: v.height
    }));
  };
  var eqSize = {
    eq: function(x) {
      return function(y) {
        if (x instanceof WidthAndHeight && y instanceof WidthAndHeight) {
          return x.value0.height === y.value0.height && x.value0.width === y.value0.width;
        }
        ;
        if (x instanceof WidthAndRatio && y instanceof WidthAndRatio) {
          return eq(eqAspectRatio)(x.value0.aspectRatio)(y.value0.aspectRatio) && x.value0.width === y.value0.width;
        }
        ;
        if (x instanceof HeightAndRatio && y instanceof HeightAndRatio) {
          return eq(eqAspectRatio)(x.value0.aspectRatio)(y.value0.aspectRatio) && x.value0.height === y.value0.height;
        }
        ;
        return false;
      };
    }
  };
  var eqPoint = {
    eq: function(x) {
      return function(y) {
        return x.x === y.x && x.y === y.y;
      };
    }
  };
  var eqDimensions = {
    eq: function(x) {
      return function(y) {
        return eq(eqPoint)(x.value0)(y.value0) && eq(eqSize)(x.value1)(y.value1);
      };
    }
  };
  var dimensionedDimensions = {
    Positioned0: function() {
      return positionedDimensions;
    },
    Sized1: function() {
      return sizedDimensions;
    }
  };
  var mkScaler = function(viewBox) {
    return function(v) {
      var transformS = function(dictSized) {
        return function(tw) {
          return function(th2) {
            return function(s) {
              return fromWidthAndHeight({
                width: tw(getWidth(dictSized)(s)),
                height: th2(getHeight(dictSized)(s))
              });
            };
          };
        };
      };
      var transformR = function(tx) {
        return function(ty) {
          return function(tw) {
            return function(th2) {
              return function(v1) {
                return {
                  x: tx(v1.x),
                  y: ty(v1.y),
                  width: tw(v1.width),
                  height: th2(v1.height)
                };
              };
            };
          };
        };
      };
      var transformP = function(dictPositioned) {
        return function(tx) {
          return function(ty) {
            return function(p2) {
              return fromXAndY({
                x: tx(getX(dictPositioned)(p2)),
                y: ty(getY(dictPositioned)(p2))
              });
            };
          };
        };
      };
      var transformD = function(dictDimensioned) {
        return function(tx) {
          return function(ty) {
            return function(tw) {
              return function(th2) {
                return function(d) {
                  return new Dimensions(fromXAndY({
                    x: tx(getX(dictDimensioned.Positioned0())(d)),
                    y: ty(getY(dictDimensioned.Positioned0())(d))
                  }), fromWidthAndHeight({
                    width: tw(getWidth(dictDimensioned.Sized1())(d)),
                    height: th2(getHeight(dictDimensioned.Sized1())(d))
                  }));
                };
              };
            };
          };
        };
      };
      var toRectangle = function(dictDimensioned) {
        return function(d) {
          return {
            x: getX(dictDimensioned.Positioned0())(d),
            y: getY(dictDimensioned.Positioned0())(d),
            width: getWidth(dictDimensioned.Sized1())(d),
            height: getHeight(dictDimensioned.Sized1())(d)
          };
        };
      };
      var actualVB = largestContainedArea(getRatio(sizedDimensions)(viewBox))(v);
      var c = getWidth(sizedDimensions)(viewBox) / getWidth(sizedSize)(actualVB);
      var h$prime = function(v1) {
        return v1 / c;
      };
      var w$prime = function(v1) {
        return v1 / c;
      };
      var margin = {
        w: (getWidth(sizedDimensions)(v) - getWidth(sizedSize)(actualVB)) / 2,
        h: (getHeight(sizedDimensions)(v) - getHeight(sizedSize)(actualVB)) / 2
      };
      var toVb = {
        "x'": function($150) {
          return function(v1) {
            return v1 + getX(positionedDimensions)(viewBox);
          }(function(v1) {
            return v1 * c;
          }(function(v1) {
            return v1 - (getX(positionedDimensions)(v) + margin.w);
          }($150)));
        },
        "y'": function($151) {
          return function(v1) {
            return v1 + getY(positionedDimensions)(viewBox);
          }(function(v1) {
            return v1 * c;
          }(function(v1) {
            return v1 - (getY(positionedDimensions)(v) + margin.h);
          }($151)));
        },
        "w'": function(v1) {
          return v1 * c;
        },
        "h'": function(v1) {
          return v1 * c;
        }
      };
      var x$prime = function($152) {
        return function(v1) {
          return v1 - w$prime(getX(positionedDimensions)(viewBox));
        }(function(v1) {
          return v1 + margin.w;
        }(w$prime($152)));
      };
      var y$prime = function($153) {
        return function(v1) {
          return v1 - h$prime(getY(positionedDimensions)(viewBox));
        }(function(v1) {
          return v1 + margin.h;
        }(h$prime($153)));
      };
      return {
        scale: c,
        viewBox,
        screen: new Dimensions(origin, v.value1),
        x: {
          toVb: toVb["x'"],
          toCr: x$prime
        },
        y: {
          toVb: toVb["y'"],
          toCr: y$prime
        },
        width: {
          toVb: toVb["w'"],
          toCr: w$prime
        },
        height: {
          toVb: toVb["h'"],
          toCr: h$prime
        },
        point: {
          toVb: function(dictPositioned) {
            return transformP(dictPositioned)(toVb["x'"])(toVb["y'"]);
          },
          toCr: function(dictPositioned) {
            return transformP(dictPositioned)(x$prime)(y$prime);
          }
        },
        size: {
          toVb: function(dictSized) {
            return transformS(dictSized)(toVb["w'"])(toVb["h'"]);
          },
          toCr: function(dictSized) {
            return transformS(dictSized)(w$prime)(h$prime);
          }
        },
        dims: {
          toVb: transformD(dimensionedDimensions)(toVb["x'"])(toVb["y'"])(toVb["w'"])(toVb["h'"]),
          toCr: transformD(dimensionedDimensions)(x$prime)(y$prime)(w$prime)(h$prime)
        },
        rect: {
          toVb: transformR(toVb["x'"])(toVb["y'"])(toVb["w'"])(toVb["h'"]),
          toCr: transformR(x$prime)(y$prime)(w$prime)(h$prime)
        },
        toRectangle: function(dictDimensioned) {
          return toRectangle(dictDimensioned);
        }
      };
    };
  };

  // output/Gesso.Application/index.js
  var Fixed = /* @__PURE__ */ function() {
    function Fixed2(value0) {
      this.value0 = value0;
    }
    ;
    Fixed2.create = function(value0) {
      return new Fixed2(value0);
    };
    return Fixed2;
  }();
  var Stretch = /* @__PURE__ */ function() {
    function Stretch2() {
    }
    ;
    Stretch2.value = new Stretch2();
    return Stretch2;
  }();
  var Fullscreen = /* @__PURE__ */ function() {
    function Fullscreen2() {
    }
    ;
    Fullscreen2.value = new Fullscreen2();
    return Fullscreen2;
  }();
  var PureUpdate = /* @__PURE__ */ function() {
    function PureUpdate2(value0) {
      this.value0 = value0;
    }
    ;
    PureUpdate2.create = function(value0) {
      return new PureUpdate2(value0);
    };
    return PureUpdate2;
  }();
  var EffectUpdate = /* @__PURE__ */ function() {
    function EffectUpdate2(value0) {
      this.value0 = value0;
    }
    ;
    EffectUpdate2.create = function(value0) {
      return new EffectUpdate2(value0);
    };
    return EffectUpdate2;
  }();
  var windowCss = /* @__PURE__ */ function() {
    var common = key(valString)(fromString(isStringKey)("outline"))("none");
    var fix2 = function(size4) {
      return discard(discardUnit)(bindStyleM)(toSizeCss(sizedSize)(size4))(function() {
        return common;
      });
    };
    var full = discard(discardUnit)(bindStyleM)(width(pct(100)))(function() {
      return discard(discardUnit)(bindStyleM)(height(pct(100)))(function() {
        return discard(discardUnit)(bindStyleM)(position(absolute))(function() {
          return discard(discardUnit)(bindStyleM)(left(pct(50)))(function() {
            return discard(discardUnit)(bindStyleM)(top2(pct(50)))(function() {
              return discard(discardUnit)(bindStyleM)(transform(translate(pct(-50))(pct(-50))))(function() {
                return common;
              });
            });
          });
        });
      });
    });
    var stretched = discard(discardUnit)(bindStyleM)(width(pct(100)))(function() {
      return discard(discardUnit)(bindStyleM)(height(pct(100)))(function() {
        return common;
      });
    });
    return function(v) {
      if (v instanceof Fixed) {
        return fix2(v.value0);
      }
      ;
      if (v instanceof Stretch) {
        return stretched;
      }
      ;
      if (v instanceof Fullscreen) {
        return full;
      }
      ;
      throw new Error("Failed pattern match at Gesso.Application (line 157, column 13 - line 160, column 21): " + [v.constructor.name]);
    };
  }();
  var runUpdate = function(delta2) {
    return function(scaler) {
      return function(state3) {
        return function(v) {
          if (v instanceof PureUpdate) {
            return pure(applicativeEffect)(v.value0(delta2)(scaler)(state3));
          }
          ;
          if (v instanceof EffectUpdate) {
            return v.value0(delta2)(scaler)(state3);
          }
          ;
          throw new Error("Failed pattern match at Gesso.Application (line 141, column 3 - line 143, column 45): " + [v.constructor.name]);
        };
      };
    };
  };
  var pureUpdate = /* @__PURE__ */ function() {
    return PureUpdate.create;
  }();
  var fullscreen = /* @__PURE__ */ function() {
    return Fullscreen.value;
  }();
  var fixed = /* @__PURE__ */ function() {
    return Fixed.create;
  }();
  var effectUpdate = /* @__PURE__ */ function() {
    return EffectUpdate.create;
  }();
  var defaultApp = /* @__PURE__ */ function() {
    return {
      window: fixed(sizeless),
      render: function(v) {
        return function(v1) {
          return function(v2) {
            return function(v3) {
              return pure(applicativeEffect)(unit);
            };
          };
        };
      },
      update: new PureUpdate(function(v) {
        return function(v1) {
          return function(v2) {
            return Nothing.value;
          };
        };
      }),
      output: function(v) {
        return function(v1) {
          return function(v2) {
            return function(v3) {
              return Nothing.value;
            };
          };
        };
      },
      input: function(v) {
        return function(v1) {
          return function(v2) {
            return function(v3) {
              return Nothing.value;
            };
          };
        };
      }
    };
  }();

  // output/Web.HTML.Event.EventTypes/index.js
  var domcontentloaded = "DOMContentLoaded";

  // output/Web.UIEvent.MouseEvent.EventTypes/index.js
  var mousemove = "mousemove";
  var mousedown = "mousedown";

  // output/Halogen.HTML.Events/index.js
  var mouseHandler = unsafeCoerce2;
  var handler2 = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return new Just(new Action(f(ev)));
      });
    };
  };
  var onMouseDown = /* @__PURE__ */ function() {
    var $15 = handler2(mousedown);
    return function($16) {
      return $15(mouseHandler($16));
    };
  }();
  var onMouseMove = /* @__PURE__ */ function() {
    var $21 = handler2(mousemove);
    return function($22) {
      return $21(mouseHandler($22));
    };
  }();

  // output/Gesso.Interactions/index.js
  var Pure = /* @__PURE__ */ function() {
    function Pure3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Pure3.create = function(value0) {
      return function(value1) {
        return new Pure3(value0, value1);
      };
    };
    return Pure3;
  }();
  var Effectful = /* @__PURE__ */ function() {
    function Effectful2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Effectful2.create = function(value0) {
      return function(value1) {
        return new Effectful2(value0, value1);
      };
    };
    return Effectful2;
  }();
  var toProps = function(toCallback) {
    return function(v) {
      var toProp = function(v1) {
        if (v1 instanceof Pure) {
          return v1.value0(function($23) {
            return toCallback(pureUpdate(v1.value1($23)));
          });
        }
        ;
        if (v1 instanceof Effectful) {
          return v1.value0(function($24) {
            return toCallback(effectUpdate(v1.value1($24)));
          });
        }
        ;
        throw new Error("Failed pattern match at Gesso.Interactions (line 109, column 12 - line 114, column 60): " + [v1.constructor.name]);
      };
      return append(semigroupArray)(map(functorArray)(toProp)(v.base))(append(semigroupArray)(map(functorArray)(toProp)(v.clipboard))(append(semigroupArray)(map(functorArray)(toProp)(v.focus))(append(semigroupArray)(map(functorArray)(toProp)(v.keyboard))(append(semigroupArray)(map(functorArray)(toProp)(v.touch))(append(semigroupArray)(map(functorArray)(toProp)(v.drag))(append(semigroupArray)(map(functorArray)(toProp)(v.mouse))(map(functorArray)(toProp)(v.wheel))))))));
    };
  };
  var mkInteraction = /* @__PURE__ */ function() {
    return Pure.create;
  }();
  var mousePosition = /* @__PURE__ */ function() {
    var getMousePos = function(event) {
      return function(v) {
        return function(v1) {
          return function(state3) {
            return new Just(function() {
              var $20 = {};
              for (var $21 in state3) {
                if ({}.hasOwnProperty.call(state3, $21)) {
                  $20[$21] = state3[$21];
                }
                ;
              }
              ;
              $20.mousePos = new Just(fromMouseEvent(event));
              return $20;
            }());
          };
        };
      };
    };
    return mkInteraction(onMouseMove)(getMousePos);
  }();
  var $$default = {
    base: [],
    clipboard: [],
    focus: [],
    keyboard: [],
    touch: [],
    drag: [],
    mouse: [],
    wheel: []
  };

  // output/Gesso.Time/foreign.js
  function _requestAnimationFrame(fn) {
    return function(window2) {
      return function() {
        return window2.requestAnimationFrame(fn);
      };
    };
  }

  // output/Gesso.Time/index.js
  var Timestamp = function(x) {
    return x;
  };
  var RequestAnimationFrameId = function(x) {
    return x;
  };
  var toPrev = /* @__PURE__ */ coerce();
  var requestAnimationFrame = function(fn) {
    var $4 = map(functorEffect)(RequestAnimationFrameId);
    var $5 = _requestAnimationFrame(function($7) {
      return fn(Timestamp($7))();
    });
    return function($6) {
      return $4($5($6));
    };
  };
  var delta = function(v) {
    return function(v1) {
      return {
        prev: v1,
        now: v,
        delta: v - v1
      };
    };
  };

  // output/Graphics.Canvas/foreign.js
  function getCanvasElementByIdImpl(id3, Just2, Nothing2) {
    return function() {
      var el = document.getElementById(id3);
      if (el && el instanceof HTMLCanvasElement) {
        return Just2(el);
      } else {
        return Nothing2;
      }
    };
  }
  function getContext2D(c) {
    return function() {
      return c.getContext("2d");
    };
  }
  function setLineWidth(ctx) {
    return function(width11) {
      return function() {
        ctx.lineWidth = width11;
      };
    };
  }
  function setFillStyle(ctx) {
    return function(style2) {
      return function() {
        ctx.fillStyle = style2;
      };
    };
  }
  function setStrokeStyle(ctx) {
    return function(style2) {
      return function() {
        ctx.strokeStyle = style2;
      };
    };
  }
  function beginPath(ctx) {
    return function() {
      ctx.beginPath();
    };
  }
  function stroke(ctx) {
    return function() {
      ctx.stroke();
    };
  }
  function lineTo(ctx) {
    return function(x) {
      return function(y) {
        return function() {
          ctx.lineTo(x, y);
        };
      };
    };
  }
  function moveTo(ctx) {
    return function(x) {
      return function(y) {
        return function() {
          ctx.moveTo(x, y);
        };
      };
    };
  }
  function arc(ctx) {
    return function(a2) {
      return function() {
        ctx.arc(a2.x, a2.y, a2.radius, a2.start, a2.end, a2.useCounterClockwise);
      };
    };
  }
  function fillRect(ctx) {
    return function(r) {
      return function() {
        ctx.fillRect(r.x, r.y, r.width, r.height);
      };
    };
  }
  function setTextAlignImpl(ctx) {
    return function(textAlign) {
      return function() {
        ctx.textAlign = textAlign;
      };
    };
  }
  function setFont(ctx) {
    return function(fontspec) {
      return function() {
        ctx.font = fontspec;
      };
    };
  }
  function fillText(ctx) {
    return function(text6) {
      return function(x) {
        return function(y) {
          return function() {
            ctx.fillText(text6, x, y);
          };
        };
      };
    };
  }

  // output/Graphics.Canvas/index.js
  var AlignLeft = /* @__PURE__ */ function() {
    function AlignLeft2() {
    }
    ;
    AlignLeft2.value = new AlignLeft2();
    return AlignLeft2;
  }();
  var AlignRight = /* @__PURE__ */ function() {
    function AlignRight2() {
    }
    ;
    AlignRight2.value = new AlignRight2();
    return AlignRight2;
  }();
  var AlignCenter = /* @__PURE__ */ function() {
    function AlignCenter2() {
    }
    ;
    AlignCenter2.value = new AlignCenter2();
    return AlignCenter2;
  }();
  var AlignStart = /* @__PURE__ */ function() {
    function AlignStart2() {
    }
    ;
    AlignStart2.value = new AlignStart2();
    return AlignStart2;
  }();
  var AlignEnd = /* @__PURE__ */ function() {
    function AlignEnd2() {
    }
    ;
    AlignEnd2.value = new AlignEnd2();
    return AlignEnd2;
  }();
  var strokePath = function(ctx) {
    return function(path) {
      return function __do2() {
        beginPath(ctx)();
        var a2 = path();
        stroke(ctx)();
        return a2;
      };
    };
  };
  var setTextAlign = function(ctx) {
    return function(textalign) {
      var toString = function(v) {
        if (v instanceof AlignLeft) {
          return "left";
        }
        ;
        if (v instanceof AlignRight) {
          return "right";
        }
        ;
        if (v instanceof AlignCenter) {
          return "center";
        }
        ;
        if (v instanceof AlignStart) {
          return "start";
        }
        ;
        if (v instanceof AlignEnd) {
          return "end";
        }
        ;
        throw new Error("Failed pattern match at Graphics.Canvas (line 531, column 5 - line 531, column 32): " + [v.constructor.name]);
      };
      return setTextAlignImpl(ctx)(toString(textalign));
    };
  };
  var getCanvasElementById = function(elId) {
    return getCanvasElementByIdImpl(elId, Just.create, Nothing.value);
  };

  // output/Data.Coyoneda/index.js
  var CoyonedaF = /* @__PURE__ */ function() {
    function CoyonedaF2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CoyonedaF2.create = function(value0) {
      return function(value1) {
        return new CoyonedaF2(value0, value1);
      };
    };
    return CoyonedaF2;
  }();
  var unCoyoneda = function(f) {
    return function(v) {
      return runExists(function(v1) {
        return f(v1.value0)(v1.value1);
      })(v);
    };
  };
  var coyoneda = function(k) {
    return function(fi) {
      return mkExists(new CoyonedaF(k, fi));
    };
  };
  var functorCoyoneda = {
    map: function(f) {
      return function(v) {
        return runExists(function(v1) {
          return coyoneda(function($83) {
            return f(v1.value0($83));
          })(v1.value1);
        })(v);
      };
    }
  };
  var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));

  // output/Data.Map.Internal/index.js
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Two = /* @__PURE__ */ function() {
    function Two2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Two2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Two2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Two2;
  }();
  var Three = /* @__PURE__ */ function() {
    function Three2(value0, value1, value22, value32, value42, value52, value62) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
    }
    ;
    Three2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return new Three2(value0, value1, value22, value32, value42, value52, value62);
                };
              };
            };
          };
        };
      };
    };
    return Three2;
  }();
  var TwoLeft = /* @__PURE__ */ function() {
    function TwoLeft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoLeft2(value0, value1, value22);
        };
      };
    };
    return TwoLeft2;
  }();
  var TwoRight = /* @__PURE__ */ function() {
    function TwoRight2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoRight2(value0, value1, value22);
        };
      };
    };
    return TwoRight2;
  }();
  var ThreeLeft = /* @__PURE__ */ function() {
    function ThreeLeft2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeLeft2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeLeft2;
  }();
  var ThreeMiddle = /* @__PURE__ */ function() {
    function ThreeMiddle2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeMiddle2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeMiddle2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeMiddle2;
  }();
  var ThreeRight = /* @__PURE__ */ function() {
    function ThreeRight2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeRight2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeRight2;
  }();
  var KickUp = /* @__PURE__ */ function() {
    function KickUp2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    KickUp2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new KickUp2(value0, value1, value22, value32);
          };
        };
      };
    };
    return KickUp2;
  }();
  var lookup3 = function(dictOrd) {
    return function(k) {
      var comp = compare(dictOrd);
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Two) {
            var v2 = comp(k)(v.value1);
            if (v2 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            if (v2 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          if (v instanceof Three) {
            var v3 = comp(k)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = comp(k)(v.value4);
            if (v4 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value5);
            }
            ;
            if (v3 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            if (v4 instanceof GT) {
              $copy_v = v.value6;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var fromZipper = function($copy_dictOrd) {
    return function($copy_v) {
      return function($copy_tree) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, tree) {
          if (v instanceof Nil) {
            $tco_done = true;
            return tree;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, tree.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_tree);
        }
        ;
        return $tco_result;
      };
    };
  };
  var insert2 = function(dictOrd) {
    return function(k) {
      return function(v) {
        var up = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }
              ;
              if (v1 instanceof Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }
                ;
                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        var comp = compare(dictOrd);
        var down = function($copy_ctx) {
          return function($copy_v1) {
            var $tco_var_ctx = $copy_ctx;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(ctx, v1) {
              if (v1 instanceof Leaf) {
                $tco_done1 = true;
                return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
              }
              ;
              if (v1 instanceof Two) {
                var v2 = comp(k)(v1.value1);
                if (v2 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper(dictOrd)(ctx)(new Two(v1.value0, k, v, v1.value3));
                }
                ;
                if (v2 instanceof LT) {
                  $tco_var_ctx = new Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                $copy_v1 = v1.value3;
                return;
              }
              ;
              if (v1 instanceof Three) {
                var v3 = comp(k)(v1.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                }
                ;
                var v4 = comp(k)(v1.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value3;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                $copy_v1 = v1.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [ctx.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_ctx, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return down(Nil.value);
      };
    };
  };
  var pop = function(dictOrd) {
    return function(k) {
      var up = function($copy_ctxs) {
        return function($copy_tree) {
          var $tco_var_ctxs = $copy_ctxs;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(ctxs, tree) {
            if (ctxs instanceof Nil) {
              $tco_done = true;
              return tree;
            }
            ;
            if (ctxs instanceof Cons) {
              if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              $tco_done = true;
              return unsafeCrashWith("The impossible happened in partial function `up`.");
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): " + [ctxs.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
          }
          ;
          return $tco_result;
        };
      };
      var removeMaxNode = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
              $tco_done1 = true;
              return up(ctx)(Leaf.value);
            }
            ;
            if (m instanceof Two) {
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
              $tco_done1 = true;
              return up(new Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
            }
            ;
            if (m instanceof Three) {
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            $tco_done1 = true;
            return unsafeCrashWith("The impossible happened in partial function `removeMaxNode`.");
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      var maxNode = function($copy_m) {
        var $tco_done2 = false;
        var $tco_result;
        function $tco_loop(m) {
          if (m instanceof Two && m.value3 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value1,
              value: m.value2
            };
          }
          ;
          if (m instanceof Two) {
            $copy_m = m.value3;
            return;
          }
          ;
          if (m instanceof Three && m.value6 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value4,
              value: m.value5
            };
          }
          ;
          if (m instanceof Three) {
            $copy_m = m.value6;
            return;
          }
          ;
          $tco_done2 = true;
          return unsafeCrashWith("The impossible happened in partial function `maxNode`.");
        }
        ;
        while (!$tco_done2) {
          $tco_result = $tco_loop($copy_m);
        }
        ;
        return $tco_result;
      };
      var comp = compare(dictOrd);
      var down = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done3 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Leaf) {
              $tco_done3 = true;
              return Nothing.value;
            }
            ;
            if (m instanceof Two) {
              var v = comp(k)(m.value1);
              if (m.value3 instanceof Leaf && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, up(ctx)(Leaf.value)));
              }
              ;
              if (v instanceof EQ) {
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new TwoLeft(max6.key, max6.value, m.value3), ctx))(m.value0)));
              }
              ;
              if (v instanceof LT) {
                $tco_var_ctx = new Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three) {
              var leaves = function() {
                if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                  return true;
                }
                ;
                return false;
              }();
              var v = comp(k)(m.value4);
              var v3 = comp(k)(m.value1);
              if (leaves && v3 instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
              }
              ;
              if (leaves && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
              }
              ;
              if (v3 instanceof EQ) {
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new ThreeLeft(max6.key, max6.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }
              ;
              if (v instanceof EQ) {
                var max6 = maxNode(m.value3);
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, removeMaxNode(new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max6.key, max6.value, m.value6), ctx))(m.value3)));
              }
              ;
              if (v3 instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              if (v3 instanceof GT && v instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value3;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): " + [m.constructor.name]);
          }
          ;
          while (!$tco_done3) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      return down(Nil.value);
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(f(m.value5)(foldr(foldableMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty(dictMonoid);
          }
          ;
          if (m instanceof Two) {
            return append(dictMonoid.Semigroup0())(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append(dictMonoid.Semigroup0())(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append(dictMonoid.Semigroup0())(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append(dictMonoid.Semigroup0())(f(m.value2))(append(dictMonoid.Semigroup0())(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append(dictMonoid.Semigroup0())(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [m.constructor.name]);
        };
      };
    }
  };
  var empty3 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var $$delete2 = function(dictOrd) {
    return function(k) {
      return function(m) {
        return maybe(m)(snd)(pop(dictOrd)(k)(m));
      };
    };
  };
  var alter = function(dictOrd) {
    return function(f) {
      return function(k) {
        return function(m) {
          var v = f(lookup3(dictOrd)(k)(m));
          if (v instanceof Nothing) {
            return $$delete2(dictOrd)(k)(m);
          }
          ;
          if (v instanceof Just) {
            return insert2(dictOrd)(k)(v.value0)(m);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): " + [v.constructor.name]);
        };
      };
    };
  };

  // output/Halogen.Data.Slot/index.js
  var foreachSlot = function(dictApplicative) {
    return function(v) {
      return function(k) {
        return traverse_(dictApplicative)(foldableMap)(function($33) {
          return k($33);
        })(v);
      };
    };
  };
  var empty4 = empty3;

  // output/Control.Applicative.Free/index.js
  var Pure2 = /* @__PURE__ */ function() {
    function Pure3(value0) {
      this.value0 = value0;
    }
    ;
    Pure3.create = function(value0) {
      return new Pure3(value0);
    };
    return Pure3;
  }();
  var Lift = /* @__PURE__ */ function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  }();
  var Ap = /* @__PURE__ */ function() {
    function Ap2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Ap2.create = function(value0) {
      return function(value1) {
        return new Ap2(value0, value1);
      };
    };
    return Ap2;
  }();
  var mkAp = function(fba) {
    return function(fb) {
      return new Ap(fba, fb);
    };
  };
  var liftFreeAp = /* @__PURE__ */ function() {
    return Lift.create;
  }();
  var goLeft = function($copy_dictApplicative) {
    return function($copy_fStack) {
      return function($copy_valStack) {
        return function($copy_nat) {
          return function($copy_func) {
            return function($copy_count) {
              var $tco_var_dictApplicative = $copy_dictApplicative;
              var $tco_var_fStack = $copy_fStack;
              var $tco_var_valStack = $copy_valStack;
              var $tco_var_nat = $copy_nat;
              var $tco_var_func = $copy_func;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(dictApplicative, fStack, valStack, nat, func, count) {
                if (func instanceof Pure2) {
                  $tco_done = true;
                  return new Tuple(new Cons({
                    func: pure(dictApplicative)(func.value0),
                    count
                  }, fStack), valStack);
                }
                ;
                if (func instanceof Lift) {
                  $tco_done = true;
                  return new Tuple(new Cons({
                    func: nat(func.value0),
                    count
                  }, fStack), valStack);
                }
                ;
                if (func instanceof Ap) {
                  $tco_var_dictApplicative = dictApplicative;
                  $tco_var_fStack = fStack;
                  $tco_var_valStack = cons3(func.value1)(valStack);
                  $tco_var_nat = nat;
                  $tco_var_func = func.value0;
                  $copy_count = count + 1 | 0;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_dictApplicative, $tco_var_fStack, $tco_var_valStack, $tco_var_nat, $tco_var_func, $copy_count);
              }
              ;
              return $tco_result;
            };
          };
        };
      };
    };
  };
  var goApply = function($copy_dictApplicative) {
    return function($copy_fStack) {
      return function($copy_vals) {
        return function($copy_gVal) {
          var $tco_var_dictApplicative = $copy_dictApplicative;
          var $tco_var_fStack = $copy_fStack;
          var $tco_var_vals = $copy_vals;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(dictApplicative, fStack, vals, gVal) {
            if (fStack instanceof Nil) {
              $tco_done = true;
              return new Left(gVal);
            }
            ;
            if (fStack instanceof Cons) {
              var gRes = apply(dictApplicative.Apply0())(fStack.value0.func)(gVal);
              var $14 = fStack.value0.count === 1;
              if ($14) {
                if (fStack.value1 instanceof Nil) {
                  $tco_done = true;
                  return new Left(gRes);
                }
                ;
                $tco_var_dictApplicative = dictApplicative;
                $tco_var_fStack = fStack.value1;
                $tco_var_vals = vals;
                $copy_gVal = gRes;
                return;
              }
              ;
              if (vals instanceof Nil) {
                $tco_done = true;
                return new Left(gRes);
              }
              ;
              if (vals instanceof Cons) {
                $tco_done = true;
                return new Right(new Tuple(new Cons({
                  func: gRes,
                  count: fStack.value0.count - 1 | 0
                }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_dictApplicative, $tco_var_fStack, $tco_var_vals, $copy_gVal);
          }
          ;
          return $tco_result;
        };
      };
    };
  };
  var functorFreeAp = {
    map: function(f) {
      return function(x) {
        return mkAp(new Pure2(f))(x);
      };
    }
  };
  var foldFreeAp = function(dictApplicative) {
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure2) {
              var v1 = goApply(dictApplicative)(v.value0)(v.value1.value1)(pure(dictApplicative)(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Lift) {
              var v1 = goApply(dictApplicative)(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Ap) {
              var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
              $copy_v = goLeft(dictApplicative)(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2(new Tuple(Nil.value, singleton5(z)));
      };
    };
  };
  var retractFreeAp = function(dictApplicative) {
    return foldFreeAp(dictApplicative)(identity(categoryFn));
  };
  var applyFreeAp = {
    apply: function(fba) {
      return function(fb) {
        return mkAp(fba)(fb);
      };
    },
    Functor0: function() {
      return functorFreeAp;
    }
  };
  var applicativeFreeAp = /* @__PURE__ */ function() {
    return {
      pure: Pure2.create,
      Apply0: function() {
        return applyFreeAp;
      }
    };
  }();
  var hoistFreeAp = function(f) {
    return foldFreeAp(applicativeFreeAp)(function($37) {
      return liftFreeAp(f($37));
    });
  };

  // output/Data.CatQueue/index.js
  var CatQueue = /* @__PURE__ */ function() {
    function CatQueue2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatQueue2.create = function(value0) {
      return function(value1) {
        return new CatQueue2(value0, value1);
      };
    };
    return CatQueue2;
  }();
  var uncons3 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
        $tco_done = true;
        return Nothing.value;
      }
      ;
      if (v.value0 instanceof Nil) {
        $copy_v = new CatQueue(reverse2(v.value1), Nil.value);
        return;
      }
      ;
      if (v.value0 instanceof Cons) {
        $tco_done = true;
        return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var snoc3 = function(v) {
    return function(a2) {
      return new CatQueue(v.value0, new Cons(a2, v.value1));
    };
  };
  var $$null3 = function(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var empty5 = /* @__PURE__ */ function() {
    return new CatQueue(Nil.value, Nil.value);
  }();

  // output/Data.CatList/index.js
  var CatNil = /* @__PURE__ */ function() {
    function CatNil2() {
    }
    ;
    CatNil2.value = new CatNil2();
    return CatNil2;
  }();
  var CatCons = /* @__PURE__ */ function() {
    function CatCons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatCons2.create = function(value0) {
      return function(value1) {
        return new CatCons2(value0, value1);
      };
    };
    return CatCons2;
  }();
  var link = function(v) {
    return function(v1) {
      if (v instanceof CatNil) {
        return v1;
      }
      ;
      if (v1 instanceof CatNil) {
        return v;
      }
      ;
      if (v instanceof CatCons) {
        return new CatCons(v.value0, snoc3(v.value1)(v1));
      }
      ;
      throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var foldr2 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl2 = function($copy_v) {
          return function($copy_c) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_var_c = $copy_c;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, c, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return c;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = v;
                  $tco_var_c = v(c)(v1.value0);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, c.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_c, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
        };
        var go2 = function($copy_xs) {
          return function($copy_ys) {
            var $tco_var_xs = $copy_xs;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(xs, ys) {
              var v = uncons3(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl2(function(x) {
                  return function(i2) {
                    return i2(x);
                  };
                })(b2)(ys);
              }
              ;
              if (v instanceof Just) {
                $tco_var_xs = v.value0.value1;
                $copy_ys = new Cons(k(v.value0.value0), ys);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_xs, $copy_ys);
            }
            ;
            return $tco_result;
          };
        };
        return go2(q2)(Nil.value);
      };
    };
  };
  var uncons4 = function(v) {
    if (v instanceof CatNil) {
      return Nothing.value;
    }
    ;
    if (v instanceof CatCons) {
      return new Just(new Tuple(v.value0, function() {
        var $45 = $$null3(v.value1);
        if ($45) {
          return CatNil.value;
        }
        ;
        return foldr2(link)(CatNil.value)(v.value1);
      }()));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
  };
  var empty6 = /* @__PURE__ */ function() {
    return CatNil.value;
  }();
  var append2 = link;
  var semigroupCatList = {
    append: append2
  };
  var snoc4 = function(cat) {
    return function(a2) {
      return append2(cat)(new CatCons(a2, empty5));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy5 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Free = /* @__PURE__ */ function() {
    function Free2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Free2.create = function(value0) {
      return function(value1) {
        return new Free2(value0, value1);
      };
    };
    return Free2;
  }();
  var Return = /* @__PURE__ */ function() {
    function Return2(value0) {
      this.value0 = value0;
    }
    ;
    Return2.create = function(value0) {
      return new Return2(value0);
    };
    return Return2;
  }();
  var Bind = /* @__PURE__ */ function() {
    function Bind2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Bind2.create = function(value0) {
      return function(value1) {
        return new Bind2(value0, value1);
      };
    };
    return Bind2;
  }();
  var toView = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      var runExpF = function(v22) {
        return v22;
      };
      var concatF = function(v22) {
        return function(r) {
          return new Free(v22.value0, append(semigroupCatList)(v22.value1)(r));
        };
      };
      if (v.value0 instanceof Return) {
        var v2 = uncons4(v.value1);
        if (v2 instanceof Nothing) {
          $tco_done = true;
          return new Return(v.value0.value0);
        }
        ;
        if (v2 instanceof Just) {
          $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
          return;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
      }
      ;
      if (v.value0 instanceof Bind) {
        $tco_done = true;
        return new Bind(v.value0.value0, function(a2) {
          return concatF(v.value0.value1(a2))(v.value1);
        });
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var fromView = function(f) {
    return new Free(f, empty6);
  };
  var freeMonad = {
    Applicative0: function() {
      return freeApplicative;
    },
    Bind1: function() {
      return freeBind;
    }
  };
  var freeFunctor = {
    map: function(k) {
      return function(f) {
        return bindFlipped(freeBind)(function() {
          var $119 = pure(freeApplicative);
          return function($120) {
            return $119(k($120));
          };
        }())(f);
      };
    }
  };
  var freeBind = {
    bind: function(v) {
      return function(k) {
        return new Free(v.value0, snoc4(v.value1)(k));
      };
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var freeApplicative = {
    pure: function($121) {
      return fromView(Return.create($121));
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy5("freeApply", "Control.Monad.Free", function() {
    return {
      apply: ap(freeMonad),
      Functor0: function() {
        return freeFunctor;
      }
    };
  });
  var liftF = function(f) {
    return fromView(new Bind(f, function() {
      var $122 = pure(freeApplicative);
      return function($123) {
        return $122($123);
      };
    }()));
  };
  var foldFree = function(dictMonadRec) {
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(Done.create)(pure(dictMonadRec.Monad0().Applicative0())(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(function($135) {
            return Loop.create(v.value1($135));
          })(k(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };
      return tailRecM(dictMonadRec)(go2);
    };
  };

  // output/Halogen.Query.ChildQuery/index.js
  var unChildQueryBox = unsafeCoerce2;

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a2) {
    return function(b2) {
      return a2 === b2;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/Halogen.Subscription/index.js
  var unsubscribe = function(v) {
    return v;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(function() {
        var $55 = $$void(functorEffect);
        return function($56) {
          return $55(k($56));
        };
      }());
    };
  };
  var notify = function(v) {
    return function(a2) {
      return v(a2);
    };
  };
  var makeEmitter = /* @__PURE__ */ coerce();
  var create = function __do() {
    var subscribers = $$new([])();
    return {
      emitter: function(k) {
        return function __do2() {
          modify_(function(v) {
            return append(semigroupArray)(v)([k]);
          })(subscribers)();
          return modify_(deleteBy(unsafeRefEq)(k))(subscribers);
        };
      },
      listener: function(a2) {
        return bind(bindEffect)(read(subscribers))(traverse_(applicativeEffect)(foldableArray)(function(k) {
          return k(a2);
        }));
      }
    };
  };

  // output/Halogen.Query.HalogenM/index.js
  var SubscriptionId = function(x) {
    return x;
  };
  var ForkId = function(x) {
    return x;
  };
  var State = /* @__PURE__ */ function() {
    function State2(value0) {
      this.value0 = value0;
    }
    ;
    State2.create = function(value0) {
      return new State2(value0);
    };
    return State2;
  }();
  var Subscribe = /* @__PURE__ */ function() {
    function Subscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Subscribe2.create = function(value0) {
      return function(value1) {
        return new Subscribe2(value0, value1);
      };
    };
    return Subscribe2;
  }();
  var Unsubscribe = /* @__PURE__ */ function() {
    function Unsubscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Unsubscribe2.create = function(value0) {
      return function(value1) {
        return new Unsubscribe2(value0, value1);
      };
    };
    return Unsubscribe2;
  }();
  var Lift2 = /* @__PURE__ */ function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  }();
  var ChildQuery2 = /* @__PURE__ */ function() {
    function ChildQuery3(value0) {
      this.value0 = value0;
    }
    ;
    ChildQuery3.create = function(value0) {
      return new ChildQuery3(value0);
    };
    return ChildQuery3;
  }();
  var Raise = /* @__PURE__ */ function() {
    function Raise2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Raise2.create = function(value0) {
      return function(value1) {
        return new Raise2(value0, value1);
      };
    };
    return Raise2;
  }();
  var Par = /* @__PURE__ */ function() {
    function Par2(value0) {
      this.value0 = value0;
    }
    ;
    Par2.create = function(value0) {
      return new Par2(value0);
    };
    return Par2;
  }();
  var Fork = /* @__PURE__ */ function() {
    function Fork2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Fork2.create = function(value0) {
      return function(value1) {
        return new Fork2(value0, value1);
      };
    };
    return Fork2;
  }();
  var Join = /* @__PURE__ */ function() {
    function Join2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Join2.create = function(value0) {
      return function(value1) {
        return new Join2(value0, value1);
      };
    };
    return Join2;
  }();
  var Kill = /* @__PURE__ */ function() {
    function Kill2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Kill2.create = function(value0) {
      return function(value1) {
        return new Kill2(value0, value1);
      };
    };
    return Kill2;
  }();
  var GetRef = /* @__PURE__ */ function() {
    function GetRef2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    GetRef2.create = function(value0) {
      return function(value1) {
        return new GetRef2(value0, value1);
      };
    };
    return GetRef2;
  }();
  var HalogenM = function(x) {
    return x;
  };
  var unsubscribe2 = function(sid) {
    return liftF(new Unsubscribe(sid, unit));
  };
  var subscribe2 = function(es) {
    return liftF(new Subscribe(function(v) {
      return es;
    }, identity(categoryFn)));
  };
  var raise = function(o) {
    return liftF(new Raise(o, unit));
  };
  var ordSubscriptionId = ordInt;
  var ordForkId = ordInt;
  var monadHalogenM = freeMonad;
  var monadStateHalogenM = {
    state: function($144) {
      return HalogenM(liftF(State.create($144)));
    },
    Monad0: function() {
      return monadHalogenM;
    }
  };
  var monadEffectHalogenM = function(dictMonadEffect) {
    return {
      liftEffect: function() {
        var $149 = liftEffect(dictMonadEffect);
        return function($150) {
          return HalogenM(liftF(Lift2.create($149($150))));
        };
      }(),
      Monad0: function() {
        return monadHalogenM;
      }
    };
  };
  var functorHalogenM = freeFunctor;
  var bindHalogenM = freeBind;
  var applicativeHalogenM = freeApplicative;

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize = /* @__PURE__ */ function() {
    function Initialize3(value0) {
      this.value0 = value0;
    }
    ;
    Initialize3.create = function(value0) {
      return new Initialize3(value0);
    };
    return Initialize3;
  }();
  var Finalize = /* @__PURE__ */ function() {
    function Finalize3(value0) {
      this.value0 = value0;
    }
    ;
    Finalize3.create = function(value0) {
      return new Finalize3(value0);
    };
    return Finalize3;
  }();
  var Receive = /* @__PURE__ */ function() {
    function Receive2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Receive2.create = function(value0) {
      return function(value1) {
        return new Receive2(value0, value1);
      };
    };
    return Receive2;
  }();
  var Action2 = /* @__PURE__ */ function() {
    function Action3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Action3.create = function(value0) {
      return function(value1) {
        return new Action3(value0, value1);
      };
    };
    return Action3;
  }();
  var Query2 = /* @__PURE__ */ function() {
    function Query3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Query3.create = function(value0) {
      return function(value1) {
        return new Query3(value0, value1);
      };
    };
    return Query3;
  }();

  // output/Halogen.VDom.Thunk/index.js
  var $runtime_lazy6 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Thunk = /* @__PURE__ */ function() {
    function Thunk2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Thunk2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Thunk2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Thunk2;
  }();
  var unsafeThunkId = unsafeCoerce2;
  var unsafeEqThunk = function(v, v1) {
    return refEq2(v.value0, v1.value0) && (refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3));
  };
  var thunk = function(tid, eqFn, f, a2) {
    return new Thunk(tid, eqFn, f, a2);
  };
  var thunked = function(eqFn) {
    return function(f) {
      var tid = unsafeThunkId({
        f
      });
      var eqFn$prime = mkFn2(eqFn);
      return function(a2) {
        return thunk(tid, eqFn$prime, f, a2);
      };
    };
  };
  var runThunk = function(v) {
    return v.value2(v.value3);
  };
  var buildThunk = function(toVDom) {
    var haltThunk = function(state3) {
      return halt(state3.vdom);
    };
    var $lazy_patchThunk = $runtime_lazy6("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $43 = unsafeEqThunk(state3.thunk, t2);
        if ($43) {
          return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step(state3.vdom, toVDom(runThunk(t2)));
        return mkStep(new Step(extract2(vdom), {
          vdom,
          thunk: t2
        }, $lazy_patchThunk(115), haltThunk));
      };
    });
    var patchThunk = $lazy_patchThunk(108);
    var renderThunk = function(spec) {
      return function(t) {
        var vdom = buildVDom(spec)(toVDom(runThunk(t)));
        return mkStep(new Step(extract2(vdom), {
          thunk: t,
          vdom
        }, patchThunk, haltThunk));
      };
    };
    return renderThunk;
  };

  // output/Halogen.Component/index.js
  var ComponentSlot = /* @__PURE__ */ function() {
    function ComponentSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ComponentSlot2.create = function(value0) {
      return new ComponentSlot2(value0);
    };
    return ComponentSlot2;
  }();
  var ThunkSlot = /* @__PURE__ */ function() {
    function ThunkSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkSlot2.create = function(value0) {
      return new ThunkSlot2(value0);
    };
    return ThunkSlot2;
  }();
  var unComponentSlot = unsafeCoerce2;
  var unComponent = unsafeCoerce2;
  var mkEval = function(args) {
    return function(v) {
      if (v instanceof Initialize) {
        return voidLeft(functorHalogenM)(traverse_(applicativeHalogenM)(foldableMaybe)(args.handleAction)(args.initialize))(v.value0);
      }
      ;
      if (v instanceof Finalize) {
        return voidLeft(functorHalogenM)(traverse_(applicativeHalogenM)(foldableMaybe)(args.handleAction)(args.finalize))(v.value0);
      }
      ;
      if (v instanceof Receive) {
        return voidLeft(functorHalogenM)(traverse_(applicativeHalogenM)(foldableMaybe)(args.handleAction)(args.receive(v.value0)))(v.value1);
      }
      ;
      if (v instanceof Action2) {
        return voidLeft(functorHalogenM)(args.handleAction(v.value0))(v.value1);
      }
      ;
      if (v instanceof Query2) {
        return unCoyoneda(function(g) {
          var $24 = map(functorHalogenM)(maybe(v.value1(unit))(g));
          return function($25) {
            return $24(args.handleQuery($25));
          };
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
    };
  };
  var mkComponent = unsafeCoerce2;
  var defaultEval = /* @__PURE__ */ function() {
    return {
      handleAction: $$const(pure(applicativeHalogenM)(unit)),
      handleQuery: $$const(pure(applicativeHalogenM)(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  }();

  // output/Halogen.HTML.Elements/index.js
  var element3 = /* @__PURE__ */ function() {
    return element2(Nothing.value);
  }();
  var canvas = function(props) {
    return element3("canvas")(props)([]);
  };

  // output/Halogen.HTML/index.js
  var memoized = function(eqFn) {
    return function(f) {
      return map(functorFn)(function($5) {
        return widget(ThunkSlot.create($5));
      })(thunked(eqFn)(f));
    };
  };

  // output/Halogen.Query.Event/index.js
  var eventListener2 = function(eventType) {
    return function(target6) {
      return function(f) {
        return makeEmitter(function(push2) {
          return function __do2() {
            var listener = eventListener(function(ev) {
              return traverse_(applicativeEffect)(foldableMaybe)(push2)(f(ev));
            })();
            addEventListener2(eventType)(listener)(false)(target6)();
            return removeEventListener2(eventType)(listener)(false)(target6);
          };
        });
      };
    };
  };

  // output/Web.DOM.NonElementParentNode/foreign.js
  function _getElementById(id3) {
    return function(node) {
      return function() {
        return node.getElementById(id3);
      };
    };
  }

  // output/Web.DOM.NonElementParentNode/index.js
  var getElementById = function(eid) {
    var $0 = map(functorEffect)(toMaybe);
    var $1 = _getElementById(eid);
    return function($2) {
      return $0($1($2));
    };
  };

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return function() {
      return doc.readyState;
    };
  }

  // output/Web.HTML.HTMLDocument.ReadyState/index.js
  var Loading = /* @__PURE__ */ function() {
    function Loading2() {
    }
    ;
    Loading2.value = new Loading2();
    return Loading2;
  }();
  var Interactive = /* @__PURE__ */ function() {
    function Interactive2() {
    }
    ;
    Interactive2.value = new Interactive2();
    return Interactive2;
  }();
  var Complete = /* @__PURE__ */ function() {
    function Complete2() {
    }
    ;
    Complete2.value = new Complete2();
    return Complete2;
  }();
  var parse = function(v) {
    if (v === "loading") {
      return new Just(Loading.value);
    }
    ;
    if (v === "interactive") {
      return new Just(Interactive.value);
    }
    ;
    if (v === "complete") {
      return new Just(Complete.value);
    }
    ;
    return Nothing.value;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var toParentNode = unsafeCoerce2;
  var toNonElementParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = /* @__PURE__ */ function() {
    var $0 = map(functorEffect)(function() {
      var $2 = fromMaybe(Loading.value);
      return function($3) {
        return $2(parse($3));
      };
    }());
    return function($1) {
      return $0(_readyState($1));
    };
  }();

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value13) {
    var tag = Object.prototype.toString.call(value13);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value13);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode2 = unsafeCoerce2;
  var fromElement = function(x) {
    return _read(Nothing.value, Just.create, x);
  };

  // output/Web.HTML.Window/foreign.js
  function document2(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Web.HTML.Window/index.js
  var toEventTarget = unsafeCoerce2;

  // output/Gesso.Canvas/index.js
  var Changed = /* @__PURE__ */ function() {
    function Changed2() {
    }
    ;
    Changed2.value = new Changed2();
    return Changed2;
  }();
  var DidNotChange = /* @__PURE__ */ function() {
    function DidNotChange2() {
    }
    ;
    DidNotChange2.value = new DidNotChange2();
    return DidNotChange2;
  }();
  var Output = function(x) {
    return x;
  };
  var Initialize2 = /* @__PURE__ */ function() {
    function Initialize3() {
    }
    ;
    Initialize3.value = new Initialize3();
    return Initialize3;
  }();
  var HandleResize = /* @__PURE__ */ function() {
    function HandleResize2() {
    }
    ;
    HandleResize2.value = new HandleResize2();
    return HandleResize2;
  }();
  var Tick = /* @__PURE__ */ function() {
    function Tick2(value0) {
      this.value0 = value0;
    }
    ;
    Tick2.create = function(value0) {
      return new Tick2(value0);
    };
    return Tick2;
  }();
  var Finalize2 = /* @__PURE__ */ function() {
    function Finalize3() {
    }
    ;
    Finalize3.value = new Finalize3();
    return Finalize3;
  }();
  var StateUpdated = /* @__PURE__ */ function() {
    function StateUpdated2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    StateUpdated2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new StateUpdated2(value0, value1, value22);
        };
      };
    };
    return StateUpdated2;
  }();
  var QueueUpdate = /* @__PURE__ */ function() {
    function QueueUpdate2(value0) {
      this.value0 = value0;
    }
    ;
    QueueUpdate2.create = function(value0) {
      return new QueueUpdate2(value0);
    };
    return QueueUpdate2;
  }();
  var UpdatesProcessed = /* @__PURE__ */ function() {
    function UpdatesProcessed2() {
    }
    ;
    UpdatesProcessed2.value = new UpdatesProcessed2();
    return UpdatesProcessed2;
  }();
  var FrameRequested = /* @__PURE__ */ function() {
    function FrameRequested2(value0) {
      this.value0 = value0;
    }
    ;
    FrameRequested2.create = function(value0) {
      return new FrameRequested2(value0);
    };
    return FrameRequested2;
  }();
  var FrameFired = /* @__PURE__ */ function() {
    function FrameFired2() {
    }
    ;
    FrameFired2.value = new FrameFired2();
    return FrameFired2;
  }();
  var unsubscribe3 = function(dictMonadAff) {
    return bind(bindHalogenM)(gets(monadStateHalogenM)(function(v) {
      return v.resizeSub;
    }))(function(mresizeSub) {
      return discard(discardUnit)(bindHalogenM)(traverse_(applicativeHalogenM)(foldableMaybe)(unsubscribe2)(mresizeSub))(function() {
        return bind(bindHalogenM)(gets(monadStateHalogenM)(function(v) {
          return v.emitterSub;
        }))(function(memitterSub) {
          return traverse_(applicativeHalogenM)(foldableMaybe)(unsubscribe2)(memitterSub);
        });
      });
    });
  };
  var subscribeResize = function(dictMonadAff) {
    return bind(bindHalogenM)(liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()))(windowImpl))(function(wnd) {
      return subscribe2(eventListener2("resize")(toEventTarget(wnd))($$const(new Just(HandleResize.value))));
    });
  };
  var saveNewState = function(dictMonadAff) {
    return function(delta2) {
      return function(scaler) {
        return function(state$prime) {
          return bind(bindHalogenM)(get(monadStateHalogenM))(function(v) {
            return discard(discardUnit)(bindHalogenM)(modify_2(monadStateHalogenM)(function(v1) {
              var $37 = {};
              for (var $38 in v1) {
                if ({}.hasOwnProperty.call(v1, $38)) {
                  $37[$38] = v1[$38];
                }
                ;
              }
              ;
              $37.localState = state$prime;
              return $37;
            }))(function() {
              return traverse_(applicativeHalogenM)(foldableMaybe)(function($115) {
                return raise(Output($115));
              })(v.app.output(delta2)(scaler)(v.localState)(state$prime));
            });
          });
        };
      };
    };
  };
  var render = function(v) {
    var style2 = function() {
      var $116 = attr2("style");
      var $117 = fromMaybe("");
      var $118 = rules([]);
      return function($119) {
        return $116($117(renderedInline($118(runS($119)))));
      };
    }();
    return canvas(append(semigroupArray)([id2(v.name), style2(windowCss(v.app.window)), tabIndex(0)])(append(semigroupArray)(toProps(QueueUpdate.create)(v.interactions))(maybe([])(toSizeProps(sizedDimensions))(v.clientRect))));
  };
  var queueAnimationFrame = function(dictMonadAff) {
    return function(mLastTime) {
      return function(mlistener) {
        return function(mcontext) {
          return function(mscaler) {
            return function(queuedUpdates) {
              return function(localState) {
                return function(app) {
                  var notify2 = function() {
                    if (mlistener instanceof Nothing) {
                      return $$const(pure(applicativeEffect)(unit));
                    }
                    ;
                    if (mlistener instanceof Just) {
                      return notify(mlistener.value0);
                    }
                    ;
                    throw new Error("Failed pattern match at Gesso.Canvas (line 352, column 12 - line 354, column 40): " + [mlistener.constructor.name]);
                  }();
                  var applyUpdate = function(delta2) {
                    return function(scaler) {
                      return function(update) {
                        return function(s) {
                          return function __do2() {
                            var v = s();
                            var mstate$prime = runUpdate(delta2)(scaler)(v.value1)(update)();
                            if (mstate$prime instanceof Just) {
                              return new Tuple(Changed.value, mstate$prime.value0);
                            }
                            ;
                            if (mstate$prime instanceof Nothing) {
                              return s();
                            }
                            ;
                            throw new Error("Failed pattern match at Gesso.Canvas (line 387, column 5 - line 389, column 19): " + [mstate$prime.constructor.name]);
                          };
                        };
                      };
                    };
                  };
                  var updateAndRender = function(listener) {
                    return function(delta2) {
                      return function(context) {
                        return function(scaler) {
                          return function __do2() {
                            var v = foldr(foldableList)(applyUpdate(delta2)(scaler))(pure(applicativeEffect)(new Tuple(DidNotChange.value, localState)))(new Cons(app.update, queuedUpdates))();
                            (function() {
                              if (v.value0 instanceof Changed) {
                                return notify(listener)(new StateUpdated(delta2, scaler, v.value1))();
                              }
                              ;
                              if (v.value0 instanceof DidNotChange) {
                                return unit;
                              }
                              ;
                              throw new Error("Failed pattern match at Gesso.Canvas (line 369, column 5 - line 371, column 32): " + [v.value0.constructor.name]);
                            })();
                            notify(listener)(UpdatesProcessed.value)();
                            return app.render(v.value1)(delta2)(scaler)(context)();
                          };
                        };
                      };
                    };
                  };
                  var rafCallback = function(timestamp) {
                    return function __do2() {
                      notify2(FrameFired.value)();
                      var mdelta = map(functorMaybe)(delta(timestamp))(mLastTime);
                      sequence(traversableMaybe)(applicativeEffect)(apply(applyMaybe)(apply(applyMaybe)(apply(applyMaybe)(map(functorMaybe)(updateAndRender)(mlistener))(mdelta))(mcontext))(mscaler))();
                      return notify2(new Tick(new Just(toPrev(timestamp))))();
                    };
                  };
                  return liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()))(function __do2() {
                    var wnd = windowImpl();
                    var rafId = requestAnimationFrame(rafCallback)(wnd)();
                    return notify2(new FrameRequested(rafId))();
                  });
                };
              };
            };
          };
        };
      };
    };
  };
  var initialState = function(v) {
    return {
      name: v.name,
      app: v.app,
      localState: v.localState,
      viewBox: v.viewBox,
      clientRect: Nothing.value,
      canvas: Nothing.value,
      context: Nothing.value,
      scaler: Nothing.value,
      resizeSub: Nothing.value,
      emitterSub: Nothing.value,
      listener: Nothing.value,
      interactions: v.interactions,
      queuedUpdates: Nil.value,
      processingUpdates: Nil.value,
      rafId: Nothing.value
    };
  };
  var getContext = function(name15) {
    return function __do2() {
      var mcanvas = getCanvasElementById(name15)();
      var mcontext = traverse(traversableMaybe)(applicativeEffect)(getContext2D)(mcanvas)();
      return mcontext;
    };
  };
  var getCanvasElement = function(name15) {
    return function __do2() {
      var doc = bindFlipped(bindEffect)(document2)(windowImpl)();
      var mcanvas = getElementById(name15)(toNonElementParentNode(doc))();
      return mcanvas;
    };
  };
  var getCanvasClientRect = function(mcanvas) {
    return function __do2() {
      var v = traverse(traversableMaybe)(applicativeEffect)(getBoundingClientRect)(mcanvas)();
      return map(functorMaybe)(fromDOMRect)(v);
    };
  };
  var initialize = function(dictMonadAff) {
    return bind(bindHalogenM)(subscribeResize(dictMonadAff))(function(resizeSub) {
      return bind(bindHalogenM)(liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()))(create))(function(v) {
        return bind(bindHalogenM)(subscribe2(v.emitter))(function(emitterSub) {
          return bind(bindHalogenM)(get(monadStateHalogenM))(function(v1) {
            return bind(bindHalogenM)(liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()))(getContext(v1.name)))(function(mcontext) {
              return bind(bindHalogenM)(liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()))(getCanvasElement(v1.name)))(function(mcanvas) {
                return bind(bindHalogenM)(liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()))(getCanvasClientRect(mcanvas)))(function(clientRect) {
                  return modify_2(monadStateHalogenM)(function(v2) {
                    var $67 = {};
                    for (var $68 in v2) {
                      if ({}.hasOwnProperty.call(v2, $68)) {
                        $67[$68] = v2[$68];
                      }
                      ;
                    }
                    ;
                    $67.context = mcontext;
                    $67.resizeSub = new Just(resizeSub);
                    $67.emitterSub = new Just(emitterSub);
                    $67.listener = new Just(v.listener);
                    $67.clientRect = clientRect;
                    $67.canvas = mcanvas;
                    $67.scaler = map(functorMaybe)(mkScaler(v1.viewBox))(clientRect);
                    return $67;
                  });
                });
              });
            });
          });
        });
      });
    });
  };
  var updateClientRect = function(dictMonadAff) {
    return bind(bindHalogenM)(get(monadStateHalogenM))(function(v) {
      return bind(bindHalogenM)(liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()))(getCanvasClientRect(v.canvas)))(function(clientRect) {
        return modify_2(monadStateHalogenM)(function(v1) {
          var $75 = {};
          for (var $76 in v1) {
            if ({}.hasOwnProperty.call(v1, $76)) {
              $75[$76] = v1[$76];
            }
            ;
          }
          ;
          $75.clientRect = clientRect;
          $75.scaler = map(functorMaybe)(mkScaler(v.viewBox))(clientRect);
          return $75;
        });
      });
    });
  };
  var handleAction = function(dictMonadAff) {
    return function(v) {
      if (v instanceof Initialize2) {
        return discard(discardUnit)(bindHalogenM)(initialize(dictMonadAff))(function() {
          return handleAction(dictMonadAff)(new Tick(Nothing.value));
        });
      }
      ;
      if (v instanceof HandleResize) {
        return updateClientRect(dictMonadAff);
      }
      ;
      if (v instanceof Tick) {
        return bind(bindHalogenM)(get(monadStateHalogenM))(function(v1) {
          var tryUpdates = append(semigroupList)(v1.queuedUpdates)(v1.processingUpdates);
          return discard(discardUnit)(bindHalogenM)(modify_2(monadStateHalogenM)(function(v2) {
            var $82 = {};
            for (var $83 in v2) {
              if ({}.hasOwnProperty.call(v2, $83)) {
                $82[$83] = v2[$83];
              }
              ;
            }
            ;
            $82.queuedUpdates = Nil.value;
            $82.processingUpdates = tryUpdates;
            return $82;
          }))(function() {
            return queueAnimationFrame(dictMonadAff)(v.value0)(v1.listener)(v1.context)(v1.scaler)(tryUpdates)(v1.localState)(v1.app);
          });
        });
      }
      ;
      if (v instanceof Finalize2) {
        return unsubscribe3(dictMonadAff);
      }
      ;
      if (v instanceof QueueUpdate) {
        return bind(bindHalogenM)(gets(monadStateHalogenM)(function(v1) {
          return v1.queuedUpdates;
        }))(function(queuedUpdates) {
          return modify_2(monadStateHalogenM)(function(v1) {
            var $93 = {};
            for (var $94 in v1) {
              if ({}.hasOwnProperty.call(v1, $94)) {
                $93[$94] = v1[$94];
              }
              ;
            }
            ;
            $93.queuedUpdates = new Cons(v.value0, queuedUpdates);
            return $93;
          });
        });
      }
      ;
      if (v instanceof UpdatesProcessed) {
        return modify_2(monadStateHalogenM)(function(v1) {
          var $97 = {};
          for (var $98 in v1) {
            if ({}.hasOwnProperty.call(v1, $98)) {
              $97[$98] = v1[$98];
            }
            ;
          }
          ;
          $97.processingUpdates = Nil.value;
          return $97;
        });
      }
      ;
      if (v instanceof StateUpdated) {
        return saveNewState(dictMonadAff)(v.value0)(v.value1)(v.value2);
      }
      ;
      if (v instanceof FrameRequested) {
        return modify_2(monadStateHalogenM)(function(v1) {
          var $103 = {};
          for (var $104 in v1) {
            if ({}.hasOwnProperty.call(v1, $104)) {
              $103[$104] = v1[$104];
            }
            ;
          }
          ;
          $103.rafId = new Just(v.value0);
          return $103;
        });
      }
      ;
      if (v instanceof FrameFired) {
        return modify_2(monadStateHalogenM)(function(v1) {
          var $107 = {};
          for (var $108 in v1) {
            if ({}.hasOwnProperty.call(v1, $108)) {
              $107[$108] = v1[$108];
            }
            ;
          }
          ;
          $107.rafId = Nothing.value;
          return $107;
        });
      }
      ;
      throw new Error("Failed pattern match at Gesso.Canvas (line 228, column 16 - line 265, column 50): " + [v.constructor.name]);
    };
  };
  var handleQuery = function(dictMonadAff) {
    return function(v) {
      return bind(bindHalogenM)(gets(monadStateHalogenM)(function(v1) {
        return v1.app;
      }))(function(v1) {
        return discard(discardUnit)(bindHalogenM)(handleAction(dictMonadAff)(new QueueUpdate(pureUpdate(v1.input(v.value0)))))(function() {
          return pure(applicativeHalogenM)(new Just(v.value1));
        });
      });
    };
  };
  var component = function(dictMonadAff) {
    return mkComponent({
      initialState,
      render: memoized(on(eq(eqMaybe(eqDimensions)))(function(v) {
        return v.clientRect;
      }))(render),
      "eval": mkEval({
        handleAction: handleAction(dictMonadAff),
        handleQuery: handleQuery(dictMonadAff),
        receive: defaultEval.receive,
        initialize: new Just(Initialize2.value),
        finalize: new Just(Finalize2.value)
      })
    });
  };

  // output/Control.Monad.Fork.Class/index.js
  var monadForkAff = {
    suspend: suspendAff,
    fork: forkAff,
    join: joinFiber,
    Monad0: function() {
      return monadAff;
    },
    Functor1: function() {
      return functorFiber;
    }
  };
  var fork = function(dict) {
    return dict.fork;
  };

  // output/Halogen.Aff.Driver.State/index.js
  var unRenderStateX = unsafeCoerce2;
  var unDriverStateX = unsafeCoerce2;
  var renderStateX_ = function(dictApplicative) {
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_(dictApplicative)(foldableMaybe)(f)(st.rendering);
      });
    };
  };
  var mkRenderStateX = unsafeCoerce2;
  var renderStateX = function(dictFunctor) {
    return function(f) {
      return unDriverStateX(function(st) {
        return mkRenderStateX(f(st.rendering));
      });
    };
  };
  var mkDriverStateXRef = unsafeCoerce2;
  var mapDriverState = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var initDriverState = function(component2) {
    return function(input4) {
      return function(handler3) {
        return function(lchs) {
          return function __do2() {
            var selfRef = $$new({})();
            var childrenIn = $$new(empty4)();
            var childrenOut = $$new(empty4)();
            var handlerRef = $$new(handler3)();
            var pendingQueries = $$new(new Just(Nil.value))();
            var pendingOuts = $$new(new Just(Nil.value))();
            var pendingHandlers = $$new(Nothing.value)();
            var fresh2 = $$new(1)();
            var subscriptions = $$new(new Just(empty3))();
            var forks = $$new(empty3)();
            var ds = {
              component: component2,
              state: component2.initialState(input4),
              refs: empty3,
              children: empty4,
              childrenIn,
              childrenOut,
              selfRef,
              handlerRef,
              pendingQueries,
              pendingOuts,
              pendingHandlers,
              rendering: Nothing.value,
              fresh: fresh2,
              subscriptions,
              forks,
              lifecycleHandlers: lchs
            };
            write(ds)(selfRef)();
            return mkDriverStateXRef(selfRef);
          };
        };
      };
    };
  };

  // output/Halogen.Aff.Driver.Eval/index.js
  var unsubscribe4 = function(sid) {
    return function(ref2) {
      return function __do2() {
        var v = read(ref2)();
        var subs = read(v.subscriptions)();
        return traverse_(applicativeEffect)(foldableMaybe)(unsubscribe)(bindFlipped(bindMaybe)(lookup3(ordSubscriptionId)(sid))(subs))();
      };
    };
  };
  var queueOrRun = function(ref2) {
    return function(au) {
      return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v) {
        if (v instanceof Nothing) {
          return au;
        }
        ;
        if (v instanceof Just) {
          return liftEffect(monadEffectAff)(write(new Just(new Cons(au, v.value0)))(ref2));
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
      });
    };
  };
  var handleLifecycle = function(lchs) {
    return function(f) {
      return discard(discardUnit)(bindAff)(liftEffect(monadEffectAff)(write({
        initializers: Nil.value,
        finalizers: Nil.value
      })(lchs)))(function() {
        return bind(bindAff)(liftEffect(monadEffectAff)(f))(function(result) {
          return bind(bindAff)(liftEffect(monadEffectAff)(read(lchs)))(function(v) {
            return discard(discardUnit)(bindAff)(traverse_(applicativeAff)(foldableList)(fork(monadForkAff))(v.finalizers))(function() {
              return discard(discardUnit)(bindAff)(parSequence_(parallelAff)(foldableList)(v.initializers))(function() {
                return pure(applicativeAff)(result);
              });
            });
          });
        });
      });
    };
  };
  var handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
  var fresh = function(f) {
    return function(ref2) {
      return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v) {
        return liftEffect(monadEffectAff)(modify$prime(function(i2) {
          return {
            state: i2 + 1 | 0,
            value: f(i2)
          };
        })(v.fresh));
      });
    };
  };
  var evalQ = function(render3) {
    return function(ref2) {
      return function(q2) {
        return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v) {
          return evalM(render3)(ref2)(v["component"]["eval"](new Query2(map(functorCoyoneda)(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render3) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref2) {
          return function(cqb) {
            return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel(parallelAff)(bind(bindAff)(liftEffect(monadEffectAff)(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render3)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map(functorAff)(v2.value2)(sequential(parallelAff)(v2.value0(applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };
        var go2 = function(ref2) {
          return function(v1) {
            if (v1 instanceof State) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                var v3 = v1.value0(v2.state);
                if (unsafeRefEq(v2.state)(v3.value1)) {
                  return pure(applicativeAff)(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard(discardUnit)(bindAff)(liftEffect(monadEffectAff)(write({
                    component: v2.component,
                    state: v3.value1,
                    refs: v2.refs,
                    children: v2.children,
                    childrenIn: v2.childrenIn,
                    childrenOut: v2.childrenOut,
                    selfRef: v2.selfRef,
                    handlerRef: v2.handlerRef,
                    pendingQueries: v2.pendingQueries,
                    pendingOuts: v2.pendingOuts,
                    pendingHandlers: v2.pendingHandlers,
                    rendering: v2.rendering,
                    fresh: v2.fresh,
                    subscriptions: v2.subscriptions,
                    forks: v2.forks,
                    lifecycleHandlers: v2.lifecycleHandlers
                  })(ref2)))(function() {
                    return discard(discardUnit)(bindAff)(handleLifecycle(v2.lifecycleHandlers)(render3(v2.lifecycleHandlers)(ref2)))(function() {
                      return pure(applicativeAff)(v3.value0);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
              });
            }
            ;
            if (v1 instanceof Subscribe) {
              return bind(bindAff)(fresh(SubscriptionId)(ref2))(function(sid) {
                return bind(bindAff)(liftEffect(monadEffectAff)(subscribe(v1.value0(sid))(function(act) {
                  return handleAff(evalF(render3)(ref2)(new Action(act)));
                })))(function(finalize) {
                  return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                    return discard(discardUnit)(bindAff)(liftEffect(monadEffectAff)(modify_(map(functorMaybe)(insert2(ordSubscriptionId)(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure(applicativeAff)(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard(discardUnit)(bindAff)(liftEffect(monadEffectAff)(unsubscribe4(v1.value0)(ref2)))(function() {
                return pure(applicativeAff)(v1.value1);
              });
            }
            ;
            if (v1 instanceof Lift2) {
              return v1.value0;
            }
            ;
            if (v1 instanceof ChildQuery2) {
              return evalChildQuery(ref2)(v1.value0);
            }
            ;
            if (v1 instanceof Raise) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                return bind(bindAff)(liftEffect(monadEffectAff)(read(v2.handlerRef)))(function(handler3) {
                  return discard(discardUnit)(bindAff)(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                    return pure(applicativeAff)(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential(parallelAff)(retractFreeAp(applicativeParAff)(hoistFreeAp(function() {
                var $83 = parallel(parallelAff);
                var $84 = evalM(render3)(ref2);
                return function($85) {
                  return $83($84($85));
                };
              }())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind(bindAff)(fresh(ForkId)(ref2))(function(fid) {
                return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                  return bind(bindAff)(liftEffect(monadEffectAff)($$new(false)))(function(doneRef) {
                    return bind(bindAff)(fork(monadForkAff)($$finally(liftEffect(monadEffectAff)(function __do2() {
                      modify_($$delete2(ordForkId)(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render3)(ref2)(v1.value0))))(function(fiber) {
                      return discard(discardUnit)(bindAff)(liftEffect(monadEffectAff)(unlessM(monadEffect)(read(doneRef))(modify_(insert2(ordForkId)(fid)(fiber))(v2.forks))))(function() {
                        return pure(applicativeAff)(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Join) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                return bind(bindAff)(liftEffect(monadEffectAff)(read(v2.forks)))(function(forkMap) {
                  return discard(discardUnit)(bindAff)(traverse_(applicativeAff)(foldableMaybe)(joinFiber)(lookup3(ordForkId)(v1.value0)(forkMap)))(function() {
                    return pure(applicativeAff)(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                return bind(bindAff)(liftEffect(monadEffectAff)(read(v2.forks)))(function(forkMap) {
                  return discard(discardUnit)(bindAff)(traverse_(applicativeAff)(foldableMaybe)(killFiber(error("Cancelled")))(lookup3(ordForkId)(v1.value0)(forkMap)))(function() {
                    return pure(applicativeAff)(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                return pure(applicativeAff)(v1.value1(lookup3(ordString)(v1.value0)(v2.refs)));
              });
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
          };
        };
        return foldFree(monadRecAff)(go2(initRef))(v);
      };
    };
  };
  var evalF = function(render3) {
    return function(ref2) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect(monadEffectAff)(flip(modify_)(ref2)(mapDriverState(function(st) {
            return {
              component: st.component,
              state: st.state,
              refs: alter(ordString)($$const(v.value1))(v.value0)(st.refs),
              children: st.children,
              childrenIn: st.childrenIn,
              childrenOut: st.childrenOut,
              selfRef: st.selfRef,
              handlerRef: st.handlerRef,
              pendingQueries: st.pendingQueries,
              pendingOuts: st.pendingOuts,
              pendingHandlers: st.pendingHandlers,
              rendering: st.rendering,
              fresh: st.fresh,
              subscriptions: st.subscriptions,
              forks: st.forks,
              lifecycleHandlers: st.lifecycleHandlers
            };
          })));
        }
        ;
        if (v instanceof Action) {
          return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v1) {
            return evalM(render3)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
      };
    };
  };

  // output/Halogen.Aff.Driver/index.js
  var newLifecycleHandlers = /* @__PURE__ */ function() {
    return $$new({
      initializers: Nil.value,
      finalizers: Nil.value
    });
  }();
  var handlePending = function(ref2) {
    return function __do2() {
      var queue = read(ref2)();
      write(Nothing.value)(ref2)();
      return for_(applicativeEffect)(foldableMaybe)(queue)(function() {
        var $28 = traverse_(applicativeAff)(foldableList)(fork(monadForkAff));
        return function($29) {
          return handleAff($28(reverse2($29)));
        };
      }())();
    };
  };
  var cleanupSubscriptionsAndForks = function(v) {
    return function __do2() {
      bindFlipped(bindEffect)(traverse_(applicativeEffect)(foldableMaybe)(traverse_(applicativeEffect)(foldableMap)(unsubscribe)))(read(v.subscriptions))();
      write(Nothing.value)(v.subscriptions)();
      bindFlipped(bindEffect)(traverse_(applicativeEffect)(foldableMap)(function() {
        var $30 = killFiber(error("finalized"));
        return function($31) {
          return handleAff($30($31));
        };
      }()))(read(v.forks))();
      return write(empty3)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component2) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render3)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_(function(handlers) {
                return {
                  initializers: new Cons(discard(discardUnit)(bindAff)(parSequence_(parallelAff)(foldableList)(reverse2(handlers.initializers)))(function() {
                    return discard(discardUnit)(bindAff)(parentInitializer)(function() {
                      return liftEffect(monadEffectAff)(function __do2() {
                        handlePending(st.pendingQueries)();
                        return handlePending(st.pendingOuts)();
                      });
                    });
                  }), preInits),
                  finalizers: handlers.finalizers
                };
              })(lchs);
            });
          };
        };
        var runComponent = function(lchs) {
          return function(handler3) {
            return function(j) {
              return unComponent(function(c) {
                return function __do2() {
                  var lchs$prime = newLifecycleHandlers();
                  var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                  var pre2 = read(lchs)();
                  write({
                    initializers: Nil.value,
                    finalizers: pre2.finalizers
                  })(lchs)();
                  bindFlipped(bindEffect)(unDriverStateX(function() {
                    var $32 = render3(lchs);
                    return function($33) {
                      return $32(function(v) {
                        return v.selfRef;
                      }($33));
                    };
                  }()))(read($$var2))();
                  bindFlipped(bindEffect)(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                  return $$var2;
                };
              });
            };
          };
        };
        var renderChild = function(lchs) {
          return function(handler3) {
            return function(childrenInRef) {
              return function(childrenOutRef) {
                return unComponentSlot(function(slot) {
                  return function __do2() {
                    var childrenIn = map(functorEffect)(slot.pop)(read(childrenInRef))();
                    var $$var2 = function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do3() {
                            flip(write)(st.handlerRef)(function() {
                              var $34 = maybe(pure(applicativeAff)(unit))(handler3);
                              return function($35) {
                                return $34(slot.output($35));
                              };
                            }())();
                            return handleAff(evalM(render3)(st.selfRef)(st["component"]["eval"](new Receive(slot.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)(function() {
                          var $36 = maybe(pure(applicativeAff)(unit))(handler3);
                          return function($37) {
                            return $36(slot.output($37));
                          };
                        }())(slot.input)(slot.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    }();
                    var isDuplicate = map(functorEffect)(function($38) {
                      return isJust(slot.get($38));
                    })(read(childrenOutRef))();
                    when(applicativeEffect)(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_(slot.set($$var2))(childrenOutRef)();
                    return bind(bindEffect)(read($$var2))(renderStateX(functorEffect)(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure(applicativeEffect)(renderSpec2.renderChild(v.value0));
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [v.constructor.name]);
                    }))();
                  };
                });
              };
            };
          };
        };
        var render3 = function(lchs) {
          return function($$var2) {
            return function __do2() {
              var v = read($$var2)();
              var shouldProcessHandlers = map(functorEffect)(isNothing)(read(v.pendingHandlers))();
              when(applicativeEffect)(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty4)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var selfRef = identity(categoryFn)(v.selfRef);
              var pendingQueries = identity(categoryFn)(v.pendingQueries);
              var pendingHandlers = identity(categoryFn)(v.pendingHandlers);
              var handler3 = function() {
                var $39 = queueOrRun(pendingHandlers);
                var $40 = $$void(functorAff);
                var $41 = evalF(render3)(selfRef);
                return function($42) {
                  return $39($40($41($42)));
                };
              }();
              var childHandler = function() {
                var $43 = queueOrRun(pendingQueries);
                return function($44) {
                  return $43(handler3(Action.create($44)));
                };
              }();
              var rendering = renderSpec2.render(function($45) {
                return handleAff(handler3($45));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children2 = read(v.childrenOut)();
              var childrenIn = read(v.childrenIn)();
              foreachSlot(applicativeEffect)(childrenIn)(function(v1) {
                return function __do3() {
                  var childDS = read(v1)();
                  renderStateX_(applicativeEffect)(renderSpec2.removeChild)(childDS)();
                  return finalize(lchs)(childDS)();
                };
              })();
              flip(modify_)(v.selfRef)(mapDriverState(function(ds$prime) {
                return {
                  component: ds$prime.component,
                  state: ds$prime.state,
                  refs: ds$prime.refs,
                  children: children2,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  rendering: new Just(rendering),
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers
                };
              }))();
              return when(applicativeEffect)(shouldProcessHandlers)(flip(tailRecM(monadRecEffect))(unit)(function(v1) {
                return function __do3() {
                  var handlers = read(pendingHandlers)();
                  write(new Just(Nil.value))(pendingHandlers)();
                  traverse_(applicativeEffect)(foldableMaybe)(function() {
                    var $46 = traverse_(applicativeAff)(foldableList)(fork(monadForkAff));
                    return function($47) {
                      return handleAff($46(reverse2($47)));
                    };
                  }())(handlers)();
                  var mmore = read(pendingHandlers)();
                  var $21 = maybe(false)($$null2)(mmore);
                  if ($21) {
                    return voidLeft(functorEffect)(write(Nothing.value)(pendingHandlers))(new Done(unit))();
                  }
                  ;
                  return new Loop(unit);
                };
              }))();
            };
          };
        };
        var finalize = function(lchs) {
          return unDriverStateX(function(st) {
            return function __do2() {
              cleanupSubscriptionsAndForks(st)();
              var f = evalM(render3)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
              modify_(function(handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return foreachSlot(applicativeEffect)(st.children)(function(v) {
                return function __do3() {
                  var dsx = read(v)();
                  return finalize(lchs)(dsx)();
                };
              })();
            };
          });
        };
        var evalDriver = function(disposed) {
          return function(ref2) {
            return function(q2) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(disposed)))(function(v) {
                if (v) {
                  return pure(applicativeAff)(Nothing.value);
                }
                ;
                return evalQ(render3)(ref2)(q2);
              });
            };
          };
        };
        var dispose = function(disposed) {
          return function(lchs) {
            return function(dsx) {
              return handleLifecycle(lchs)(function __do2() {
                var v = read(disposed)();
                if (v) {
                  return unit;
                }
                ;
                write(true)(disposed)();
                finalize(lchs)(dsx)();
                return unDriverStateX(function(v1) {
                  return function __do3() {
                    var v2 = liftEffect(monadEffectEffect)(read(v1.selfRef))();
                    return for_(applicativeEffect)(foldableMaybe)(v2.rendering)(renderSpec2.dispose)();
                  };
                })(dsx)();
              });
            };
          };
        };
        return bind(bindAff)(liftEffect(monadEffectAff)(newLifecycleHandlers))(function(lchs) {
          return bind(bindAff)(liftEffect(monadEffectAff)($$new(false)))(function(disposed) {
            return handleLifecycle(lchs)(function __do2() {
              var sio = create();
              var dsx = bindFlipped(bindEffect)(read)(runComponent(lchs)(function() {
                var $48 = liftEffect(monadEffectAff);
                var $49 = notify(sio.listener);
                return function($50) {
                  return $48($49($50));
                };
              }())(i2)(component2))();
              return unDriverStateX(function(st) {
                return pure(applicativeEffect)({
                  query: evalDriver(disposed)(st.selfRef),
                  messages: sio.emitter,
                  dispose: dispose(disposed)(lchs)(dsx)
                });
              })(dsx)();
            });
          });
        });
      };
    };
  };

  // output/Halogen.Aff.Util/index.js
  var selectElement = function(query2) {
    return bind(bindAff)(liftEffect(monadEffectAff)(bindFlipped(bindEffect)(composeKleisliFlipped(bindEffect)(function() {
      var $2 = querySelector(query2);
      return function($3) {
        return $2(toParentNode($3));
      };
    }())(document2))(windowImpl)))(function(mel) {
      return pure(applicativeAff)(bindFlipped(bindMaybe)(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do2() {
      var rs = bindFlipped(bindEffect)(readyState)(bindFlipped(bindEffect)(document2)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map(functorEffect)(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return callback(new Right(unit));
        })();
        addEventListener2(domcontentloaded)(listener)(false)(et)();
        return effectCanceler(removeEventListener2(domcontentloaded)(listener)(false)(et));
      }
      ;
      callback(new Right(unit))();
      return nonCanceler;
    };
  });
  var awaitBody = /* @__PURE__ */ discard(discardUnit)(bindAff)(awaitLoad)(function() {
    return bind(bindAff)(selectElement("body"))(function(body2) {
      return maybe(throwError(monadThrowAff)(error("Could not find body")))(pure(applicativeAff))(body2);
    });
  });

  // output/Web.DOM.Node/foreign.js
  var getEffProp2 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var baseURI = getEffProp2("baseURI");
  var _ownerDocument = getEffProp2("ownerDocument");
  var _parentNode = getEffProp2("parentNode");
  var _parentElement = getEffProp2("parentElement");
  var childNodes = getEffProp2("childNodes");
  var _firstChild = getEffProp2("firstChild");
  var _lastChild = getEffProp2("lastChild");
  var _previousSibling = getEffProp2("previousSibling");
  var _nextSibling = getEffProp2("nextSibling");
  var _nodeValue = getEffProp2("nodeValue");
  var textContent = getEffProp2("textContent");
  function insertBefore(node1) {
    return function(node2) {
      return function(parent2) {
        return function() {
          parent2.insertBefore(node1, node2);
        };
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }
  function removeChild2(node) {
    return function(parent2) {
      return function() {
        parent2.removeChild(node);
      };
    };
  }

  // output/Web.DOM.Node/index.js
  var parentNode2 = /* @__PURE__ */ function() {
    var $2 = map(functorEffect)(toMaybe);
    return function($3) {
      return $2(_parentNode($3));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $13 = map(functorEffect)(toMaybe);
    return function($14) {
      return $13(_nextSibling($14));
    };
  }();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy7 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var substInParent = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Just && v2 instanceof Just) {
          return $$void(functorEffect)(insertBefore(v)(v1.value0)(v2.value0));
        }
        ;
        if (v1 instanceof Nothing && v2 instanceof Just) {
          return $$void(functorEffect)(appendChild(v)(v2.value0));
        }
        ;
        return pure(applicativeEffect)(unit);
      };
    };
  };
  var removeChild3 = function(v) {
    return function __do2() {
      var npn = parentNode2(v.node)();
      return traverse_(applicativeEffect)(foldableMaybe)(function(pn) {
        return removeChild2(v.node)(pn);
      })(npn)();
    };
  };
  var mkSpec = function(handler3) {
    return function(renderChildRef) {
      return function(document3) {
        var getNode = unRenderStateX(function(v) {
          return v.node;
        });
        var done = function(st) {
          if (st instanceof Just) {
            return halt(st.value0);
          }
          ;
          return unit;
        };
        var buildWidget2 = function(spec) {
          var buildThunk2 = buildThunk(unwrap())(spec);
          var $lazy_patch = $runtime_lazy7("patch", "Halogen.VDom.Driver", function() {
            return function(st, slot) {
              if (st instanceof Just) {
                if (slot instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot.value0);
                }
                ;
                if (slot instanceof ThunkSlot) {
                  var step$prime = step(st.value0, slot.value0);
                  return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot);
            };
          });
          var $lazy_render = $runtime_lazy7("render", "Halogen.VDom.Driver", function() {
            return function(slot) {
              if (slot instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot.value0);
              }
              ;
              if (slot instanceof ThunkSlot) {
                var step4 = buildThunk2(slot.value0);
                return mkStep(new Step(extract2(step4), new Just(step4), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot.constructor.name]);
            };
          });
          var $lazy_renderComponentSlot = $runtime_lazy7("renderComponentSlot", "Halogen.VDom.Driver", function() {
            return function(cs) {
              var renderChild = read(renderChildRef)();
              var rsx = renderChild(cs)();
              var node = getNode(rsx);
              return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
            };
          });
          var patch = $lazy_patch(91);
          var render3 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render3;
        };
        var buildAttributes = buildProp(handler3);
        return {
          buildWidget: buildWidget2,
          buildAttributes,
          document: document3
        };
      };
    };
  };
  var renderSpec = function(document3) {
    return function(container) {
      var render3 = function(handler3) {
        return function(child2) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do2() {
                  var renderChildRef = $$new(child2)();
                  var spec = mkSpec(handler3)(renderChildRef)(document3);
                  var machine = buildVDom(spec)(v);
                  var node = extract2(machine);
                  $$void(functorEffect)(appendChild(node)(toNode2(container)))();
                  return {
                    machine,
                    node,
                    renderChildRef
                  };
                };
              }
              ;
              if (v1 instanceof Just) {
                return function __do2() {
                  write(child2)(v1.value0.renderChildRef)();
                  var parent2 = parentNode2(v1.value0.node)();
                  var nextSib = nextSibling(v1.value0.node)();
                  var machine$prime = step(v1.value0.machine, v);
                  var newNode = extract2(machine$prime);
                  when(applicativeEffect)(not(heytingAlgebraFunction(heytingAlgebraFunction(heytingAlgebraBoolean)))(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                  return {
                    machine: machine$prime,
                    node: newNode,
                    renderChildRef: v1.value0.renderChildRef
                  };
                };
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [v1.constructor.name]);
            };
          };
        };
      };
      return {
        render: render3,
        renderChild: identity(categoryFn),
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component2) {
    return function(i2) {
      return function(element4) {
        return bind(bindAff)(liftEffect(monadEffectAff)(map(functorEffect)(toDocument)(bindFlipped(bindEffect)(document2)(windowImpl))))(function(document3) {
          return runUI(renderSpec(document3)(element4))(component2)(i2);
        });
      };
    };
  };

  // output/Gesso/index.js
  var runGessoAff = runHalogenAff;
  var run3 = function(component2) {
    return function(input4) {
      return function(element4) {
        return bind(bindAff)(runUI2(component2)(input4)(element4))(function() {
          return pure(applicativeAff)(unit);
        });
      };
    };
  };

  // output/Example.UnitGrid.Main/index.js
  var render2 = function(v) {
    return function(v1) {
      return function(scale2) {
        return function(context) {
          var drawGridLine = function(i2) {
            var n = function(v2) {
              return v2 / 10;
            }(toNumber(i2));
            return strokePath(context)(function __do2() {
              moveTo(context)(scale2.x.toCr(-n))(scale2.y.toCr(-1))();
              lineTo(context)(scale2.x.toCr(-n))(scale2.y.toCr(1))();
              moveTo(context)(scale2.x.toCr(n))(scale2.y.toCr(-1))();
              lineTo(context)(scale2.x.toCr(n))(scale2.y.toCr(1))();
              moveTo(context)(scale2.x.toCr(-1))(scale2.y.toCr(-n))();
              lineTo(context)(scale2.x.toCr(1))(scale2.y.toCr(-n))();
              moveTo(context)(scale2.x.toCr(-1))(scale2.y.toCr(n))();
              return lineTo(context)(scale2.x.toCr(1))(scale2.y.toCr(n))();
            });
          };
          var drawGridLines = function __do2() {
            setStrokeStyle(context)("black")();
            setLineWidth(context)(scale2.width.toCr(5e-3))();
            return sequence_(applicativeEffect)(foldableArray)(map(functorArray)(drawGridLine)(range(1)(10)))();
          };
          var drawCross = function(x) {
            return function(y) {
              return function(length9) {
                return strokePath(context)(function __do2() {
                  moveTo(context)(x - scale2.width.toCr(length9))(y)();
                  lineTo(context)(x + scale2.width.toCr(length9))(y)();
                  moveTo(context)(x)(y - scale2.height.toCr(length9))();
                  return lineTo(context)(x)(y + scale2.height.toCr(length9))();
                });
              };
            };
          };
          var drawMouseClicked = function(mxy) {
            var y$prime = function($25) {
              return function(v2) {
                return v2 / 1e3;
              }(toNumber(round2(function(v2) {
                return v2 * 1e3;
              }(scale2.y.toVb($25)))));
            };
            var x$prime = function($26) {
              return function(v2) {
                return v2 / 1e3;
              }(toNumber(round2(function(v2) {
                return v2 * 1e3;
              }(scale2.x.toVb($26)))));
            };
            var text6 = function() {
              if (mxy instanceof Nothing) {
                return "Nothing)";
              }
              ;
              if (mxy instanceof Just) {
                return show(showNumber)(x$prime(getX(positionedPoint)(mxy.value0))) + (", " + (show(showNumber)(y$prime(getY(positionedPoint)(mxy.value0))) + ")"));
              }
              ;
              throw new Error("Failed pattern match at Example.UnitGrid.Main (line 113, column 12 - line 115, column 82): " + [mxy.constructor.name]);
            }();
            var size4 = show(showInt)(floor2(scale2.width.toCr(0.2)));
            return function __do2() {
              setFont(context)(size4 + "px 'Courier New'")();
              setFillStyle(context)("black")();
              setTextAlign(context)(AlignCenter.value)();
              fillText(context)("Clicked: (" + text6)(scale2.x.toCr(0))(scale2.y.toCr(-1.1))();
              if (mxy instanceof Nothing) {
                return unit;
              }
              ;
              if (mxy instanceof Just) {
                setStrokeStyle(context)("black")();
                setLineWidth(context)(scale2.width.toCr(0.01))();
                strokePath(context)(arc(context)({
                  x: getX(positionedPoint)(mxy.value0),
                  y: getY(positionedPoint)(mxy.value0),
                  radius: scale2.width.toCr(0.05),
                  start: 0,
                  end: tau,
                  useCounterClockwise: false
                }))();
                return drawCross(getX(positionedPoint)(mxy.value0))(getY(positionedPoint)(mxy.value0))(0.05)();
              }
              ;
              throw new Error("Failed pattern match at Example.UnitGrid.Main (line 98, column 5 - line 105, column 51): " + [mxy.constructor.name]);
            };
          };
          var drawMouseCursor = function(point) {
            return function __do2() {
              setStrokeStyle(context)("black")();
              setLineWidth(context)(scale2.width.toCr(0.01))();
              return drawCross(getX(positionedPoint)(point))(getY(positionedPoint)(point))(0.05)();
            };
          };
          var drawAxes = function __do2() {
            setStrokeStyle(context)("black")();
            setLineWidth(context)(scale2.width.toCr(0.015))();
            return drawCross(scale2.x.toCr(0))(scale2.y.toCr(0))(1)();
          };
          var clearBackground = function __do2() {
            setFillStyle(context)("white")();
            return fillRect(context)(scale2.toRectangle(dimensionedDimensions)(scale2.screen))();
          };
          return function __do2() {
            clearBackground();
            drawAxes();
            drawGridLines();
            drawMouseClicked(v.clicked)();
            return traverse_(applicativeEffect)(foldableMaybe)(drawMouseCursor)(v.mousePos)();
          };
        };
      };
    };
  };
  var mouseDown = /* @__PURE__ */ function() {
    var getMousePos = function(event) {
      return function(v) {
        return function(v1) {
          return function(state3) {
            return new Just(function() {
              var $22 = {};
              for (var $23 in state3) {
                if ({}.hasOwnProperty.call(state3, $23)) {
                  $22[$23] = state3[$23];
                }
                ;
              }
              ;
              $22.clicked = new Just(fromMouseEvent(event));
              return $22;
            }());
          };
        };
      };
    };
    return mkInteraction(onMouseDown)(getMousePos);
  }();
  var input3 = /* @__PURE__ */ function() {
    return {
      name: "test-app",
      localState: {
        mousePos: Nothing.value,
        clicked: Nothing.value
      },
      app: {
        window: fullscreen,
        render: render2,
        update: defaultApp.update,
        output: defaultApp.output,
        input: defaultApp.input
      },
      viewBox: fromPointAndSize(fromXAndY({
        x: -1.5,
        y: -1.5
      }))(fromWidthAndHeight({
        width: 3,
        height: 3
      })),
      interactions: {
        base: $$default.base,
        clipboard: $$default.clipboard,
        focus: $$default.focus,
        keyboard: $$default.keyboard,
        touch: $$default.touch,
        drag: $$default.drag,
        mouse: [mousePosition, mouseDown],
        wheel: $$default.wheel
      }
    };
  }();
  var main2 = /* @__PURE__ */ runGessoAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
    return run3(component(monadAffAff))(input3)(body2);
  }));

  // <stdin>
  main2();
})();
