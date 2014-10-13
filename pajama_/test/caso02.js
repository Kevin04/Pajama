load('rt/util.js');

function factorialCola(a) {
  return (function($x) {
    return (function($n, $c) {
      if ((function($x) {
        return (patListTest)([function($x) {
          return ($x[0] === 0);
        },
        any], $x);
      })($n)) return (function($x) {
        return $x[1];
      })($n);
      else return ($c)($n);
    })($x, function($x) {
      return (function($n, $c) {
        if ((function($x) {
          return (patListTest)([any, any], $x);
        })($n)) return (function($x) {
          return (factorialCola)([(function($x) {
            return ($x[0] - 1);
          })($x), (function($x) {
            return ($x[0] * $x[1]);
          })($x)]);
        })($n);
        else return ($c)($n);
      })($x, fail);
    });
  })(a);
}
function factorial(n) {
  return (function($x) {
    return (function($n, $c) {
      if ((function($x) {
        return (arguments[0] >= 0);
      })($n)) return (function($x) {
        return (factorialCola)([(function($x) {
          return arguments[0];
        })($x), (function($x) {
          return 1;
        })($x)]);
      })($n);
      else return ($c)($n);
    })($x, function($x) {
      return (function($n, $c) {
        if ((any)($n)) return (function($x) {
          return 0;
        })($n);
        else return ($c)($n);
      })($x, fail);
    });
  })(n);
}
function largo(a) {
  return (function($x) {
    return (function($n, $c) {
      if ((function($x) {
        return (patListTestEmpty)($x);
      })($n)) return (function($x) {
        return 0;
      })($n);
      else return ($c)($n);
    })($x, function($x) {
      return (function($n, $c) {
        if ((function($x) {
          return (patListTest)([any], $x.slice(0, 1)) && (any)($x);
        })($n)) return (function($x) {
          return ((largo)((function($x) {
            return $x.slice(1);
          })($x)) + 1);
        })($n);
        else return ($c)($n);
      })($x, fail);
    });
  })(a);
}
function largoRC(a) {
  return (function($x) {
    return (function($n, $c) {
      if ((function($x) {
        return (patListTest)([function($x) {
          return (patListTestEmpty)($x);
        },
        any], $x);
      })($n)) return (function($x) {
        return $x[1];
      })($n);
      else return ($c)($n);
    })($x, function($x) {
      return (function($n, $c) {
        if ((function($x) {
          return (patListTest)([function($x) {
            return (patListTest)([any], $x[0].slice(0, 1)) && (any)($x);
          },
          any], $x);
        })($n)) return (function($x) {
          return (largoRC)([(function($x) {
            return $x.slice(1);
          })($x), (function($x) {
            return ($x[1] + 1);
          })($x)]);
        })($n);
        else return ($c)($n);
      })($x, fail);
    });
  })(a);
}
function largo2(a) {
  return (function($x) {
    return (function($n, $c) {
      if ((any)($n)) return (function($x) {
        return (largoRC)([(function($x) {
          return arguments[0];
        })($x), (function($x) {
          return 0;
        })($x)]);
      })($n);
      else return ($c)($n);
    })($x, fail);
  })(a);
}(expectEQ)("factorial(5)", "120");
(expectEQ)("factorial(-5)", "0");
(expectEQ)("largo([1,2,3])", "3");
(expectEQ)("largo2([1,2,3])", "3");
(expectFAIL)("largo2(2)");
