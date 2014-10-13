load('rt/util.js');

function size(a) {
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
            return (patListTest)([any], $x[0]) && (any)($x);
          },
          any], $x);
        })($n)) return (function($x) {
          return (size)([(function($x) {
            return $x.slice(1)[0];
          })($x), (function($x) {
            return ($x[1] + 1);
          })($x)]);
        })($n);
        else return ($c)($n);
      })($x, fail);
    });
  })(a);
}
function sumaRec(a) {
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
          return (patListTest)([any], $x) && (any)($x);
        })($n)) return (function($x) {
          return ($x[0] + (sumaRec)((function($x) {
            return $x.slice(1)[0];
          })($x)));
        })($n);
        else return ($c)($n);
      })($x, fail);
    });
  })(a);
}
