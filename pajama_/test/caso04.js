load('rt/util.js');

function times13(a) {
  return (function($x) {
    return (function($n, $c) {
      if ((function($x) {
        return (patListTest)([any, any, any], $x);
      })($n)) return (function($x) {
        return ($x[0] * $x[2]);
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
  })(a);
}(expectEQ)("times13([10, 20, 30])", "300");
(expectEQ)("times13([10, 20])", "0");
