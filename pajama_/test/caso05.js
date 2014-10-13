load('rt/util.js');

function sumar(a) {
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
          return ($x[0] + (sumar)((function($x) {
            return $x.slice(1)[0];
          })($x)));
        })($n);
        else return ($c)($n);
      })($x, fail);
    });
  })(a);
}(expectEQ)("sumar([1,2,3])", "6");
(expectFAIL)("sumar(1)");
