load('rt/util.js');

function foo(n) {
  return (function($x) {
    return (function($n, $c) {
      if ((function($x) {
        return (arguments[0] > 1);
      })($n)) return (function($x) {
        return true;
      })($n);
      else return ($c)($n);
    })($x, fail);
  })(n);
}
function goo(n) {
  return (function($x) {
    return (function($n, $c) {
      if ((function($x) {
        return (patListTest)([any, function($x) {
          return ($x[1] === 2);
        }], $x);
      })($n)) return (function($x) {
        return $x[0];
      })($n);
      else return ($c)($n);
    })($x, fail);
  })(n);
}(expectEQ)("foo(20)", "true");
(expectEQ)("goo([1,2])", "true");
(expectFAIL)("foo(1)");
