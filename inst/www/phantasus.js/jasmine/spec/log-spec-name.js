// ChatGPT-generated code
(function () {
  var currentSpec = null;

  // Track which spec is running
  jasmine.getEnv().addReporter({
    specStarted: function (result) {
      currentSpec = result.fullName;  // "Suite description it description"
    },
    specDone: function () {
      currentSpec = null;
    }
  });

  function wrapConsoleMethod(orig) {
    return function () {
      var args = Array.prototype.slice.call(arguments);
      if (currentSpec) {
        args.unshift('[spec: ' + currentSpec + ']');
      }
      return orig.apply(console, args);
    };
  }

  console.log   = wrapConsoleMethod(console.log);
  console.warn  = wrapConsoleMethod(console.warn);
  console.error = wrapConsoleMethod(console.error);
})();
