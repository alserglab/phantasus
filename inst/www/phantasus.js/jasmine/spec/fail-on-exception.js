// ChatGPT-generated code
/*
(function () {
  // Uncaught synchronous exceptions
  window.onerror = function (message, source, lineno, colno, error) {
    const msg = `Uncaught error: ${message} (${source}:${lineno}:${colno})`;
    console.error(msg);
    if (typeof fail === 'function') fail(msg);
    return false; // let Karma also log it
  };

  // Unhandled promise rejections (async errors)
  window.addEventListener('unhandledrejection', function (event) {
    const msg = `Unhandled promise rejection: ${event.reason}`;
    console.error(msg);
    if (typeof fail === 'function') fail(msg);
  });
})();
*/

(function () {
  // Synchronous uncaught errors
  window.addEventListener('error', function (event) {
    // event.error may be undefined in some browsers, so fall back to message
    var err = event.error || event.message || event;
    if (window.__karma__ && typeof window.__karma__.error === 'function') {
      window.__karma__.error(err);
    } else {
      throw err;
    }
  });

  // Unhandled Promise rejections (your deferred case)
  window.addEventListener('unhandledrejection', function (event) {
    var reason = event.reason || event;
    if (window.__karma__ && typeof window.__karma__.error === 'function') {
      window.__karma__.error(reason);
    } else {
      throw reason;
    }
  });
})();
