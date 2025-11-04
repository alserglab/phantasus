afterAll(function (done) {
  // Wait 4 seconds before Karma quits, so that not checked promises
  // has a chance to fail
  setTimeout(done, 4000);
});
