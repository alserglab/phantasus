// Karma configuration
// Generated on Fri Jul 21 2017 20:14:52 GMT+0300 (MSK)

module.exports = function(config) {
  config.set({

    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: '',

    hostname: '127.0.0.1',
    // optional, but can help in some setups:
    listenAddress: '127.0.0.1',

// frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['jasmine'],

    plugins: [
      require( 'karma-jasmine' ),
      require( 'karma-firefox-launcher' )
    ],

    // list of files / patterns to load in the browser
    files: [
      {pattern: './js/jquery-2.2.4.min.js', watched: false},
      {pattern: './jasmine/test_files/**', watched: false, included: false, served: true, nocache: false},
      {pattern: './*.proto', watched: false, included: false, served: true, nocache: false},
      './js/phantasus-external-other.min.js',
      './js/phantasus-external-pdfkit-xlsx.min.js',
      './js/phantasus-external-plotly-echarts.min.js',
      './js/phantasus.js',
      './jasmine/spec/log-spec-name.js',
      './jasmine/spec/fail-on-exception.js',
      './jasmine/matchers/*.js',
      './jasmine/run_ocpu.js',
      './jasmine/spec/*[tT]est.js',
      './jasmine/spec/global-sleep.js'
    ],

    proxies: {
      '/jasmine/test_files/': '/base/jasmine/test_files/',
      '/message.proto': '/base/message.proto',
      '/test_files/': '/base/jasmine/test_files/'
    },


    // list of files to exclude
    exclude: ['jasmine/spec/marker_selection_test.js',
              'jasmine/spec/nearest_neighbors_test.js',
              'jasmine/spec/qnorm_test.js',
              'jasmine/spec/save_dataset_test.js'],


    // preprocess matching files before serving them to the browser
    // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
    },


    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['progress'],


    // web server port
    port: 9876,


    // enable / disable colors in the output (reporters and logs)
    colors: true,


    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_DEBUG,


    // enable / disable watching file and executing tests whenever any file changes
    autoWatch: true,


    // start these browsers
    // available browser launchers: https://npmjs.org/browse/keyword/karma-launcher
    browsers: ['FirefoxHeadless'],

    // Continuous Integration mode
    // if true, Karma captures browsers, runs the tests and exits
    singleRun: false,

    // Concurrency level
    // how many browser should be started simultaneous
    concurrency: Infinity,

    // captureTimeout: 60000, // it was already there
    browserDisconnectTimeout : 10000,
    browserDisconnectTolerance : 1,
    browserNoActivityTimeout : 240000 //by default 10000
  })
};
