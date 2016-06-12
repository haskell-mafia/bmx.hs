#!/usr/bin/env node
"use strict";

var fs = require('fs')
  , through = require('through2')
  , ReactDOMServer = require('react-dom/server')
  , _ = require('lodash')

var frame = process.argv[2];

var data = '';
process.stdin.setEncoding('utf8');
process.stdin
  .pipe(through(
    function(chunk, _, next) {
      data += chunk;
      return next();
    },
    function(done) {
      let module = require("../" + frame);
      let context = { helpers: {} };
      _.extend(context.helpers, require("../src/BMX/React/helpers.js"));
      let vars = JSON.parse(data);
      let react = module.test(context, vars, {});
      console.log(ReactDOMServer.renderToStaticMarkup(react));
      return done();
    }
  ))
  .on('error', function() { console.error(arguments) })
  .pipe(process.stdout)
