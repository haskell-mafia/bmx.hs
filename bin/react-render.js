#!/usr/bin/env node
"use strict";

var fs = require('fs')
  , through = require('through2')
  , ReactDOMServer = require('react-dom/server')
  , _ = require('lodash')

var dir = process.argv[2];
var frame = process.argv[3];

var data = '';
process.stdin.setEncoding('utf8');
process.stdin
  .pipe(through(
    function(chunk, _, next) {
      data += chunk;
      return next();
    },
    function(done) {
      let partials = fs.readdirSync(dir).reduce(function(b, a) { return _.extend(b, require("../" + dir + "/" + a)); }, {})
      let context = { helpers: {}, partials: partials };
      _.extend(context.helpers, require("../src/BMX/React/helpers.js"));
      let vars = JSON.parse(data);
      let react = partials.test(context, vars, {});
      console.log(ReactDOMServer.renderToStaticMarkup(react));
      return done();
    }
  ))
  .on('error', function() { console.error(arguments) })
  .pipe(process.stdout)
