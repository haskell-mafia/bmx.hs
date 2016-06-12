"use strict";

exports.each = function (args, iff, els) {
  if (args.length !== 1)
    throw ("Invalid number of arguments to 'each'. Expected 1, got " + args.length);
  let arg = args[0];
  if (Array.isArray(arg)) {
    if (arg.length > 0)
      return arg.map(function(a, i) {
        return iff({index: i, first: i === 0, last: i === arg.length - 1}, a);
      });
    else
      return els({}, {});
  } else {
    let keys = Object.keys(arg)
    if (keys.length > 0)
      return keys.map(function(k, i) {
        return iff({key: k}, k, arg[k]);
      });
    else
      return els({}, {});
  }
};

exports.if = function (args, iff, els) {
  if (args.length !== 1)
    throw ("Invalid number of arguments to 'each'. Expected 1, got " + args.length);
  let arg = args[0];
  if (arg) {
    return iff({}, {});
  } else {
    els({}, {});
  }
};
