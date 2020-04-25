// reason-promise doesn't ship with native support for this PPX, so we simply
// add our own by re-defining the module, including all the stuff from the
// original module, and adding our own function.
include Promise;

let let_ = Promise.flatMapOk;

// This is totally optional. It can be nice sometimes to return a
// non-promise value at the end of a function and have it automatically
// wrapped.
module Wrap = {
  let let_ = (p, f) => Promise.map(p, x => Ok(f(x)));
};
