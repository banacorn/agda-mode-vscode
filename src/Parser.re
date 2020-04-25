
let filepath = s => {
  // remove the Windows Bidi control character
  let removedBidi =
    if (Js.String.charCodeAt(0, s) === 8234.0) {
      Js.String.sliceToEnd(~from=1, s);
    } else {
      s;
    };

  // normalize the path with Node.Path.normalize
  let normalized = Node.Path.normalize(removedBidi);

  // replace Windows' stupid backslash with slash
  let replaced = Js.String.replaceByRe([%re "/\\\\/g"], "/", normalized);

  replaced;
};
