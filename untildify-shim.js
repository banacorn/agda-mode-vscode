// Browser-compatible shim for the untildify package
// In browser environments, tilde paths are not used, so we just return the input unchanged
module.exports = function untildify(pathWithTilde) {
  return pathWithTilde;
};
