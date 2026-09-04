const { test } = require("node:test");
const assert = require("node:assert/strict");
const {
  isValidVersion,
  isValidTag,
  tagForVersion,
  tagMatchesVersion,
  compareVersions,
} = require("./version-utils.js");

test("isValidVersion accepts X.Y.Z", () => {
  assert.strictEqual(isValidVersion("0.10.2"), true);
  assert.strictEqual(isValidVersion("1.0.0"), true);
});

test("isValidVersion rejects non X.Y.Z strings", () => {
  assert.strictEqual(isValidVersion("v0.10.2"), false);
  assert.strictEqual(isValidVersion("0.10"), false);
  assert.strictEqual(isValidVersion("0.10.2-beta"), false);
  assert.strictEqual(isValidVersion(""), false);
});

test("isValidTag accepts vX.Y.Z", () => {
  assert.strictEqual(isValidTag("v0.10.2"), true);
});

test("isValidTag rejects non vX.Y.Z strings", () => {
  assert.strictEqual(isValidTag("0.10.2"), false);
  assert.strictEqual(isValidTag("v0.10"), false);
  assert.strictEqual(isValidTag("V0.10.2"), false);
});

test("tagForVersion prefixes with v", () => {
  assert.strictEqual(tagForVersion("0.10.2"), "v0.10.2");
});

test("tagMatchesVersion matches the exact prefixed tag", () => {
  assert.strictEqual(tagMatchesVersion("v0.10.2", "0.10.2"), true);
  assert.strictEqual(tagMatchesVersion("v0.10.3", "0.10.2"), false);
  assert.strictEqual(tagMatchesVersion("0.10.2", "0.10.2"), false);
});

test("compareVersions orders by major, then minor, then patch", () => {
  assert.strictEqual(compareVersions("0.10.2", "0.10.1"), 1);
  assert.strictEqual(compareVersions("0.10.1", "0.10.2"), -1);
  assert.strictEqual(compareVersions("0.10.2", "0.10.2"), 0);
  assert.strictEqual(compareVersions("1.0.0", "0.99.99"), 1);
  assert.strictEqual(compareVersions("0.9.10", "0.9.9"), 1);
});
