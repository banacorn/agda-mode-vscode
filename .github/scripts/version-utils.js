// Pure version/tag logic shared by release.yml's tag and deploy jobs, pulled
// out here so it has one implementation and can be unit tested directly
// instead of only through the inline bash/node in the workflow file.

const VERSION_REGEX = /^[0-9]+\.[0-9]+\.[0-9]+$/;
const TAG_REGEX = /^v[0-9]+\.[0-9]+\.[0-9]+$/;

function isValidVersion(version) {
  return VERSION_REGEX.test(version);
}

function isValidTag(tag) {
  return TAG_REGEX.test(tag);
}

function tagForVersion(version) {
  return `v${version}`;
}

function tagMatchesVersion(tag, version) {
  return tag === tagForVersion(version);
}

// Returns 1 if `current` is a greater semver than `previous`, -1 if lesser,
// 0 if equal. Both must already be validated with isValidVersion.
function compareVersions(current, previous) {
  const c = current.split(".").map(Number);
  const p = previous.split(".").map(Number);
  for (let i = 0; i < 3; i++) {
    if (c[i] > p[i]) return 1;
    if (c[i] < p[i]) return -1;
  }
  return 0;
}

module.exports = {
  isValidVersion,
  isValidTag,
  tagForVersion,
  tagMatchesVersion,
  compareVersions,
};

if (require.main === module) {
  const [command, ...args] = process.argv.slice(2);

  switch (command) {
    case "is-valid-version":
      process.exit(isValidVersion(args[0]) ? 0 : 1);
    case "is-valid-tag":
      process.exit(isValidTag(args[0]) ? 0 : 1);
    case "tag-matches-version":
      process.exit(tagMatchesVersion(args[0], args[1]) ? 0 : 1);
    case "is-increase": {
      const [current, previous] = args;
      if (!isValidVersion(current)) {
        console.error(`package.json version '${current}' is not a valid X.Y.Z semver`);
        process.exit(1);
      }
      if (!isValidVersion(previous)) {
        console.error(`latest tag version '${previous}' is not a valid X.Y.Z semver, cannot compare`);
        process.exit(1);
      }
      if (compareVersions(current, previous) <= 0) {
        console.error(`package.json version went from ${previous} to ${current}, which is not an increase`);
        process.exit(1);
      }
      process.exit(0);
    }
    default:
      console.error(`Unknown command: ${command}`);
      process.exit(1);
  }
}
