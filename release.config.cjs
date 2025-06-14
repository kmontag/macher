/**
 * @type {Partial<import('semantic-release').GlobalConfig>}
 */
const config = {
  // Note that we can't use pre-release channels, since the format of pre-release versions generated
  // by semantic-release is incompatible with Eask (Eask needs a string parseable by
  // `version-to-list`, e.g. 1.0.0alpha1, but the semantic-release format is like 1.0.0-alpha.1).
  branches: ["main"],
  // Plugins can register actions on one or more release steps. Release steps are executed in order,
  // and within a step, plugin actions are executed in the order they appear here. See
  // https://semantic-release.gitbook.io/semantic-release/usage/plugins.
  plugins: [
    "@semantic-release/commit-analyzer",
    "@semantic-release/release-notes-generator",
    "@semantic-release/changelog",
    [
      "@semantic-release/exec",
      {
        // Manual step to update the version string in the package and the Eask config.
        prepareCmd:
          // Update the macher.el version.
          "sed -i 's/;; Version: .*/;; Version: ${nextRelease.version}/' macher.el && " +
          // Update the Eask version.
          'sed -i \'s/(package "macher" "[^"]*"/(package "macher" "${nextRelease.version}"/\' Eask && ' +
          // Verify that the correct string appears in both places.
          'grep -q ";; Version: ${nextRelease.version}" macher.el && echo "macher.el updated correctly" && ' +
          'grep -q \'(package "macher" "${nextRelease.version}"\' Eask && echo "Eask updated correctly" && ' +
          // Verify that linting still passes. This should throw an error if there's somehow a
          // version mismatch, or if the changes somehow broke linting in some other way.
          "make lint && " +
          // Verify that unit tests still pass - we don't need to run the full test suite, but this
          // should verify that no syntax errors have been introduced.
          "make test.unit",
      },
    ],
    // During the `prepare` step, commit changes to files that got updated by other plugins.
    ["@semantic-release/git", { assets: ["CHANGELOG.md", "macher.el", "Eask"] }],
    // Publish a GitHub release during the `publish` step.
    [
      "@semantic-release/github",
      {
        // Don't try to leave comments on issues or pull requests related to the release. The value
        // here is a lodash template string.
        successCommentCondition: "<% return false; %>",
      },
    ],
  ],
};
module.exports = config;
