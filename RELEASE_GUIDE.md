# Release guide

Commit the changes for the release and submit them to Gerrit for review before
following these steps. The examples use `1.30.0`; replace it with the target
version for other releases.

1. Update `vsn` in `src/klsn.app.src` to the release version.
   For `1.30.0`, use `{vsn, "1.30.0"}`.
   Commit only this `vsn` change after the existing change commits.
   Use the commit subject `Bump version to <version>`, which is
   `Bump version to 1.30.0` in this example.
   Give it a separate Change-Id and submit it to Gerrit as a separate change.

2. Confirm that all Gerrit changes, including the version bump, have merged.
   Record the hash of the merged version bump commit and ask ts-klassen to
   create a lightweight tag on that commit through the Gerrit UI.
   Include the tag name and commit hash in the request.
   Use only the version number as the tag name, such as `1.30.0`, without `v`.
   If the tag is named `v1.30.0`, ask ts-klassen to recreate it without `v`.

3. Verify that the Gerrit tag points to the correct commit.

   ```bash
   git ls-remote --tags origin 'refs/tags/1.30.0'
   ```

   Confirm that the hash on the tag line matches the hash recorded in step 2.
   If it does not, ask ts-klassen to correct the tag through the Gerrit UI,
   then check again.

4. Once the tag points to the correct commit, run:

   ```bash
   qrexec-client-vm github user.Push+klsn
   ```

   The qrexec policy is `ask`. If the command stalls, keep the pending
   invocation running in the background and ask the user to approve it.
   Explain that the GitHub push is waiting for approval under the qrexec policy.
   Do not start duplicate invocations. After approval, check the exit code
   and output.

5. After the push succeeds, verify the tag and commit hash on GitHub.

   ```bash
   git ls-remote --tags https://github.com/ts-klassen/klsn.git \
     'refs/tags/1.30.0'
   ```

   Confirm that the target tag exists in `github.com/ts-klassen/klsn` and
   that the hash on the tag line matches the hash on Gerrit.
   The release is complete once this check passes.

6. After the release is complete, return to `master` and fast-forward it
   to the latest `origin/master`.

   ```bash
   git switch master
   git pull --ff-only origin master
   ```

## Completion criteria

- The application version bump is committed as a separate Gerrit change.
- `vsn` in `src/klsn.app.src` matches the release version.
- All Gerrit changes, including the version bump, have merged.
- ts-klassen has used the Gerrit UI to create a lightweight tag without `v`
  on the merged version bump commit, and its target has been verified.
- `qrexec-client-vm github user.Push+klsn` has succeeded.
- The same tag exists on GitHub, and its commit hash matches Gerrit.
- The working branch is `master`, fast-forwarded to the latest `origin/master`.
