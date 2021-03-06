# Update system notes

These are notes on how the Bcc wallet launcher and update system
currently work.

## Old Design

### Diagram

This is the `frontendOnlyScenario` from `bcc-sl/tools/src/launcher/Main.hs`.
Do not worry about `serverScenario` - replace it completely.

The Klarity install is always configured to run the launcher in
`frontendOnlyMode`. The other code (`clientScenario`) is dead
code. The diagram describes `frontendOnlyMode`, which goes through the
`node-ipc` system.

![sequence-chart](update-system-old.png)

#### Running the update system

The very first thing that `bcc-launcher` does, before starting the
frontend, is check for a previously downloaded update.

If there is a file at `updaterPath`, it runs the updater, which is a
bit wrong.

There are two code paths, depending on whether launcher is running on Windows.

##### macOS and Linux

To run the updater on non-Windows systems, it will spawn
`${updaterPath} ${updaterArgs} ${updateArchive}`.

After the update subprocess exits successfully, it hashes the
`updateArchive` file, checks that the wallet database has this hash,
then deletes the `updateArchive` file.

##### Windows

To run the updater on Windows systems, it will create a `.bat` file
then execute it. The temporary `.bat` file runs the following steps:

1. Kill the `bcc-launcher` process with `TaskKill /PID`
2. Run the downloaded installer for the new version.
3. Delete the installer file.
4. Run `bcc-launcher` with the same command line arguments that
   the previous launcher was run with.
5. Delete the `.bat` file.

#### Starting Klarity

Klarity is started by running Electron, with the main Javascript file
being in a subdirectory (the exact filename is configured by the
`main` key of `package.json`)

The Klarity Window title bar is configured using the `productName`
key of `package.json`.

Klarity uses a number of variables at runtime.

| Variable          | Description                                             | Set by   |
| ----------------- |:------------------------------------------------------- | -------- |
| `NETWORK`         | Used to show the user what network Klarity is          | Build    |
|                   | connecting to. One of `mainnet`, `staging`, `testnet`.  |          |
| `REPORT_URL`      | The base URL for posting Klarity automatic bug         | Build    |
|                   | reports.                                                |          |
| `API_VERSION`     | Used to show the Bcc version in the About box.      | Build    |
| `LAUNCHER_CONFIG` | Path to a launcher config YAML file.                    | Launcher |

The variables listed as being set by "Build" are in fact hard-coded
using string substitution at installer build-time, but this need not
be the case.

#### Starting `bcc-node`

Klarity spawns the `bcc-node` process using
[child_process](https://nodejs.org/api/child_process.html). The
command-line parameters are built from the file specified by
`LAUNCHER_CONFIG`.

After starting the `bcc-node` process, it then uses the nodejs API
to pass "messages" to the subprocess (see
`bcc-sl/node-ipc/src/Bcc/NodeIPC.hs`).


#### Applying updates

To apply an update, `bcc-launcher` just loops back to the start,
where the update system will immediately run.

#### Update server

The update server is a S3 bucket managed by devops, accessible by
HTTPS. Before proposing an update on the blockchain, the installer
files are uploaded to the updates bucket, named with their Blake2b
hash.

As an example: [the testnet update server](https://updates-bcc-testnet.s3.amazonaws.com/index.html)



## New design

The old design can be simplified and improved on. At the very least,
dead code should be removed.

### Requirements

#### In scope for `bcc-shell`

- Handles updating the software when Klarity user clicks "Yes" to
  apply a new update (in current design, this means responding when
  Klarity exits with status 20).

- Responds correctly when Klarity exits with status 21 or 22.

- Works on Windows. Note the possible portability issues surrounding:
   - Output redirection
   - Exclusive file locking
   - Paths
   - Killing processes
   - Different installation methods

- Klarity is able to handle the wallet node crashing. The wallet node
  is able to restart after crashing.

- Works well for Klarity development.

- Multiple Klarity instances for different networks can run at the
  same time. Automatic updates for one network do not interfere with a
  Klarity instance running on another network.

#### New requirements

- Works on Linux for single-file executable containers such as
  AppImage or Snap. These executable formats require that the
  `bcc-launcher` process be terminated and restarted for updates.

- All code is the same regardless of network, and the network is
  selectable as a command line option.

- No more launcher YAML file.

- Launcher code has automatic tests which are executed on all target
  platforms.

#### Out of scope for `bcc-shell`

- Update system for nodes which aren't running inside Klarity. For
  example, exchange wallets and staking nodes. These users will handle
  updating their node software.

- Sending node crash reports to report-server. Nobody looks at
  these. Klarity has its own crash reporting feature.

- Running multiple bcc-node backends at the same time within a
  single instance of Klarity.

- Switching between networks at runtime.

#### Questions

- Is it necessary to have two processes `bcc-launcher` and
  `bcc-node`, or can they be combined?

- Is there a more "standard" way for electron apps to start their
  backend servers?
