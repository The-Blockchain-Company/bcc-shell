#TODO verify links are setup and Klarity
`bcc-launcher` is an executable which is used to launch
[Klarity](https://github.com/The-Blockchain-Company/Klarity) as well as handling
restart/exit process.

The high-level overview:

![launcher_updater_flowchart](https://user-images.githubusercontent.com/6264437/66833560-54c90280-ef5c-11e9-95cf-df8f9a1eb9dc.jpeg)

Key responsibilities include:

1. Generate TLS certificates that are needed for Klarity to communicate with [klarity](https://github.com/The-Blockchain-Company/klarity).
2. Setup environment variables that are needed for Klarity to run.
3. Launching Klarity wallet as well as handling restart/exit process.
4. Restart Klarity in GPU-safe mode.
4. Run an update system. For more details about how update is being done, please see
[wiki](https://github.com/The-Blockchain-Company/bcc-shell/wiki/How-update-is-being-done-in-bcc)

### What it does

`bcc-launcher` will first setup environment variables that are needed for
Klarity to run. It then generates TLS certificates that Klarity uses to
communicate with `klarity`. After that, `bcc-launcher` will spawn
Klarity as a child process. Depending on how Klarity exits, `bcc-launcher`
will perform restart process, then run Klarity again. Restart process includes:

1. Running update system when user asks Klarity to run update.
2. Switching between GPU-safe mode and Normal mode.

When Klarity exits, the launcher will clean itself up, and exits along with
Klarity.

## Developing `bcc-launcher`

1. Go into nix-shell by running `nix-shell` command
2. Build the project with stack `stack build --nix`

## Generating `bcc-launcher` executable

### Using stack + nix

1. Go into nix-shell: `nix-shell`
2. Build the project with stack: `stack build --nix`
3. Run `stack install` command to build `bcc-launcher` executable

```terminal
stack --nix install bcc-launcher:bcc-launcher --local-bin-path path/to/bin
```

### Perform cross-compilation

`bcc-launcher` is also capabale of generating portable Windows executable 
on Linux machine by using `nix-build` command.

1. Run the script below.

```terminal
nix-build release.nix -A nix-tools.cexes.x86_64-pc-mingw32-bcc-shell.bcc-launcher.x86_64-linux
```

2. If the build is successful, there should be a `bcc-launcher.exe` file in the 
`result/bin/` directory.

## Stubbing the exit codes for the wallet and the updater (AKA state machine testing)

`bcc-launcher` has the support for stubbing out the wallet and the updater.
This enables controlled testing and supports QA and people who want to see how it behaves without
actually providing all the required environment and configuration.

```
                                               +------------------------+
                                               |                        |
                                               |                        |
                                               |                        |
                           +------------------>+        Wallet          |
                           |                   |                        |
                           |                   |                        |
                           |                   |                        |
                           |                   |                        |
+--------------------------+                   +------------------------+
|                          |
|                          |
|                          |
|                          |
|                          |
|        Launcher          |
|                          |
|                          |
|                          |
|                          |
|                          |
|                          |
+--------------------------+                 +---------------------------+
                           |                 |                           |
                           |                 |                           |
                           |                 |                           |
                           |                 |                           |
                           +---------------->+          Updater          |
                                             |                           |
                                             |                           |
                                             |                           |
                                             |                           |
                                             |                           |
                                             +---------------------------+

```

We can simply imagine that we stub out the wallet and the updater with a list of exit codes.

So when we run something like:
```
stack exec bcc-launcher -- --config ./bcc-launcher/configuration/launcher/launcher-config.demo.yaml --wallet-exit-codes "[CLIExitCodeFailure 21, CLIExitCodeFailure 22, CLIExitCodeFailure 20, CLIExitCodeSuccess]" --updater-exit-codes "[CLIExitCodeFailure 1, CLIExitCodeSuccess]"
```

We stub out both the wallet and the updater with a list of exit codes we defines.
So the first time we run the wallet, the wallet will return the first exit code.
When we run it the second time, the wallet will return the second exit code.
And so on.
After the fourth time (there are 4 exit codes) the real wallet execution will be run
and the exit code we get will be from the actual wallet execution.

It works the same for the updater. It will return stubbed exit codes in order until
it exhaust the exit codes and then it will run the actual updater function.

_If we don't define either of both of these, the real function will be used._

## Manual testing on Windows

`bcc-launcher` have a script that will package everything you need to test the
launcher on Windows platform.

Run the script below and you'll have `updater_test.zip` file located in the `result/`
directory.

```terminal
nix-build updater-test-win.nix
```


