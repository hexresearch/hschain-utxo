# hschain-utxo-nix

The build scripts for robust and fast incremental building of hschain-utxo.

You need:
- installed nix
- configured ssh access to other private repos on hexresearch github

# Private repo clone configure

## NixOS

We have special NixOS module to support SSH agent passing to nix daemon. See [private-ssh-pipe.nix](./private-ssh-pipe.nix).
To use it, simply copy it to `/etc/nixos/private-ssh-pipe.nix` and modify your `configuration.nix`:
``` nix
imports =
  [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./private-ssh-pipe.nix
  ];

services.private-ssh-pipe.enable = true;
services.private-ssh-pipe.user = "user"; # Here insert name of your user
```
Switch to new configuration `sudo nixos-build switch` and relog/reboot (need to update $NIX_PATH system wide).

To test that you have all set right:
```
$ systemctl status --user ssh-agent-pipe.service # should be active
$ echo $NIX_PATH # should contains <ssh-config-file> and <ssh-auth-socket>
```

If you get errors about `sudo` not found in service log, execute the following as user before starting of service:
```
systemctl --user import-environment PATH
```

## Not NixOS

You shouldn't do nothing, the clone should work as it.

# Building of backend

To enter build environment you need to just:
```
./shell.sh
```

Note: if you get errors about `repository doesn't exist`, try `ssh-add` key that has access to the repo.

After all dependencies are built, you will be in root folder pf the project in shell, where
`ghc`, `ghcid` and `cabal` are configured to work with all internal packages.

To build the backend:
```
cabal new-build all
```

To run result of build:
```
cabal new-run hschain-utxo-service -- <pass args here>
```

To run tests:
```
cabal new-test all
```

If you want a functionality similar to `cabal --file-watch` to rebuild when files change:
```
ghcid -c "cabal new-repl hschain-utxo-lang"
```

To exit the shell, type `exit`. Besides, you might want to kill `socat` that is left at the background (only for NixOS):
```
sudo pkill -TERM socat
```

You can trigger non incremental build:
```
./build.sh
```

# Building frontend

See [README.md](../frontend/README.md)

# Adding dependencies

* You can add external dependencies (that are public third-party packages) using:
```
cabal2nix cabal://<package-name>-<version> > ./derivations/<package-name>.nix
```
Where you can use url for git repo instead of `cabal://`. Also you can add `--no-check` or `--no-haddock`.
The public dependencies are automatically added to environment from `derivations` folder when you enter the shell.

* External dependencies in private repos. Do the same as with a public dependency, but patch resulted nix file to
fetch from private repo:
```
src = tryEval <thundermint-src> (fetchgitPrivate pkgConfig.thundermint);
```  
And add coresponding item to `versions.json`:
```
"thundermint":  {
  "url":    "git@github.com:hexresearch/thundermint.git",
  "rev":    "613a42c24ee8dbe9e16d0c66f106fdd556b0c4a0",
  "sha256": "0kam8qqmj95p85ilrck5c052lj4c1rwr9r9y59chrml0z1ybpa0w"
},
```

You can temporarily use a local version of those dependencies by patching `shell.nix` as:
```
NIX_PATH=ssh-config-file=$SSH_CONFIG:ssh-auth-sock=/tmp/hax:crypto-history-src=./crypto-history:$NIX_PATH nix-shell --command "cd ../hschain-utxo; return"
```

* Internal package. Internal packages are added automatically by the script
that searches the project directory for submodules


# Updating dependencies

* Public dependencies, just call `cabal2nix` like in `Adding dependencies` part.
* Internal dependencies, update `versions.json` file.

# Building docker container

TODO

# Using netrc file

You can use netrc file to authentificate clone of private repos. To perform this, you need:

* Change url to `https` in `versions.json`

* Create file `.netrc` in the root of the repository, format:
```
machine github.com
username <github login>
login <github login>
password <password or github access token>
```

* Execute `./shell.sh`
