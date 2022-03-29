#TODO update repo noted below (nix)
#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nix-prefetch-git

set -euo pipefail

NIX_DIR=`dirname $0`

nix-prefetch-git https://github.com/The-Blockchain-Company/tbco-nix \
                 > $NIX_DIR/tbco-nix-src.json
