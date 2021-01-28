# Trying to workaround
# https://github.com/NixOS/nixpkgs/issues/105573
#
# by going to a commit before the one introducing the regression.
args:
let pkgs = import (fetchTarball "https://github.com/tweag/nixpkgs/archive/76318102e1177a235ff38872cf4dfb2fc9590a42.tar.gz") args;
    stackWrapper = pkgs.writeScriptBin "stack" ''
      #!${pkgs.stdenv.shell}
      set -euo pipefail
      
      declare -a args
      for a in "''$@"
      do
          if [[ "$a" != "--global-hints" ]]
          then
              args+=("$a")
          fi
      done
      exec ${pkgs.stack}/bin/stack --system-ghc ''${args[@]}
      '';
 in pkgs // { stack_no_global_hints = stackWrapper; }
