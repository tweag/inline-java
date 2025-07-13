args:
let pkgs = import (fetchTarball {
             sha256 = "0dcslr2lwfaclfl4pmbwb3yw27bnvwlqiif394d3d66vyd163dvy";
             url = "https://github.com/NixOS/nixpkgs/archive/3e3afe5174c561dee0df6f2c2b2236990146329f.tar.gz";
             }) args;
    stack_ignore_global_hints = pkgs.writeScriptBin "stack" ''
      #!${pkgs.stdenv.shell}
      # Skips the --global-hints parameter to stack. This is
      # necessary when using an unreleased compiler whose hints
      # aren't available yet.
      set -euo pipefail
      
      declare -a args
      for a in "''$@"
      do
          if [[ "$a" != "--global-hints" ]]
          then
              args+=("$a")
          fi
      done
      # Passing --no-nix is necessary on nixos to stop stack from
      # looking for nixpkgs.
      # --system-ghc is also necessary to pick the unreleased compiler
      # from the PATH.
      exec ${pkgs.stack}/bin/stack --no-nix --system-ghc ''${args[@]}
      '';
 in pkgs // { inherit stack_ignore_global_hints; }
