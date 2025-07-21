args:
let pkgs = import (fetchTarball {
             sha256 = "sha256:0bpw6x46mp0xqfdwbrhnjn6qlb4avglir993n3cdqg8zv4klgllw";
             url = "https://github.com/NixOS/nixpkgs/archive/706eef542dec88cc0ed25b9075d3037564b2d164.tar.gz";
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
