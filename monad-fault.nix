let
  pkgs = import nix/pkgs.nix;
in pkgs.haskell.packages.ghc802.callCabal2nix "monad-fault" ./. {}