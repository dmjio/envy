let
  pkgs = import <nixpkgs> {};
  overrides = self: super: {
    envy = self.callCabal2nix "envy" ./. {};        
  };
  hPkgs = pkgs.haskellPackages.override { inherit overrides; };
in
{
  inherit pkgs;
  inherit (hPkgs) envy;
}
