with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "guide";
  inherit ghc;
  buildInputs = [ git ncurses zlib ];
  LANG = "en_US.UTF-8";
}
