with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "guide";
  inherit ghc;
  buildInputs = [ git ncurses zlib postgresql ];
  LANG = "en_US.UTF-8";
}
