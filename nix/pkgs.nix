let rev = "120b013e0c082d58a5712cde0a7371ae8b25a601";
in import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {}