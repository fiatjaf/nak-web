# nostr web army knife

## a toolkit for debugging all things nostr as a webpage:

![2025-03-16-213105_1454x948_scrot](https://github.com/user-attachments/assets/1f7b9bcb-9b2d-4139-9d11-4b324e4916fe)

written in [scala](https://scala-lang.org/) with [calico](https://www.armanbilge.com/calico/) and [snow](https://github.com/fiatjaf/snow)

check it out on https://nwak.nostr.technology/

### building

here is one way you can build and host a local copy of `nwak`:
1. install [nix](https://nixos.org) the package manager, and make sure [flakes](https://wiki.nixos.org/wiki/Flakes) are enabled
2. checkout this repo and `cd` into it
3. `nix develop` will get you into a dev environment with all the things (`sbt`)
4. `sbt fullLinkJS/esBuild` will build the all important `bundle.js` file
5. `python3 -m http.server 8743` will serve up the html/js app at http://localhost:8743

### developing locally

after step 3 of the the above, run `just` if you have it installed.

that should set up a process that will continuously recompile the javascript while also serving the webpage at http://localhost:8743
