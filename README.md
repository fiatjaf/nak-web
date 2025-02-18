# nostr army knife

## a toolkit for debugging all things nostr as a webpage:

![](https://user-images.githubusercontent.com/1653275/227681805-0cd20b39-de0d-4fcb-abb4-de3283404e8f.png)

written in [scala](https://scala-lang.org/) with [calico](https://www.armanbilge.com/calico/) and [snow](https://github.com/fiatjaf/snow)

check it out on https://nak.nostr.com/

### building

Here is one way you can build and host a local copy of `nak-web`:
1. install [Nix](https://nixos.org) the package manager, and make sure [flakes](https://wiki.nixos.org/wiki/Flakes) are enabled
2. checkout this repo and `cd` into it
3. `nix develop` will get you into a dev environment with all the things (`sbt`)
4. `sbt fullLinkJS/esBuild` will build the all important `bundle.js` file
5. `python3 -m http.server 8743` will serve up the html/js app at [localhost:8743](http://localhost:8743)
6. (optional) `./build-gh-pages.sh` if you want to create a local branch called "gh-pages" with only
  the necessary things for serving (such as by github pages).
