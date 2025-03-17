dev:
  #!/usr/bin/env bash
  sbt ~fastLinkJS/esBuild &
  pid1=$!
  python -m http.server 8743
  pid2=$!
  trap "kill $pid1 $pid2" SIGINT SIGTERM SIGQUIT EXIT
  wait

build-prod:
    sbt fullLinkJS/esBuild
