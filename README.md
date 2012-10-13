chess
=====

setup
------------

1. install [leiningen](https://github.com/technomancy/leiningen)
2. clone this repo, cd into directory
3. run 'lein deps' to download dependencies that are listed in project.clj
4. run 'lein cljsbuild once' or 'lein cljsbuild auto' to compile sources in src-cljs ('auto' rebuilds automatically when sources change)
5. run 'lein run' to start a webserver on port 8080
6. browse to localhost:8080
