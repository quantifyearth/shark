FROM ocaml/opam:debian-12-ocaml-5.1 as build
USER 1000:1000
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard 3505e93828fa76861e82d09d92a37a6272d46da5 && opam update
COPY --chown=opam shark.opam /src/
WORKDIR /src
RUN opam install -y --deps-only --with-test .
ADD --chown=opam . .
RUN opam exec -- dune build @runtest @install @check