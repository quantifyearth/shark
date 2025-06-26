FROM ocaml/opam:debian-11-ocaml-5.1 as build
RUN  sudo apt-get update \
  && sudo apt-get install -y runc
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam
USER 1000:1000
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard eb1c31096c67d2f03b97ad71332ed5f2fc15f762 && opam update
COPY --chown=opam shark.opam shark-ast.opam /src/
WORKDIR /src
RUN opam pin . -yn
RUN opam install -y --deps-only --with-test .
ADD --chown=opam . .
RUN opam exec -- dune build @runtest @install @check
RUN opam exec -- dune install