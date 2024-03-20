FROM ocaml/opam:debian-11-ocaml-5.1 as build
RUN  sudo apt-get update && sudo apt-get install -y software-properties-common \
  && sudo apt-add-repository contrib \
  && sudo apt-get update \
  && sudo apt-get install -y spl kmod \
  && sudo apt-get install -y zfsutils-linux zfs-dkms zfs-zed runc
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam
USER 1000:1000
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard 3505e93828fa76861e82d09d92a37a6272d46da5 && opam update
COPY --chown=opam shark.opam /src/
WORKDIR /src
RUN opam install -y --deps-only --with-test .
ADD --chown=opam . .
RUN opam exec -- dune build @runtest @install @check
RUN opam exec -- dune install
RUN src/test/ci.sh zfs