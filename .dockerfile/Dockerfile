# NOTE maybe add a "RUN echo ..." to force a rebuild, or --no-cache

# FROM ocaml/opam:ubuntu - latest doesn't work due to problems with ppx_deriving deps
# 17.04 doesn't work due to missing archives at ubuntu? or stale packages info?
FROM ocaml/opam:ubuntu-16.04_ocaml-4.04.2
# RUN apt-get -y update

RUN opam list
RUN opam update

RUN eval `opam config env` && opam pin add tjr_lib https://github.com/tomjridge/tjr_lib.git

RUN git clone https://github.com/tomjridge/p1_core.git
RUN eval `opam config env` && opam pin add p1_core ./p1_core

RUN eval `opam config env` && cd p1_core && make all p1_examples.native && ./p1_examples.native
