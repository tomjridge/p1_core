# NOTE maybe add a "RUN echo ..." to force a rebuild, or --no-cache

# FROM ocaml/opam:ubuntu - latest doesn't work due to problems with ppx_deriving deps
# 17.04 doesn't work due to missing archives at ubuntu? or stale packages info?
FROM ocaml/opam:ubuntu-16.04_ocaml-4.04.2
# RUN apt-get -y update

RUN opam list
RUN opam update

RUN opam install core_kernel rresult bos fileutils omd

# NOTE in following not sure why this eval needs to be there -
# shouldn't the default RUN setup the opam env anyway?

RUN git clone https://github.com/tomjridge/tjr_lib.git
RUN eval `opam config env` && make -C tjr_lib

RUN git clone https://github.com/tomjridge/p1_core.git
RUN eval `opam config env` && make -C p1_core all p1_examples.native
RUN cd p1_core && ./p1_examples.native
