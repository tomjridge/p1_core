set -a # export all vars
#set -x # debug

libname="p1_core"
required_packages="tjr_lib"
description="Core p1 parsing library"


# generic from here ----------------------------------------------------

PKGS="-package $required_packages"
SYNTAX=""

# 8~"pattern-matching is not exhaustive"; 
# 11~"this match case is unused";
# 26~"unused variable s2"
WARN="-w @f@p@u@s@40-8-11-26"

# -thread needed for core
  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   -thread $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt -thread $WARN $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"

mk_cma="$DISABLE_BYTE ocamlfind ocamlc"
mk_cmxa="$DISABLE_NTVE ocamlfind ocamlopt"

mls=p1_core.ml #FIXME was `ocamldep -sort -one-line *.ml`
cmos="${mls//.ml/.cmo}"
cmxs="${mls//.ml/.cmx}"

natives="p1_examples.native"

branch=`git symbolic-ref --short HEAD` 
v=`date +'%F'`
if [ "$branch" = "master" ]; then
    package_name="${libname}"
else 
    package_name="${libname}_${branch}"
fi


function mk_meta() {
cat >META <<EOF
name="$package_name"
description="$description"
version="$v"
requires="$required_packages"
archive(byte)="$libname.cma"
archive(native)="$libname.cmxa"
EOF
}

