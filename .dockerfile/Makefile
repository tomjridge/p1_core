NAME:=p1_core

all: 
	$(MAKE) using_opam
# 	$(MAKE) local
# 	$(MAKE) from_github

# local:
# 	rm -rf p1_core
# 	mkdir p1_core
# 	rsync --exclude='.dockerfile/*' --exclude='.git/*' -vaz ../ ./p1_core
# 	docker build -t $(NAME)_local -f Dockerfile .
# 
# from_github:
# 	docker build -t $(NAME)_from_github -f Dockerfile.from_github .
# 

using_opam:
	docker build -t $(NAME)_using_opam -f Dockerfile .

# 
# #	docker run -ti $NAME bash

clean:
	-docker image rm $(NAME)_local $(NAME)_from_github $(NAME)_using_opam
	rm -rf p1_core
