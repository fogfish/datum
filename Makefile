##  
##
## targets:
##    make all [APP=...]
##    make rel [APP=...] [config=vars.config]
##    make pkg [APP=...] [config=vars.config]
##    make run
##    make test
##
.PHONY: test rel deps all pkg

## application name
ROOT = `pwd`
PREFIX ?= /usr/local
APP ?= $(notdir $(CURDIR))
ARCH = $(shell uname -m)
PLAT = $(shell uname -s)
TAG  = ${ARCH}.${PLAT}
TEST?= priv/${APP}.benchmark
S3   =

## root path to benchmark framework
BB     = ../../../github/basho_bench
SSHENV = /tmp/ssh-agent.conf
 
## erlang flags
EFLAGS = \
	-name ${APP}@127.0.0.1 \
	-setcookie nocookie \
	-pa ./ebin \
	-pa deps/*/ebin \
	-pa apps/*/ebin \
	-kernel inet_dist_listen_min 32100 \
	-kernel inet_dist_listen_max 32199 \
	+P 1000000 \
	+K true +A 160 -sbt ts

## application release
ifeq ($(wildcard rel/reltool.config),) 
	REL =
	VSN =
	TAR =
	PKG =
else
	REL  = $(shell cat rel/reltool.config | sed -n 's/{target_dir,.*\"\(.*\)\"}./\1/p')
	VSN  = $(shell echo ${REL} | sed -n 's/.*-\(.*\)/\1/p')
ifeq (${VSN},)
	VSN  = $(shell cat rel/reltool.config | sed -n 's/.*{rel,.*\".*\",.*\"\(.*\)\".*/\1/p')
endif
ifeq (${config},)
	RFLAGS  =	
	VARIANT =
else
	VARIANT = $(addprefix ., $(notdir $(basename ${config})))
	RFLAGS  = target_dir=${REL}${VARIANT} overlay_vars=${ROOT}/${config}
endif
	TAR = ${REL}${VARIANT}.${TAG}.tgz
	PKG = ${REL}${VARIANT}.${TAG}.bundle
endif

## self-extracting bundle wrapper
BUNDLE_INIT = PREFIX=${PREFIX}\nREL=${PREFIX}/${REL}${VARIANT}\nAPP=${APP}\nVSN=${VSN}\nLINE=\`grep -a -n 'BUNDLE:\x24' \x240\`\ntail -n +\x24(( \x24{LINE\x25\x25:*} + 1)) \x240 | gzip -vdc - | tar -C ${PREFIX} -xvf - > /dev/null\n
BUNDLE_FREE = exit\nBUNDLE:\n

##
## build
##
all: rebar deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean ; \
	rm -rf test.*-temp-data ; \
	rm -rf tests ; \
	rm -rf log ; \
	rm -f  *.${TAG}.tgz ; \
	rm -f  *.${TAG}.bundle


distclean: clean 
	@./rebar delete-deps

test: all
	@./rebar skip_deps=true eunit

docs:
	@./rebar skip_deps=true doc

##
## release
##
ifneq (${REL},)
rel: 
	@./rebar generate ${RFLAGS}; \
	cd rel ; tar -zcf ../${TAR} ${REL}${VARIANT}/; cd -

pkg: rel/deploy.sh rel
	@printf "${BUNDLE_INIT}"  > ${PKG} ; \
	cat  rel/deploy.sh       >> ${PKG} ; \
	printf  "${BUNDLE_FREE}" >> ${PKG} ; \
	cat  ${TAR}              >> ${PKG} ; \
	chmod ugo+x  ${PKG}

${PKG}: pkg

s3: ${PKG}
	aws s3 cp ${PKG} ${S3}/${APP}-latest${VARIANT}.${TAG}.bundle

endif

##
## deploy
##
ifneq (${host},)
${SSHENV}:
	ssh-agent -s > ${SSHENV}

node: ${SSHENV}
	. ${SSHENV} ;\
	k=`basename ${pass}` ;\
	l=`ssh-add -l | grep $$k` ;\
	if [ -z "$$l" ] ; then \
		ssh-add ${pass} ;\
	fi ;\
	rsync -cav --rsh=ssh --progress ${PKG} ${host}:${PKG} ;\
	ssh -t ${host} "sudo sh ./${PKG}"

endif

##
## debug
##
run:
	@erl ${EFLAGS}

benchmark:
	$(BB)/basho_bench -N bb@127.0.0.1 -C nocookie ${TEST}
	$(BB)/priv/summary.r -i tests/current
	open tests/current/summary.png

ifneq (${REL},)
start: 
	@./rel/${REL}${VARIANT}/bin/${APP} start

stop:
	@./rel/${REL}${VARIANT}/bin/${APP} stop

console: 
	@./rel/${REL}${VARIANT}/bin/${APP} console

attach:
	@./rel/${REL}${VARIANT}/bin/${APP} attach
endif

##
## dependencies
##
rebar:
	@curl -O https://raw.githubusercontent.com/wiki/basho/rebar/rebar ; \
	chmod ugo+x rebar

