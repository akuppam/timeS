##################################################
# HomeAway Makefile - http://h.a/makefile        #
##################################################

include properties.mk

##################################################
# Makefile Targets                               #
##################################################
ifdef IN_BUILD_AGENT

#####
# Required Targets
#####
.PHONY: clean
.PHONY: build
.PHONY: test
.PHONY: install
.PHONY: publish
.PHONY: print-gav
.PHONY: prepare-agent

#####
# Optional Targets
#####
.PHONY: run
.PHONY: start
.PHONY: stop

include targets.mk

##################################################
# Build Agent Bootstrapping                      #
#                                                #
# Do not edit below this line                    #
##################################################
else

FIRST_GOAL=$(shell echo "$(MAKECMDGOALS)" | awk '{print $$1}')
EXTERNAL_DOCKER_FLAGS=
AGENT_COMMAND=make -e

ifndef ENABLE_USER_MAPPING
ENABLE_USER_MAPPING=true
endif

ALLOCATE_TTY=-t
ifdef CI
ALLOCATE_TTY=
endif

IS_MAC=$(shell uname | grep -q Darwin && echo "true")
ifeq ($(ENABLE_USER_MAPPING),true)
ifeq ($(IS_MAC),true)
AGENT_COMMAND_PREFIX := cu root 1000 50 $(AGENT_COMMAND_PREFIX)
else
AGENT_COMMAND_PREFIX := cu root $(shell id -u) $(shell id -g) $(AGENT_COMMAND_PREFIX)
endif
endif

AGENT_RUNNING=$(shell docker inspect --format="{{ .State.Running }} " $(BUILD_AGENT_NAME) 2> /dev/null)
AGENT_EXISTS=$(shell docker ps -a | grep -e "\b$(BUILD_AGENT_NAME)\b" | awk '{print $$1}' )

foo:
	echo foo
.PHONY: raw
raw:
	@IN_BUILD_AGENT=true $(MAKE) -e $(filter-out $@,$(MAKECMDGOALS))

agent-running:
	@if [ "x$(AGENT_RUNNING)" != "xtrue" ]  && [ "x$(FIRST_GOAL)" != "xagent-stop" ] && [ "x$(FIRST_GOAL)" != "xraw" ]; then \
		if [ -n "$(AGENT_EXISTS)" ]; then \
			echo "docker rm -f $(BUILD_AGENT_NAME)"; docker rm -f $(BUILD_AGENT_NAME); \
		fi; \
		echo "Build agent [$(BUILD_AGENT_NAME)] was not running; starting & preparing it."; \
		docker pull $(BUILD_AGENT); \
		docker run --name=$(BUILD_AGENT_NAME) \
			-d -i -t \
			-e IN_BUILD_AGENT=true \
			-v "$(CURDIR)/":"$(BUILD_AGENT_WORKDIR)" \
			-v /var/run/docker.sock:/var/run/docker.sock \
			-w "$(BUILD_AGENT_WORKDIR)" \
			$(DOCKER_FLAGS) \
			$(EXTERNAL_DOCKER_FLAGS) \
			$(BUILD_AGENT) \
			agent-as-service $(BUILD_AGENT_WORKDIR) $(BUILD_AGENT_TIMEOUT) $(BUILD_AGENT_NAME) \
			$(AGENT_COMMAND_PREFIX) sh || exit 1; \
	fi

.PHONY: agent-stop
agent-stop:
	@if [ -n "$(AGENT_EXISTS)" ]; then \
		echo "docker rm -f $(BUILD_AGENT_NAME)"; docker rm -f $(BUILD_AGENT_NAME); \
	fi

.PHONY: agent
agent: agent-running
	@AGENT_COMMAND="" $(MAKE) -e $(filter-out $@,$(MAKECMDGOALS))

%: agent-running
ifneq ($(IN_BUILD_AGENT),true)
ifeq ($(FIRST_GOAL),raw)
		@:
else
	@if [ "x$(FIRST_GOAL)" = "x$@" ]; then \
		echo docker exec -i $(ALLOCATE_TTY) \
			$(BUILD_AGENT_NAME) \
			$(AGENT_COMMAND_PREFIX) \
			$(AGENT_COMMAND) \
			$(filter-out raw,$(MAKECMDGOALS)); \
		docker exec -i $(ALLOCATE_TTY) \
			$(BUILD_AGENT_NAME) \
			$(AGENT_COMMAND_PREFIX) \
			$(AGENT_COMMAND) \
                        PATH=/opt/conda/bin:\$(PATH) \
			$(filter-out raw,$(MAKECMDGOALS)); \
	fi
endif
endif
endif

GITREMOTE=$(shell git config --get remote.origin.url )
GITREPO=$(shell echo $(GITREMOTE) | awk -F '/' '{print $$NF}' | cut -d. -f1 )
GITORG=$(shell echo $(GITREMOTE) | awk -F ':' '{print $$NF}' | cut -d/ -f1 )
BUILD_AGENT_NAME=build-agent-$(GITREPO)

ifeq ($(GITREPO),)
BUILD_AGENT_NAME=build-agent-$(notdir $(CURDIR:%/=%))
endif

ifndef BUILD_AGENT_TIMEOUT
BUILD_AGENT_TIMEOUT=28800
endif

ifndef BUILD_AGENT_WORKDIR
BUILD_AGENT_WORKDIR=/work
endif


