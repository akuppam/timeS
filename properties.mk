BUILD_AGENT=dl2.homeawaycorp.com/datascience/datascience-build-agent:1

PROJECT_NAME ?= time-series-multireg
ENV_FILE ?= environment.yml
CONDA_ENV ?= $(PROJECT_NAME)