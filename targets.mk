.PHONY: clean data lint train deploy

#################################################################################
# COMMANDS                                                                      #
#################################################################################


# Publish the module to Artifactory
publish_module: clean install
	# NOTE: There are several artifactory tags which publish-model adds that
	# this method currently does not.
	python setup.py \
		bdist_wheel \
		bdist_egg \
		sdist \
		upload -r homeaway

publish: clean
	pip install --upgrade publish-model
	publish-model build-deploy

# Install the model.
install: clean
	pip install -e . && \
	bash -c model_bootstrap.sh

# Run PyTests
test: clean install
	pytest --cov-config .coveragerc \
		--cov=time_series_multireg \
		--disable-warnings \
		tests/

# Create the conda environment.
env:
	conda env create -f $(ENV_FILE)
	conda activate $(CONDA_ENV) && pip install -e .

## Delete all compiled Python files
clean:
	find . -name "*.pyc" -exec rm {} \;
	rm -rf deploy

# Lint with Flake8
lint:
	pip install flake8
	flake8 time_series_multireg/
	flake8 tests/
