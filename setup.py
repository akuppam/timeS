from setuptools import setup, find_packages
import versioneer

setup(
    name="time-series-multireg",
    version=versioneer.get_version(),
    cmdclass=versioneer.get_cmdclass(),
    install_requires=[
        "pandas==0.23.4",
        "scikit-learn==0.20.0",
        "loguru==0.2.4",
        "PypeR==1.1.2",
        "joblib==0.13.2",
        "toolz==0.9.0"
    ],
    pacakges=find_packages(
        exclude=[
            "tests",
            "notebooks",
            "Flowcharts"
        ]
    ),
    long_description=open("README.md").read(),
    setup_requires=["pytest-runner"],
    test_requires=["pytest"],
    author="Aaron Hellman, Arun Kuppam, Tim Renner",
    author_email="ahellman@homeaway.com",
    include_package_data=True,
    description="Multi regressor model selection for time series data.",
    scripts=["model_bootstrap.sh"]
    # NOTE: No sage entry point needed yet.
)
