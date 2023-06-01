# Main-Data

This repository contains scripts and tests for loading and processing data.

## Directory Structure

- `.github/workflows`: Contains the GitHub Actions workflow files. These are used to automate tasks such as running tests and updating data.
- `scripts`: Contains Python scripts for loading and processing data.
- `test`: Contains Python unit tests for the scripts in the `scripts` directory.
- `requirements.txt`: Lists the Python dependencies required for the scripts in this repository.
- `README.md`: This file.

## Scripts

The `scripts` directory contains the following scripts:

- `load_data.py`: This script loads data from a specified URL and saves it to a local file.
- `derived_data.py`: This script processes the raw data loaded by `load_data.py`.

## Tests

The `test` directory contains unit tests for the scripts in the `scripts` directory. These tests can be run using the Python `unittest` module.

## GitHub Actions

The `.github/workflows` directory contains a GitHub Actions workflow that automates the process of updating data. This workflow runs on a schedule and can also be triggered manually.

## Getting Started

To run the scripts locally, you will need to have Python installed on your machine. You can install the required dependencies with:

```bash
pip install -r requirements.txt
```

You can then run the scripts with:

```bash
python scripts/load_data.py
python scripts/derived_data.py
```

You can run the tests with:

```bash
python -m unittest test/test_raw_data.py
python -m unittest test/test_derived_data.py
```


