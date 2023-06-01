import os
import sys
import unittest
import pandas as pd
from unittest.mock import patch, mock_open

# Get the absolute path to the directory containing derived_data.py
derived_data_directory = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__)))), 'test')
sys.path.insert(0, derived_data_directory)

from derived_data import clean_data


class TestCleanData(unittest.TestCase):
    @patch('pandas.read_csv')
    @patch('glob.glob')
    @patch('os.makedirs')
    def test_clean_data_success(self, mock_makedirs, mock_glob, mock_read_csv):
        # Setup
        directory = "data/raw"
        output_directory = "data/derived"
        filename_pattern = f'{directory}/*.csv'
        mock_glob.return_value = ['test.csv']  # Assume a CSV file exists
        mock_makedirs.return_value = None  # No return value for os.makedirs

        # Create a mock DataFrame for pandas.read_csv
        data = {
            'Document Type': ['Journals', 'Letters', None, 'Daybooks', 'Discourses'],
            'Text Only Transcript': ['Text 1', 'Text 2', 'Text 3', 'Text 4', 'Text 5']
        }
        df = pd.DataFrame(data)
        mock_read_csv.return_value = df

        # Call function
        with patch("pandas.DataFrame.to_csv", mock_open()) as mock_to_csv:
            clean_data(directory, output_directory)

        # Asserts
        mock_read_csv.assert_called_once_with('test.csv')  # The function should have read the raw data file
        self.assertEqual(mock_read_csv().shape[0], 5)  # The new DataFrame should have 5 rows (since no rows are dropped)
        self.assertTrue('Text Only Transcript' in mock_read_csv().filter(items=['Text Only Transcript']).columns)
        mock_to_csv.assert_called_once_with(os.path.join(output_directory, 'derived_data.csv'), index=False)


    @patch('os.path.isfile')
    def test_clean_data_error(self, mock_isfile):
        # Setup
        directory = "data/raw"
        output_directory = "data/derived"
        mock_isfile.return_value = False  # Assume the raw data file does not exist

        # Call function
        with self.assertRaises(SystemExit) as cm:
            clean_data(directory, output_directory)

        # Asserts
        self.assertEqual(cm.exception.code, 1)  # Exit code for failure


if __name__ == '__main__':
    unittest.main()
