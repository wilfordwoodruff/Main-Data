import os
import sys
import unittest
from unittest.mock import patch, mock_open
import requests

# Get the absolute path to the directory containing load_data.py
load_data_directory = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__)))), 'script')
sys.path.insert(0, load_data_directory)

from load_data import get_data


class TestGetData(unittest.TestCase):
    @patch('requests.get')
    def test_get_data_success(self, mock_get):
        # Setup
        api_key = 'test-api-key'
        url = 'https://wilfordwoodruffpapers.org/api/v1/pages/export'
        directory = "data/raw"
        filename = "pages-export.csv"
        mock_response = requests.Response()
        mock_response.status_code = 200
        mock_response.headers = {"Content-Disposition": f'filename={filename}'}
        mock_response._content = b'Test Content'  # raw content for the response
        mock_get.return_value = mock_response

        # Call function
        with patch("builtins.open", mock_open()) as mock_file:
            get_data(url, api_key, directory, 'pages')

        # Asserts
        mock_get.assert_called_once_with(url, headers={"Authorization": f"Bearer 2|{api_key}"})
        mock_file.assert_called_once_with(os.path.join(directory, filename), 'wb')

    @patch('requests.get')
    def test_get_data_error(self, mock_get):
        # Setup
        api_key = 'test-api-key'
        url = 'https://wilfordwoodruffpapers.org/api/v1/pages/export'
        directory = "data/raw"
        mock_response = requests.Response()
        mock_response.status_code = 404
        mock_response.reason = 'Not Found'
        mock_get.return_value = mock_response

        # Call function
        with self.assertRaises(SystemExit) as cm:
            get_data(url, api_key, directory,'pages')

        # Asserts
        mock_get.assert_called_once_with(url, headers={"Authorization": f"Bearer 2|{api_key}"})
        self.assertEqual(cm.exception.code, 1)  # Exit code for failure

if __name__ == '__main__':
    unittest.main()
