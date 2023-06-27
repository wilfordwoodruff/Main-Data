import os
import requests
import re
import argparse
import sys

def get_data(url, api_key, directory, name):
    headers = {
        "Authorization": f"Bearer 2|{api_key}"
    }

    response = requests.get(url, headers=headers)

    if response.status_code == 200:
        content_disposition = response.headers.get("Content-Disposition")
        if content_disposition:
            match = re.search(r'filename=(.+)', content_disposition)
            if match:
                filename = match.group(1)
            else:
                filename = f"{name}-export.csv"
        else:
            filename = f"{name}-export.csv"

        # Check if the directory exists, if not, create it
        if not os.path.exists(directory):
            os.makedirs(directory)

        # Write the contents of the file to a local file in the specified directory
        with open(os.path.join(directory, filename), "wb") as f:
            f.write(response.content)
    else:
        print(f"Error: {response.status_code} - {response.reason}")
        sys.exit(1)  # add this line

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--api_key', default=os.getenv('WOODRUFF_AUTHORIZATION_HEADER'), help='API key')
    args = parser.parse_args()

    api_key = args.api_key

    if not api_key:
        print("Error: No API key provided. Please set the API_KEY environment variable or use the --api_key argument.")
        exit(1)

    url_1 = 'https://wilfordwoodruffpapers.org/api/v1/pages/export'
    url_2 = 'https://wilfordwoodruffpapers.org/api/v1/places/export'
    directory = "data/raw"  # Directory to save the data

    get_data(url_1, api_key, directory, 'pages')
    get_data(url_2, api_key, directory, 'locations')
