import os
import glob
import pandas as pd
import nltk
from nltk.sentiment import SentimentIntensityAnalyzer

def add_sentiment_analysis(df):
    sid = SentimentIntensityAnalyzer()
    sentiment_scores = []

    for index, row in df.iterrows():
        transcript = row['Text Only Transcript']
        sentiment_score = sid.polarity_scores(transcript)
        sentiment_scores.append(sentiment_score['compound'])

    df['Sentiment'] = sentiment_scores

    return df

def clean_data(directory, output_directory):
    filename_pattern = os.path.join(directory, '*.csv')
    output_filename = 'derived_data.csv'

    # Find all CSV files in the directory
    csv_files = glob.glob(filename_pattern)

    # Check if any CSV file exists in the directory
    if not csv_files:
        print(f"Error: No CSV files found in directory {directory}")
        exit(1)

    # Select the first CSV file found
    filename = csv_files[0]

    # Load the raw data
    df_raw = pd.read_csv(filename)

    # Remove rows where 'Document Type' is NaN
    df_raw = df_raw[df_raw['Document Type'].notna()]

    # Filter the dataframe to only 'Text Only Transcript'
    df_derived = df_raw[df_raw['Text Only Transcript'].notna()]

    # Perform sentiment analysis and add sentiment scores to the dataframe
    df_derived = add_sentiment_analysis(df_derived)

    # Check if the output directory exists, if not, create it
    if not os.path.exists(output_directory):
        os.makedirs(output_directory)

    # Save the derived data
    df_derived.to_csv(os.path.join(output_directory, output_filename), index=False)


if __name__ == '__main__':
    raw_data_directory = "data/raw"  # Directory where the raw data is located
    derived_data_directory = "data/derived"  # Directory to save the cleaned data
    clean_data(raw_data_directory, derived_data_directory)
