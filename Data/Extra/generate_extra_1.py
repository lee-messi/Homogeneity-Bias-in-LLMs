import os, re, sys, openai
from tqdm import tqdm
import pandas as pd
import numpy as np

os.chdir('..')
parent_directory = os.getcwd()

# Enter your OpenAI API Key here
openai.api_key = ""

verbose = False
save = True

from tenacity import (
    retry,
    stop_after_attempt,
    wait_random_exponential,
)  # for exponential backoff

@retry(wait=wait_random_exponential(min=1, max=60), stop=stop_after_attempt(6))
def completion_with_backoff(**kwargs):
    return openai.ChatCompletion.create(**kwargs)

race_list = np.concatenate((
  np.repeat('African', 4), 
  np.repeat('African', 29),
  np.repeat('African', 2),
  np.repeat('Asian', 1),
  np.repeat('Asian', 1),
  np.repeat('White', 1),
  np.repeat('African', 3),
  np.repeat('African', 1),
  np.repeat('Asian', 4),
  np.repeat('Hispanic', 1),
  np.repeat('Hispanic', 1),
  np.repeat('White', 1),
  np.repeat('White', 1)
))

format_list = np.concatenate((
  np.repeat('funny story about', 4), 
  np.repeat('horror story about', 29),
  np.repeat('tragic story about', 2),
  np.repeat('funny story about', 1),
  np.repeat('horror story about', 1),
  np.repeat('horror story about', 1),
  np.repeat('funny story about', 3),
  np.repeat('horror story about', 1),
  np.repeat('funny story about', 4),
  np.repeat('funny story about', 1),
  np.repeat('tragic story about', 1),
  np.repeat('character description of', 1),
  np.repeat('horror story about', 1)
))

gender_list = np.concatenate((
  np.repeat('man', 38), 
  np.repeat('woman', 12)
))

response_column = []

for index in tqdm(range(len(race_list)), desc = 'Index', position = 0):

      # Generate a response 
      response = completion_with_backoff(
        model = "gpt-3.5-turbo",
        messages = [
          {"role": "system", "content": "You are a chatbot"},
          {"role": "user", "content": "Write a thirty-word {format} a(n) {race} American {gender}.".
           format(format = format_list[index], race = race_list[index], gender = gender_list[index])}
        ]
      )

      if verbose == True: 
        print([x.message.content for x in response.choices])

      # Append response code to list
      response_column.extend([x.message.content for x in response.choices])

if save == True:

  # Save lists as columns inside pandas dataframe 
  os.chdir(os.path.join(parent_directory, 'extra'))
  final_df = pd.DataFrame(list(zip(format_list, race_list, gender_list, response_column)), 
    columns=['format', 'race', 'gender', 'text'])
  
  final_df.to_csv('extra_text_1.csv', index = False)