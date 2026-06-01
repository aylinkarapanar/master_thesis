import pandas as pd

def eat_the_pickle(file):
    pickle_data = pd.read_pickle(file)
    return pickle_data
