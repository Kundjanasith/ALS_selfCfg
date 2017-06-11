import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

tag_headers = ['user_id', 'movie_id', 'tag', 'timestamp']
tags = pd.read_csv('data/tags.dat', sep='::', engine='python', header=None, names=tag_headers)
tags.to_csv('data/tags.csv',index=False)

rating_headers = ['user_id', 'movie_id', 'rating', 'timestamp']
ratings = pd.read_csv('data/ratings.dat', sep='::', engine='python', header=None, names=rating_headers)
ratings.to_csv('data/ratings.csv',index=False)

movie_headers = ['movie_id', 'title', 'genres']
movies = pd.read_csv('data/movies.dat', sep='::', engine='python', header=None, names=movie_headers)
movies.to_csv('data/movies.csv',index=False)