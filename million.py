import os
import sys
import time
import glob
import datetime
import sqlite3
import numpy as np 

msd_subset_path='/Users/kjzth/Desktop/novie_len_als/MillionSongSubset'
msd_subset_data_path=os.path.join(msd_subset_path,'data')
msd_subset_addf_path=os.path.join(msd_subset_path,'AdditionalFiles')
assert os.path.isdir(msd_subset_path),'wrong path' 

msd_code_path='/Users/kjzth/Desktop/novie_len_als/MSongsDB'
assert os.path.isdir(msd_code_path),'wrong path'

sys.path.append( os.path.join(msd_code_path,'PythonSrc') )

import hdf5_getters as GETTERS

def strtimedelta(starttime,stoptime):
    return str(datetime.timedelta(seconds=stoptime-starttime))

def apply_to_all_files(basedir,func=lambda x: x,ext='.h5'):
    cnt = 0
    for root, dirs, files in os.walk(basedir):
        files = glob.glob(os.path.join(root,'*'+ext))
        cnt += len(files)
        for f in files :
            func(f)       
    return cnt

print('number of song files:',apply_to_all_files(msd_subset_data_path))

all_artist_names = set()

def func_to_get_artist_name(filename):
    h5 = GETTERS.open_h5_file_read(filename)
    artist_name = GETTERS.get_artist_name(h5)
    all_artist_names.add( artist_name )
    h5.close()
    
t1 = time.time()
apply_to_all_files(msd_subset_data_path,func=func_to_get_artist_name)
t2 = time.time()
print('all artist names extracted in:',strtimedelta(t1,t2))

print('found',len(all_artist_names),'unique artist names')
for k in range(5):
    print(list(all_artist_names)[k])

conn = sqlite3.connect(os.path.join(msd_subset_addf_path,'subset_track_metadata.db'))

q = "SELECT DISTINCT artist_name FROM songs"
t1 = time.time()
res = conn.execute(q)
all_artist_names_sqlite = res.fetchall()
t2 = time.time()
print('all artist names extracted (SQLite) in:',strtimedelta(t1,t2))

conn.close()

for k in range(5):
    print(all_artist_names_sqlite[k][0])

conn = sqlite3.connect(os.path.join(msd_subset_addf_path, 'subset_track_metadata.db'))
q = "SELECT DISTINCT artist_id FROM songs"
res = conn.execute(q)
all_artist_ids = map(lambda x: x[0], res.fetchall())
conn.close()

for k in range(4):
    print(all_artist_ids[k])

files_per_artist = {}
for aid in all_artist_ids:
    files_per_artist[aid] = 0

def func_to_count_artist_id(filename):
    h5 = GETTERS.open_h5_file_read(filename)
    artist_id = GETTERS.get_artist_id(h5)
    files_per_artist[artist_id] += 1
    h5.close()

apply_to_all_files(msd_subset_data_path,func=func_to_count_artist_id)

most_pop_aid = sorted(files_per_artist,
                      key=files_per_artist.__getitem__,
                      reverse=True)[0]
print(most_pop_aid,'has',files_per_artist[most_pop_aid],'songs.')

conn = sqlite3.connect(os.path.join(msd_subset_addf_path,'subset_track_metadata.db'))
q = "SELECT DISTINCT artist_name FROM songs"
q += " WHERE artist_id='"+most_pop_aid+"'"
res = conn.execute(q)
pop_artist_names = map(lambda x: x[0], res.fetchall())
conn.close()
print('SQL query:',q)
print('name(s) of the most popular artist:',pop_artist_names)

t1 = time.time()
conn = sqlite3.connect(os.path.join(msd_subset_addf_path,'subset_track_metadata.db'))
q = "SELECT DISTINCT artist_id,artist_name,Count(track_id) FROM songs"
q += " GROUP BY artist_id"
res = conn.execute(q)
pop_artists = res.fetchall()
conn.close()
t2 = time.time()
print('found most popular artist in',strtimedelta(t1,t2))
print(sorted(pop_artists,key=lambda x:x[2],reverse=True)[0])