# -*- coding: utf-8 -*-
"""
Created on Mon Nov 19 12:35:42 2018

@author: saulin_a
"""

import hddm
import pandas as pd
import pickle
from kabuki.analyze import gelman_rubin

data = hddm.load_csv('hddm_data.csv', sep = ",")
data.columns = ['drop', 'subj_idx', 'rt', 'response', 'condition']

# edit data for hddm code
data.rt = data.rt/1000
data.subj_idx = data.subj_idx-1

# work around enabling proper saving of the models
def my_save(self, fname):
    import pickle
    with open(fname, 'wb') as f:
        pickle.dump(self, f)

hddm.HDDM.my_save = my_save

# This will tailor an individual hierarchical DDM around your dataset.
model = hddm.HDDM(data,  include='all', depends_on={'v': 'condition', 'z' : 'condition', 'a' : 'condition'})
# find a good starting point which helps with the convergence.
model.find_starting_values()
# start drawing samples and discarding 2000 as burn-in
model.sample(5000, burn=2000, dbname='traces.db', db='pickle')
hddm.HDDM.my_save(model, 'model_upload')
m_all_zva = pickle.load(open('model_upload', 'rb'))

# extract statistics
model_stats = pd.DataFrame(model.gen_stats())
model_stats.to_csv('hddm_model_stats.csv')

# check convergence
models = []
for i in range(5):
    m = hddm.HDDM(data, include='all', depends_on={'v': 'condition', 'z' : 'condition', 'a' : 'condition'})
    m.find_starting_values()
    m.sample(7000, burn=5000, dbname='traces.db', db='pickle')
    models.append(m)

gelman_rubin(models)
