# -*- coding: utf-8 -*-
"""
Created on Wed Apr 28 22:26:58 2021

@author: saulin_a
"""

import hddm
import pandas as pd
import pickle
from kabuki.analyze import gelman_rubin

data = hddm.load_csv('data_for_hddm.csv', sep = ",")
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
model= hddm.models.HDDMRegressor(data, 'v ~ other_poss_loss',
                                  depends_on={'v': 'condition', 'z': 'condition', 'a': 'condition'},
                                  bias=True, include=['z', 'st', 'sz', 'sv'], p_outlier=0.05)
# find a good starting point which helps with the convergence.
model.find_starting_values()
# start drawing samples and discarding 2000 as burn-in
model.sample(5000, burn=2000, dbname='traces.db', db='pickle')
hddm.HDDM.my_save(model, 'my_model')
m_all_vza = pickle.load(open('my_model', 'rb'))

# extract statistics
model_stats = pd.DataFrame(model.gen_stats())
model_stats.to_csv('hddm_model_stats.csv')

# check convergence
models = []
for i in range(5):
    m = hddm.models.HDDMRegressor(data, 'v ~ other_poss_loss',
                                  depends_on={'v': 'condition', 'z': 'condition', 'a': 'condition'},
                                  bias=True, include=['z', 'st', 'sz', 'sv'], p_outlier=0.05)
    m.find_starting_values()
    m.sample(7000, burn=5000, dbname='traces.db', db='pickle')
    models.append(m)
gelman_rubin(models)   

# retrieve plausible t-values by repeatedly sampling from posterior distribution
# pick some new samples to calculate with these plausible values
new_sample_n = 5000
new_sample_burn = 10
new_samples = m_all_vza.sample(new_sample_n, burn=new_sample_burn)

# new_samples = m_all_vza
# for every sample create a data frame
# and compare the conditions, save the t values (example: z-parameter)
t_values_z_mm_con = []
t_values_z_mm_emp = []
t_values_z_mm_rec = []
for s in range(0, new_sample_n - new_sample_burn):
    this_z_sample_df = pd.DataFrame()
    for cond in data.condition.unique():
        this_cond_z_samples = []
        for subj in data.subj_idx.unique():
            z_string = 'z_subj(' + cond + ').' + str(subj)
            this_cond_z_samples.append(new_samples.trace(z_string)[s])
        this_z_sample_df[cond] = this_cond_z_samples
    t_values_z_mm_con.append(stats.ttest_rel(this_z_sample_df['multi-motive'], this_z_sample_df['control']).statistic)
    t_values_z_mm_emp.append(stats.ttest_rel(this_z_sample_df['multi-motive'], this_z_sample_df['empathy']).statistic)
    t_values_z_mm_rec.append(stats.ttest_rel(this_z_sample_df['multi-motive'], this_z_sample_df['reciprocity']).statistic)

# critical t value for two sided test with alpha = 0.05 and df N-1
crit_t = stats.t.ppf(1 - 0.025, len(data.subj_idx.unique()) - 1)

print "T_z(Multimotive unequal Reciprocity) = ", np.mean(t_values_z_mm_rec)
print "T_z(Multimotive unequal Empathy) = ", np.mean(t_values_z_mm_emp)
print "T_z(Multimotive unequal Baseline) = ", np.mean(t_values_z_mm_con)


