from SALib.sample import saltelli
from SALib.analyze import sobol
import numpy as np
import pickle

import wall_funcs

problem = {
    'num_vars': 7,
    'names': ['Pmax','Dmax','Fmax','Gmax', 'Fmin', 'Gmin', 'tau'],
    'bounds': [[0.01,1],
               [0.01,1],
               [0.11,1],
               [0.11,1],
               [0.001,0.1],
               [0.001,0.1],
               [0.01,0.03]]
    }

sampleSize = 1000
par_val = saltelli.sample(problem,sampleSize)

n_runs = par_val.shape[0]
Y_CoV = np.zeros([n_runs], dtype = 'float')
Y_Rbar = np.zeros([n_runs], dtype = 'float')
outList = list()

for i, X in enumerate(par_val):
     out = wall_funcs.rugatulus(seed = 0, q = 0, R2 = 18, Pmax = X[0], Dmax = X[1], Fmax = X[2], Gmax = X[3], Fmin = X[4], Gmin = X[5], tau = X[6])
     Y_CoV[i] = sum(out[4])/len(out[4])
     Y_Rbar[i] = sum(out[5])/len(out[5])
     outList.append(out)
     print(i)

Si_CoV = sobol.analyze(problem,Y_CoV, print_to_console = True)
Si_Rbar = sobol.analyze(problem,Y_Rbar, print_to_console = True)

SA_out = {
    'problem': problem,
    'par_val': par_val,
    'Y_CoV': Y_CoV,
    'Y_Rbar': Y_Rbar,
    'Si_CoV': Si_CoV,
    'Si_Rbar': Si_Rbar,
    'outList': outList
    }

picklePath = "path/To/File/SA_res_rug_3000stones_T5000"

# Save as a pickle
filePickle = open(picklePath, 'wb')
pickle.dump(SA_out, filePickle)  # each pickled file only contains one object (i.e. if multiple objects need to be saved, they need to be structured as subelements of a bigger objects by the user)
filePickle.close()

# to reload
pickleOpen = open(picklePath, 'rb')
SA_load = pickle.load(pickleOpen)
pickleOpen.close()

problem = SA_load["problem"]
Y_CoV = SA_load["Y_CoV"]
Y_Rbar = SA_load["Y_Rbar"]

# save par values and statistics to csv for R plotting
parValList = list()
for x in range(n_runs):
    for y in par_val[x]:
        parValList.append(y)
parArray = np.array(parValList)
parArray = np.reshape(parArray, (n_runs,7))
YCoV = [[x] for x in Y_CoV]
YRbar = [[x] for x in Y_Rbar]
cmbArray = np.hstack((parArray,YCoV,YRbar))

np.savetxt("Path/To/File/rug - pars and statistics.csv", cmbArray, delimiter = ',', header = 'Pmax, Dmax,Fmax,Gmax, Fmin, Gmin, tau, CoV, Rbar')
