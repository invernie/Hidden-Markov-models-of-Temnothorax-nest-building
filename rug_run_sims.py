import pickle  # use pickle because shelve: 1. splits db in three objects with different extensions in win10 and there are issues calling the file back and 2. is not portbale across OS
import numpy as np
import wall_funcs as w

# run sims
stones = 1000
S = 100
filepathrug = "pathToFile/results/out_rug_" + str(stones) +"stones"
outListrug = list()    
        
for s in range(S):
    outListrug.append(w.rugatulus(s, 0, 18, stones = stones))
    print("rugatulus sim is at:" + str(s))
    
filerug = open(filepathrug, 'wb')
pickle.dump(outListrug, filerug)
filerug.close()


# save stats for all stone availability conditions in a single csv file
import csv, pickle
import numpy as np
import matplotlib.pyplot as plt

loadpath = "pathToFile/results/out_rug_1000stones"
filer1000 = open(loadpath, 'rb')
allResr1000 = pickle.load(filer1000)
filer1000.close()
CoVList_r1000 = [sum(f[4][-fsteps:])/fsteps for f in allResr1000]
aveCoV_r1000 = np.mean(CoVList_r1000)
stdCoV_r1000 = np.std(CoVList_r1000)
RbarList_r1000 = [sum(f[5][-fsteps:])/fsteps for f in allResr1000]
aveRbar_r1000 = np.mean(RbarList_r1000)
stdRbar_r1000 = np.std(RbarList_r1000)
aveFt_r1000 = np.mean([f[0] for f in allResr1000])
stdFt_r1000 = np.std([f[0] for f in allResr1000])
averbar_r1000 = np.mean([f[1] for f in allResr1000])
stdrbar_r1000 = np.std([f[1] for f in allResr1000])
minrbar_r1000 = min([f[1] for f in allResr1000])
maxrbar_r1000 = max([f[1] for f in allResr1000])
imr1000 = allResr1000[99][6]

loadpath = "pathToFile/results/out_rug_3000stones"
filer3000 = open(loadpath, 'rb')
allResr3000 = pickle.load(filer3000)
filer3000.close()
CoVList_r3000 = [sum(f[4][-fsteps:])/fsteps for f in allResr3000]
aveCoV_r3000 = np.mean(CoVList_r3000)
stdCoV_r3000 = np.std(CoVList_r3000)
RbarList_r3000 = [sum(f[5][-fsteps:])/fsteps for f in allResr3000]
aveRbar_r3000 = np.mean(RbarList_r3000)
stdRbar_r3000 = np.std(RbarList_r3000)
aveFt_r3000 = np.mean([f[0] for f in allResr3000])
stdFt_r3000 = np.std([f[0] for f in allResr3000])
averbar_r3000 = np.mean([f[1] for f in allResr3000])
stdrbar_r3000 = np.std([f[1] for f in allResr3000])
minrbar_r3000 = min([f[1] for f in allResr3000])
maxrbar_r3000 = max([f[1] for f in allResr3000])
imr3000 = allResr3000[99][6]

savepath = "pathToFile/results/stone_n_results_with_rug.csv"
with open(savepath, mode = "w", newline='') as stat_file:
    stat_writer = csv.writer(stat_file, delimiter = ',', quotechar = '"', quoting = csv.QUOTE_MINIMAL)
    
    stat_writer.writerow(["SO_r", "1000", aveCoV_r1000, stdCoV_r1000, aveRbar_r1000, stdRbar_r1000, aveFt_r1000, stdFt_r1000, averbar_r1000, stdrbar_r1000])
    stat_writer.writerow(["SO_r", "3000", aveCoV_r3000, stdCoV_r3000, aveRbar_r3000, stdRbar_r3000, aveFt_r3000, stdFt_r3000, averbar_r3000, stdrbar_r3000])


# wall image
img = imr1000
im = plt.imshow(img, cmap = 'gray')
plt.colorbar()
plt.axis('off')
plt.savefig("pathToFile/results/wallr_1000stones_T5000.eps", format = 'eps', dpi = 350)

