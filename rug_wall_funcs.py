import math, copy, importlib  # importlib is for module reloading, if necessary
import numpy as np
import matplotlib.pyplot as plt

# save global functions into variables for speed
unif = np.random.uniform
randint = np.random.randint
ranchoice = np.random.choice
xp = math.exp

def rugatulus(seed,q,R2,R1 = 18, Pmax = 0.35, Fmin = 0.01, Fmax = 0.55, Dmax = 0.5, Gmin = 0.01, Gmax = 0.55, tau = 0.025, stones = 3000, T = 5000, tint = 100, printing = "off", delayedPopInc = "off", lateRopt = 24, recordStoneMovement = 'off', ind_var = 'off', var_sd = 0, corners = 'off', showIm = "off"):
    
    np.random.seed(seed)
    
    # save local functions into variables for speed
    sigma = 1
    def calcDr(rd,Ropt,Sd,Sc):
        Dr = (Dmax/(1+tau*(rd-Ropt)**2))*(Gmin+(Gmax-Gmin)*xp(-(Sc-Sd)**2/(2*sigma**2)))
        return(Dr)
    def calcPr(rp,Ropt,Sp,Sc):
        if Sp < Sc:
            F = Fmax
        else:
            F = Fmin
        Pr = Pmax*(1-(1/(1+tau*(rp-Ropt)**2)))*F
        return(Pr)
    
    
    #creates space and sets simulation variables
    N = 30    # n of workers
    a = 81    # 80 mm by 80 mm matrix; distance node-node = 1 mm
    
    # additional Franks and Deneubourg (1997) model parameters
    Qmax = 3  # max n of stones per node
    Sc = 6    # critical stone density
    
    # fitness function parameters
    beta = 1
    gamma = 1
    
    # record data at time intervals
    Ft_list = np.zeros(int(T/tint), dtype = np.float)
    rbar_list = np.zeros(int(T/tint), dtype = np.float)
    CoV_list = np.zeros(int(T/tint), dtype = np.float)
    Rbar_list = np.zeros(int(T/tint), dtype = np.float)
    if recordStoneMovement == 'on':
        space_mat_list = list()
    
    # space matrix and conversion to cartesian coordinates
    space_matrix = np.zeros([a,a], dtype = 'int8')
    r_mat = np.zeros([a,a], dtype = 'float64')
    cos_mat = np.zeros([a,a], dtype = 'float64')
    sin_mat = np.zeros([a,a], dtype = 'float64')
    for x in range(a):
        for y in range(a):
            ##calculate cartesian coordinates from matrix coordinates. Space matrix refers the position of nodes and not cells.
            xcart = x - 40
            ycart = 40 - y
            r = math.sqrt(xcart*xcart+ycart*ycart)
            r_mat[x,y] = r
            if r > 0:
                cos_mat[x,y] = xcart/r
                sin_mat[x,y] = ycart/r #at 0;0 , sine and cosine have default values 0 and 0 (trigonometrically impossible)
    
    #creates colony population and assigns alleles
    ant_mat = np.zeros([N], dtype = 'float64')
    if ind_var == 'off':
        qi = int(round(N*q))
        ant_mat [:qi] = R2
        ant_mat [qi:] = R1
    elif ind_var == 'on':
        # if ind_var on, we use the R1 parameter as the mean of the distribution from which we draw individual values
        ant_mat = np.random.normal(R1, var_sd, N)
        #ant_mat = np.random.uniform(15, 18, N)
    else:
        raise ValueError("Accepted values for individual variation: 'off'(default)/'on'")

    #pellet generation: 1000 stone items are generated at initialisation. The number of items does not change throughout the simulation.
    for _ in range(stones):
        assign = 0
        while (assign ==0):
            ax = randint(0,a)
            ay = randint(0,a)
            if space_matrix[ax,ay]<Qmax:
                space_matrix[ax,ay] += 1
                assign = 1
    if corners == 'on':
        space_matrix[0,0] = Qmax
        space_matrix[0,a-1] = Qmax
        space_matrix[a-1,0] = Qmax
        space_matrix[a-1,a-1] = Qmax
        xlist = range(1,a-1)
        ylist = range(1,a-1)
    elif corners == 'off':
        xlist = range(0,a)
        ylist = range(0,a)


    #SIMULATION
    t=0
    while t<T:
    
        np.random.shuffle(ant_mat)
        for n in range(N):
            #at each cycle, each ant has one chance to pick up a pellet; if it picks up, it then repeats its action until the pellet is deposited
            Ropt = ant_mat[n]
            if delayedPopInc == "on" and t>=5000:  # for sims where we check for the effect of a delayed increase in worker pop size
                Ropt = lateRopt
            pickedup = 0
            deposited = 0
            while not pickedup:
                px = ranchoice(xlist, 1)
                py = ranchoice(ylist, 1)
                if  space_matrix[px,py] > 0:
                    rp = r_mat[px,py]
                    if px == 80:
                        xpr = 0
                        xpl = space_matrix[px-1,py]
                    elif px == 0:
                        xpl = 0
                        xpr = space_matrix[px+1,py]
                    else:
                        xpr = space_matrix[px+1,py]
                        xpl = space_matrix[px-1,py]
                    if py == 80:
                        ypd = 0
                        ypu = space_matrix[px,py-1]
                    elif py == 0:
                        ypu = 0
                        ypd = space_matrix[px,py+1]
                    else:
                        ypu = space_matrix[px,py-1]
                        ypd = space_matrix[px,py+1] 
                    Sp = xpl+xpr+ypd+ypu
                    Pr = calcPr(rp, Ropt, Sp, Sc)
                    if unif(0,1)<=Pr:
                        space_matrix[px,py]+= -1
                        pickedup = 1
                    else:
                        pickedup = 2
                    
            if pickedup == 1:
                while not deposited:
                    dx = ranchoice(xlist, 1)
                    dy = ranchoice(xlist, 1)
                    Qd = space_matrix[dx,dy]
                    if Qd < Qmax:
                        rd = r_mat[dx,dy]
                        if dx == 80:
                            xdr = 0
                            xdl = space_matrix[dx-1,dy]
                        elif dx == 0:
                            xdl = 0
                            xdr = space_matrix[dx+1,dy]
                        else:
                            xdr = space_matrix[dx+1,dy]
                            xdl = space_matrix[dx-1,dy]
                        if dy == 80:
                            ydd = 0
                            ydu = space_matrix[dx,dy-1] 
                        elif dy == 0:
                            ydu = 0
                            ydd = space_matrix[dx,dy+1]
                        else:
                            ydd = space_matrix[dx,dy+1]
                            ydu = space_matrix[dx,dy-1] 
                        Sd = xdl+xdr+ydd+ydu+Qd
                        Dr = calcDr(rd, Ropt, Sd, Sc)
                        if unif(0,1)<=Dr:
                            space_matrix[dx,dy] += 1
                            deposited = 1
                        
        #calculate structure fitness at time step t
        ##calculate rbar
        stone_v = r_mat[space_matrix[:,:] > 0] # locations where stones are present; note: slice given as a list of extracted values
        rbar = sum(stone_v)/len(stone_v)       # ave radius of those locations
        ##calculate CoV
        CoV = np.std(stone_v)/rbar
        ##calculate mean resultant length/circular spread Rbar
        l = len(stone_v)
        cos_v = cos_mat[space_matrix[:,:] > 0]
        sin_v = sin_mat[space_matrix[:,:] > 0]
        Rbar = math.sqrt(sum(cos_v)**2+sum(sin_v)**2)/l

        Ft = math.exp(-beta*CoV)*math.exp(-gamma*Rbar)
    
        t += 1
    
        if t%tint == 0:
            fi = int(t/tint)
            Ft_list [fi-1] = Ft 
            rbar_list [fi-1] = rbar
            CoV_list [fi-1] = CoV
            Rbar_list [fi-1] = Rbar
            if recordStoneMovement == 'on':
                space_mat_list.append(copy.deepcopy(space_matrix))

        if printing == "on":
            print(t)

    #plot matrix
    if showIm == "on":
        plt.imshow(space_matrix, cmap = 'gray');
        plt.colorbar()
    #Final Ft as average Ft across the last 1000 rounds of simulation
    fsteps = 10
    Ft = sum(Ft_list[-fsteps:])/len(Ft_list[-fsteps:])
    rbar = sum(rbar_list[-fsteps:])/len(rbar_list[-fsteps:])
    
    if recordStoneMovement == 'on':
        outObj = [Ft, rbar, Ft_list,rbar_list,CoV_list,Rbar_list,space_mat_list]
    else:
        outObj = [Ft, rbar, Ft_list,rbar_list,CoV_list,Rbar_list,space_matrix]
        
    return(outObj)  # output will be added to a list and then saved as a binary file

