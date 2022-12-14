### Base code for the new analysis that uses both occupancy and individual records to estimate
## an average population size across sites


npath = 'C:/Users/anfac/Dropbox/SmallMammal/A_analysis/new/'
main = pd.read_csv(npath+'site_stats.csv') #this is the main data set holding site attributes
names = main.Plot_Name.tolist()

### These next 2 data sets contain the detection probabilities for neotoma and peromyscus
## for neighborhoodsa and years. These distributions will be used to estimate population size

idr = individual detection rates

neo = pd.read_csv(npath+'neo_occ.csv')
neo['NBR']=neo.apply(lambda x: create_neigh(x['sites']),axis=1)
pero = pd.read_csv(npath+'pero_occ.csv')
pero['NBR']=pero.apply(lambda x: create_neigh(x['sites']),axis=1)

p_idr = pd.read_csv(npath+'pero_det.csv')
n_idr = pd.read_csv(npath+'neo_det.csv')

est = pd.read_csv(npath+'sm_work.csv')
est['CutClass'] = est.apply(lambda x: class_age(x.YearsCut),axis=1)



#generate the distribution for the final comparisons for peromyscus
# the outputs from these have been put back into the file sm_work.csv
reps = 1000
estimate =[]

for r in range(0,reps):
    
    group = []
    for n in names:
        site = main[main['Plot_Name']==n]
        mna = site['Pero_MNA'].tolist()[0]
        lo_occ = site['pocc_lo'].tolist()[0]
        hi_occ = site['pocc_hi'].tolist()[0]
        occ_prob = np.random.uniform(lo_occ,hi_occ,1)
        true_occup = np.random.binomial(1,occ_prob,1)
        
        if true_occup==0 and mna==0:
            group.append(0) 
        elif true_occup >0 or mna>0:
            if mna==0:
                mna=1
            
            nbr = site['Neighborhood'].tolist()[0] #set the neighborhood
            year = site['Year'].tolist()[0] #set the year
            
            det_data = p_idr[(p_idr['nbr']==nbr)&(p_idr['year']==year)]
            low = det_data['low'].tolist()[0]
            hi = det_data['hi'].tolist()[0]
            
            #this is a saftey that pulls the detection rate up to the mean
            # detection rate distribution across all sites for peromyscus
            if low < 0.1:
                low = 0.108
                hi = 0.14225
                        
            rand_det = np.random.uniform(low,hi,1)
            est = mna/rand_det[0]
            group.append(est)
    estimate.append(group)
            
gg = np.mean(estimate,axis=0) 
hh= np.std(estimate,axis=0)




#generate the distribution for the final comparisons neotoma 

reps = 1000
estimate =[]

for r in range(0,reps):
    
    group = []
    for n in names:
        site = main[main['Plot_Name']==n]
        mna = site['Neo_MNA'].tolist()[0]
        lo_occ = site['nocc_lo'].tolist()[0]
        hi_occ = site['nocc_hi'].tolist()[0]
        occ_prob = np.random.uniform(lo_occ,hi_occ,1)
        true_occup = np.random.binomial(1,occ_prob,1)
        
        if true_occup==0 and mna==0:
            group.append(0) 
        elif true_occup >0 or mna>0:
            if mna==0:
                mna=1
            
            nbr = site['Neighborhood'].tolist()[0] #set the neighborhood
            year = site['Year'].tolist()[0] #set the year
            
            det_data = n_idr[(n_idr['nbr']==nbr)&(n_idr['year']==year)]
            if len(det_data)==0:
                low = 0.159978
                hi = 0.2330724
            elif len(det_data)>0:
                low = det_data['low'].tolist()[0]
                hi = det_data['hi'].tolist()[0]
           
            #this is a saftey that pulls the detection rate up to the mean
            # detection rate distribution across all sites
            if low < 0.01:
                low = 0.159978
                hi = 0.2330724
                        
            rand_det = np.random.uniform(low,hi,1)
            est = mna/rand_det[0]
            group.append(est)
    estimate.append(group)
            
zz = np.mean(estimate,axis=0) 
yy = np.std(estimate,axis=0)
