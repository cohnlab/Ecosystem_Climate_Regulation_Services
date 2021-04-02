# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'
# ### Computes growing season variables using a crop calendar and daily data, including EDDs and GDDs. 
# #### Repeats the process adding fixed temperature increases before computing

# %%
from matplotlib import pyplot as plt
import numpy as np
import pandas as pd
import xarray as xr
import warnings
import tqdm
import glob
import os
import re
import copy
import datetime
import numba
from numba import jit,prange

# ### Setup

# %%

# Folder with the daily input files (one per year)
infolder = "Input_Data/Xavier/"

# Folder with deltaT NetCDFs. These should be 2D only
dtinfolder = "Output/DT_B/"

outfolder = "Output/DEDD_B/"

# Actually a middle word
masterpref = ".daily."
# Actually prefixes
tempsuf = "temp"
tmaxsuf = "tmax"
tminsuf = "tmin"

calfolder = "Input_Data/Crop_calendar/"
calsuf = ".crop.calendar.fill.nc"
planvar = "plant.start"
harvvar = "harvest.end"

mskfname = infolder+"seamask.nc" # A land sea mask that should be with the original climate data

crops = ["Soybeans"]

gddlims = {"Soybeans":10.0, "Maize":10.0, "Cotton":15.0}
eddlims = {"Soybeans":30.0, "Maize":29.0, "Cotton":32.0}

# The basis of calculation will be harvest years
hyears = list(range(1995,2005+1))
print(hyears)

# ### Defining functions

# %%
# Opens and concatenates a harvest year with the equivalent planting year
# def concat_clim(infolder,climpref,hyear):
def concat_clim(infolder,masterpref,climsuf,hyear):
    pyear = hyear - 1

    climharr = xr.open_dataarray(infolder+climsuf+masterpref+str(hyear)+".nc", decode_times = False)
    climparr = xr.open_dataarray(infolder+climsuf+masterpref+str(pyear)+".nc", decode_times = False)

    climarr = xr.concat([climparr,climharr], dim = "time")
    return climarr


# %%
# Calculates AGDD for a single day (ascending numbers, constrained in both sides)
@jit(nopython=True)
def calc_agdd_day(Tmin,Tmax,Tlo,Thi):
    res = 0.005 #Resolution (dt, in days) on which to evaluate the T sine curve

    t = np.arange(0,1,res)
    nt = t.shape[0]

    Tamp = (Tmax-Tmin)/2.0
    Tmed = (Tmax+Tmin)/2.0

    T = Tmed + Tamp*np.sin(t*(2.0*np.pi/1))

    gdd = np.sum(np.invert(np.isnan(
        np.where((T>=Tlo)&(T<=Thi),T,np.nan)
    ))*(T-Tlo))/nt

    return gdd

@jit(nopython=True)
def calc_agdd_point(tmaxvec,tminvec,Tlo,Thi):
    gdd = 0.0
    for day in range(tminvec.shape[0]):
        gdd = gdd + calc_agdd_day(tminvec[day],tmaxvec[day],Tlo,Thi)
    return gdd


# %%
# Calculates CGDD for a single day (ascending numbers, constrained just below)
@jit(nopython=True)
def calc_cgdd_day(Tmin,Tmax,Tlo):
    res = 0.005 #Resolution (dt, in days) on which to evaluate the T sine curve

    t = np.arange(0,1,res)
    nt = t.shape[0]

    Tamp = (Tmax-Tmin)/2.0
    Tmed = (Tmax+Tmin)/2.0

    T = Tmed + Tamp*np.sin(t*(2.0*np.pi/1))

    gdd = np.sum(np.invert(np.isnan(
        np.where((T>=Tlo),T,np.nan)
    ))*(T-Tlo))/nt

    return gdd

@jit(nopython=True)
def calc_cgdd_point(tmaxvec,tminvec,Tlo):
    gdd = 0.0
    for day in range(tminvec.shape[0]):
        gdd = gdd + calc_cgdd_day(tminvec[day],tmaxvec[day],Tlo)
    return gdd


# %%
# Calculates everything for the growing season given numpy arrays. 
# Loops throught the points
# Compiles with Numba parallel
@jit(nopython=True, parallel = True)
# @jit(nopython=True)
def calc_all(planmat,harvmat,tempmat,tmaxmat,tminmat,
             gddmat,eddmat,
             gddlim,eddlim):
    for lati in prange(tempmat.shape[1]):
        for lonj in range(tempmat.shape[2]):
            if (np.isnan(planmat[lati,lonj])) or (np.isnan(tempmat[0,lati,lonj])):
                continue
            plan = int(planmat[lati,lonj])
            harv = int(harvmat[lati,lonj])

            tempvec = tempmat[plan:harv,lati,lonj]
            tmaxvec = tmaxmat[plan:harv,lati,lonj]
            tminvec = tminmat[plan:harv,lati,lonj]

            gddmat[lati,lonj] = calc_agdd_point(tmaxvec,tminvec,gddlim,eddlim)
            eddmat[lati,lonj] = calc_cgdd_point(tmaxvec,tminvec,eddlim)

#%% 
# Wraps calc_all with some preprocessing
# Returns a Dataset with the calculated DDs.
def calc_all_Wrap(temparr,tmaxarr,tminarr,mskarr,gddlim,eddlim):
    temparr = temparr.where(mskarr == 1)
    tmaxarr = tmaxarr.where(mskarr == 1)
    tminarr = tminarr.where(mskarr == 1)

    # Preallocate the arrays that will be filled
    lldims = ("latitude","longitude")
    coords = [(i,temparr.coords[i].data,temparr.coords[i].attrs) for i in lldims] # Tuples with lat and lon dimension specs

    # 2D arrays
    gddarr = xr.DataArray(coords = coords, name = "gdd")
    eddarr = xr.DataArray(coords = coords, name = "edd")

    # This basically creates pointers to the numpy arrays inside the xr.Dataarrays
    # We need those for numba to work. An alternative would be passing the .data in the function call
    planmat = planarr.data
    harvmat = harvarr.data

    tempmat = temparr.data
    tmaxmat = tmaxarr.data
    tminmat = tminarr.data

    gddmat = gddarr.data
    eddmat = eddarr.data

    # Calculates everything.
    calc_all(planmat,harvmat,tempmat,tmaxmat,tminmat,
                    gddmat,eddmat,
                    gddlim,eddlim)
    outds = xr.merge([gddarr,eddarr])
    return(outds)

# ### Begin main script

# %%
# Lists all deltaT files from dtfolder #FIXME: This should be inside the crop loop?
dtfnames = glob.glob(dtinfolder + "/*.nc")

# Create output folder
if not os.path.exists(outfolder): os.makedirs(outfolder, exist_ok=True)

# Read the mask
mskarr = xr.open_dataarray(mskfname)
    
# crop = crops[0]
for crop in crops:
    print(crop)

    # Open calendar and convert it to two-year based indexes. FIXME: Ignoring leap years here
    caldata = xr.open_dataset(calfolder+crop+calsuf)
    planarr = caldata[planvar]
    harvarr = caldata[harvvar]
    harvarr = xr.where(harvarr < planarr,harvarr + 365,harvarr) - 1 

    # Define DD limits
    gddlim = gddlims[crop]
    eddlim = eddlims[crop]

    for dtfname in dtfnames:
        dtfbasename = os.path.splitext(os.path.basename(dtfname))[0]
        print(crop + " " + dtfbasename)

        # Open the deltaT dataset. Should have just a deltat variable
        # but we'll get the first 2D variable on it, and
        # throw a warning if there's more than one
        dtds = xr.open_dataset(dtfname)

        # Fix metadata
        dtds = dtds.rename_dims({"X" : "longitude", "Y" : "latitude"})

        # Get the first 2D variable
        varnames2d = [var for var in list(dtds.data_vars) if len(dtds[var].shape) == 2]
        if len(varnames2d) != 1:
            print("WARNING: There are " + len(varnames2d) + " 2D variables in file ")
            print(dtfname)
            print(varnames2d)
            print("Defaulting to the first one")
        dtarr = dtds[varnames2d[0]]



        allyearsout = xr.Dataset()
        for hyear in tqdm.tqdm(hyears):
            # Open the climate arrays, concatenating them
            temparr = concat_clim(infolder,masterpref,tempsuf,hyear)
            tmaxarr = concat_clim(infolder,masterpref,tmaxsuf,hyear)
            tminarr = concat_clim(infolder,masterpref,tminsuf,hyear)

            # Evaluate baseline DDs FIXME: We could set this up to be run just once per year
            basedds = calc_all_Wrap(temparr,tmaxarr,tminarr,mskarr,gddlim,eddlim)
            # Evaluate scenario DDs
            scendds = calc_all_Wrap(temparr + dtarr,tmaxarr + dtarr,tminarr + dtarr,mskarr,gddlim,eddlim)

            # deltaEDD and deltaGDD
            deltadds = scendds - basedds

            # Add a year variable for concatenating later
            yout = deltadds
            yout = yout.expand_dims('year')
            yout["year"] = pd.Index([hyear])

            if len(list(allyearsout.data_vars)) == 0:
                allyearsout = yout
            else:
                allyearsout = xr.concat([allyearsout,yout], dim = 'year')

        # We'll use the average deltaEDD from the baseline
        outdata = allyearsout.mean(dim = 'year')

        # Some metadata
        outdata.attrs['year_start'] = min(hyears)
        outdata.attrs['year_end'] = max(hyears)
        outdata.attrs['calendar_path'] = calfolder+crop+calsuf
        outdata.attrs['climdata_path'] = infolder
        outdata.attrs['deltat_file'] = dtfname
        outdata.attrs['comments'] = "DDs here are average differences caused by an input deltat file"
        #         outdata.attrs['climdata_ex_path'] = infolder+temppref+str(hyear)+".nc"

        # Write output
        outfname = outfolder + crop + ".deltadd." + "_" + dtfbasename +  "_" + ".nc"
        outdata.to_netcdf(outfname,
                    engine = "netcdf4")



# %%
