#!/usr/bin/env python
# coding: utf-8
print('load modules')
import pandas as pd
import re
import glob
import matplotlib.pyplot as plt
import argparse
import sys
import cartopy.crs as ccrs
import cartopy
import xarray as xr

print('load functions')
# load arguments
def load_arguments():
    # deals with argument
    parser = argparse.ArgumentParser()
    parser.add_argument("-refid", metavar='refid list' , help="used to look information in runid.db", type=str, nargs='+' , required=True )
    parser.add_argument("-runid", metavar='runid list' , help="used to look information in runid.db", type=str, nargs='+' , required=True )
    parser.add_argument("-basin", metavar='basin number', help="basin number"                       , type=str, nargs='+' , required=False, default=['00'] )
    parser.add_argument("-dir"  , metavar='directory of input file' , help="directory of input file", type=str, nargs=1   , required=False, default=['./'])
    parser.add_argument("-o"    , metavar='figure_name', help="output figure name without extension", type=str, nargs=1   , required=False, default=['output'])
    parser.add_argument("-noshow" , help="do not display the figure (only save it)"                                       , required=False, action="store_true")
    return parser.parse_args()

# parse style name
def parse_dbfile(runid):
    try:
        lstyle=False
        with open('style_elmer.db') as fid:
            for cline in fid:
                att=cline.split('|')
                if att[0].strip() == runid:
                    cpltrunid = att[0].strip()
                    cpltname  = att[1].strip()
                    cpltline  = att[2].strip()
                    cpltcolor = att[3].strip()
                    lstyle=True
        if not lstyle:
            print( runid+' not found in style_elmer.db' )
            raise Exception

    except Exception as e:
        print( 'Issue with file : style_elmer.db' )
        print( e )
        sys.exit(42)

    # return value
    return cpltrunid, cpltname, cpltline, cpltcolor

def parse_dbbasin():
    try:
        with open('basin_elmer.db') as fid:
            iline=0
            dict_basin={}
            for cline in fid:
                iline += 1
                if iline == 1:
                    cfile_basin=cline.split(':')[1].strip()
                else :
                    dict_basin[cline.split('|')[0].strip()]=cline.split('|')[1].strip()

    except Exception as e:
        print( 'Issue with file : basin_elmer.db' )
        print( e )
        sys.exit(42)

    return cfile_basin,dict_basin

def read_datname(cfile):
    var=[]
    with open(cfile) as f:
        lines = f.readlines()
        for line in lines:
            linevar=re.findall( ' *\d\d?: .*' , line)
            if len(linevar) > 0:
                var.append(''.join(linevar[0].split(':')[1:]).strip())
    return var

def read_dat(cfile,varlst,ctime='Time'):
    files=glob.glob(cfile)
    data=[]
    for file in files:
        df = pd.read_csv(file, header = None, delimiter = '\s+',names=varlst).rename(columns={ctime:'Time'}).set_index('Time')
        data.append(df)
    return data

def rundf_to_vardf(runidlst,dflst,keyslst):
    dict_df={}
    for ikey,ckey in enumerate(keyslst):
        df=[]
        for irunid,runid in enumerate(runidlst):
            df.append(dflst[irunid][[ckey]].rename(columns={ckey:runid}).sort_index())
        dict_df[ckey]=pd.concat(df, axis=1)
    return dict_df

# inputs
print('load arguments and constants')
args=load_arguments()

cdir=args.dir[0]

BASINs=args.basin[:]

RUNIDs=args.runid[:]
REFIDs=args.refid[:]

cfile_out=args.o[0]
   
plot_keys =['norm dhdt', 'norm h', 'norm ssavelocity', 'int h  mpi_sum', 'int dhdt  mpi_sum','int smb  mpi_sum','int melt  mpi_sum','sum h residual  mpi_sum']

plot_sf   =[ 1.0                ,  1.0            , 1.0         , 1.0, 1.0, 1.0, 1.0, 1.0]

plot_units=[ '???'              ,  '???'          , '???'       , '???', '???', '???', '???', '???']

print('load db files')
# read header
varsca = read_datname(cdir+'/'+RUNIDs[0]+'/'+RUNIDs[0]+'_S/'+RUNIDs[0]+'_1_scalars.dat.names')
print(varsca)

# get basin definition
cfile_basin,dict_basin=parse_dbbasin()

# style
plot_sty=[]
plot_clr=[]
for runid in RUNIDs:
    _, runid_name, styline, styclr = parse_dbfile(runid)
    plot_sty.append(styline)
    plot_clr.append(styclr)

print('start plotting:')
# load all the cvs file
for cbasin in BASINs:
    print('    '+cbasin)

    # load refids
    datadf=[]
    for runid in REFIDs:
        datasca = read_dat(cdir+'/'+runid+'/'+runid+'_S/'+runid+'_*_scalars.dat',varsca,'value time scalar variable')
        df=pd.concat(datasca,axis=0)
        datadf.append(df)
    
    # transform each run data frame to variable data frame
    dict_refdf = rundf_to_vardf(REFIDs,datadf,plot_keys)
 
    # load runids
    datadf=[]
    for runid in RUNIDs:
        datasca = read_dat(cdir+'/'+runid+'/'+runid+'_S/'+runid+'_*_scalars.dat',varsca,'value time scalar variable')
        df=pd.concat(datasca,axis=0)
        datadf.append(df)
    
    # transform each run data frame to variable data frame
    dict_df = rundf_to_vardf(RUNIDs,datadf,plot_keys)

    # compute differences
    for ikey,ckey in enumerate(plot_keys):
        for runid in RUNIDs:
            dict_df[ckey][runid]=dict_df[ckey][runid]-dict_refdf[ckey][REFIDs[0]]
            dict_df[ckey][runid]=dict_df[ckey][runid]*plot_sf[ikey]
 
    # plot data
    fig=plt.figure(figsize=(16,20), dpi= 100, facecolor='w', edgecolor='k')
    #axes = fig.subplots(3,3)
    axes=[None]*12
    fig.suptitle('Elmer restart [ref = '+REFIDs[0]+']',fontsize=18)
    count=0
    for r in range(3):
        for c in range(3):
            count+=1
            if (r,c) != (2,2):
                axes[count-1] = fig.add_subplot(3,3,count)
                ckey=list(dict_df.keys())[count-1]
                dict_df[ckey].index=dict_df[ckey].index-dict_df[ckey].index.min()
                dict_df[ckey]=dict_df[ckey].dropna()#interpolate(limit_area='inside')
                lg=dict_df[ckey].plot(ax=axes[count-1],legend=False,label=RUNIDs,linewidth=2,fontsize=14,color=plot_clr,style=plot_sty)
                axes[count-1].set_title(ckey+' ['+plot_units[count-1]+']',fontsize=16)
                axes[count-1].set_xlabel('')
                #axes[count-1].ticklabel_format(axis='both', style='plain', useOffset=False)
                axes[count-1].grid(True)
                ymin=dict_df[ckey].quantile([.01]).min().min()
                ymax=dict_df[ckey].quantile([.99]).max().max()
                yrange=ymax-ymin
                ymin=ymin-0.05*yrange
                ymax=ymax+0.05*yrange
                axes[count-1].set_ylim(ymin, ymax)
                if r == 2 or (r,c) == (1,2):
                    axes[count-1].set_xlabel('Time [years]',fontsize=16)
            else:
                proj = ccrs.Stereographic(central_latitude=-90.0, central_longitude=0.0) 
                axes[count-1] = fig.add_subplot(4,3,12,projection=proj)
                # read netcdf
                basin_df=xr.open_dataset(cfile_basin)
                basin_df=basin_df.where(basin_df.basins == int(cbasin), drop=True)
                # plot data
                basin_df.basins.plot(x='lon', y='lat', transform=ccrs.PlateCarree(),add_colorbar=False)
                axes[count-1].set_extent((-180, 180, -90, -65), ccrs.PlateCarree())
                feature=cartopy.feature.NaturalEarthFeature('physical', 'antarctic_ice_shelves_polys', '50m', facecolor='none')
                axes[count-1].add_feature(feature,linewidth=0.5,edgecolor='k')
                feature=cartopy.feature.NaturalEarthFeature('physical', 'coastline'                , '50m', facecolor='none')
                axes[count-1].add_feature(feature,linewidth=0.5,edgecolor='k')
    
    fig.subplots_adjust(left=0.01,right=0.98, bottom=0.05, top=0.93, wspace=0.17, hspace=0.17)
    
    lax = plt.axes([0.0, 0.0, 1, 0.00])
    lline, llabel = lg.get_legend_handles_labels()
    leg=plt.legend(lline, llabel, loc='upper left', ncol = 4, frameon=False,fontsize=16)
    lax.set_axis_off()
   
    cfile_out_png=cfile_out+'_'+cbasin+'.png' 
    fig.savefig(cfile_out_png, format='png', dpi=150, bbox_inches="tight")
    
    if args.noshow:
       pass
    else:
       plt.show()
