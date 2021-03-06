#!/usr/bin/env python
# coding: utf-8
print('load modules')
import pandas as pd
import re
import os
import glob
import matplotlib
matplotlib.use('TkAgg')
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
    parser.add_argument("-cfg"  , metavar='cfg name'    , help="configuration name"                  , type=str, nargs=1   , required=True )
    parser.add_argument("-runid", metavar='runid list'  , help="used to look information in runid.db", type=str, nargs='+' , required=True )
#    parser.add_argument("-refid", metavar='refid name'  , help="used to look information in runid.db", type=str, nargs=1   , required=False )
    parser.add_argument("-basin", metavar='basin number', help="basin number"                       , type=str, nargs='+'  , required=False, default=['00'] )
    parser.add_argument("-dir"  , metavar='directory of input file' , help="directory of input file", type=str, nargs=1    , required=False, default=[os.environ['EDDIR']+'/'])
    parser.add_argument("-o"    , metavar='figure_name', help="output figure name without extension", type=str, nargs=1    , required=False, default=['output'])
    parser.add_argument("-noshow" , help="do not display the figure (only save it)"                                        , required=False, action="store_true")
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

# inputs
print('load arguments and constants')
args=load_arguments()

cdir=args.dir[0]

CONFIG=args.cfg[0]

if args.basin[0] == 'ALL':
    BASINs=["%.2d" % i for i in range(20)]
else :
    BASINs=args.basin[:]

RUNIDs=args.runid[:] #['ANT50.GL1-EPM007','ANT50.GL1-EPM008','ANT50.GL1-EPM009','ANT50.GL1-EPM010','ANT50.GL1-EPM011']

#refid=args.refid[0]

cfile_out=args.o[0]

plot_keys =['SMB Flux', 'BMB Flux' , 'Ice Discharge', 'Ice flux at Grounding Line', 'Residual Flux', 'Floating ice area', 'Volume'  , 'Volume rate of change']

plot_sf   =[1e-9*0.917, -1e-9*0.917,  1e-9*0.917    ,  1e-9*0.917                 ,  1e-9*0.917    ,  1e-6*1e-6         ,  1e-6*1e-9,  1e-9             ]

plot_units=['Gt/y'    , 'Gt/y'     , 'Gt/y'         , 'Gt/y'                      , 'Gt/y'         ,  '1e6 km2'          , '1e6 km3' , 'km3/y']

print('load db files')
# read header
var=[]
#with open(cdir+'/'+CONFIG+'/'+RUNIDs[0]+'/'+RUNIDs[0]+'_S/Basin_'+RUNIDs[0]+'elmer_*.*.365d_INITMIP.dat') as f:
#with open(cdir+'/'+CONFIG+'/'+RUNIDs[0]+'/'+RUNIDs[0]+'_S/Basin_'+RUNIDs[0]+'elmer_*.*.365d_INITMIP.dat') as f:
with open(cdir+'/'+CONFIG+'/'+RUNIDs[0]+'/'+RUNIDs[0]+'_S/INITMIP_Scalar_OUTPUT_'+RUNIDs[0]+'_1.dat.names') as f:
    lines = f.readlines()
    for line in lines:
        linevar=re.findall( ' *\d\d?: .*' , line)
        if len(linevar) > 0:
            var.append(linevar[0].split(':')[1].strip())

# get basin definition
cfile_basin,dict_basin=parse_dbbasin()

# create dictionary for all var
dict_df={}

print('start plotting:')
# load all the cvs file
for cbasin in BASINs:
    print('    '+cbasin)
    datadf=[]
    plot_sty=[]
    plot_clr=[]
    line_name=[]
    for runid in RUNIDs:
#Basin19_eORCA025.L121-EPM026_elmer_20081231.30.365d_INITMIP.dat
        files=glob.glob(cdir+'/'+CONFIG+'/'+runid+'/'+runid+'_S/Basin'+cbasin+'INITMIP_Scalar_OUTPUT_'+runid+'_*.dat')
        #files=glob.glob(cdir+'/'+CONFIG+'/'+runid+'/'+runid+'_S/Basin'+cbasin+'_'+runid+'elmer_*.*.365d_INITMIP.dat')
        data=[]
        for file in files:
            df = pd.read_csv(file, header = None, delimiter = '\s+',names=var).set_index('Time')
            data.append(df)
        datadf.append(pd.concat(data,axis=0))
    
        _, runid_name, styline, styclr = parse_dbfile(runid)
        line_name.append(runid_name)
        plot_sty.append(styline)
        plot_clr.append(styclr)

    title_ext=''
#    if args.refid:
#        refdf=[]
#        files=glob.glob(cdir+'/'+CONFIG+'/'+refid+'/'+refid+'_S/Basin'+cbasin+'INITMIP_Scalar_OUTPUT_'+refid+'_*.dat')
#        data=[]
#        for file in files:
#            df = pd.read_csv(file, header = None, delimiter = '\s+',names=var).set_index('Time')
#            data.append(df)
#        refdf.append(pd.concat(data,axis=0))
#        title_ext=' [diff. with reference '+refid[0]+']'
#        sys.exit('diff with reference run still in progress')
   
    print(line_name) 
    # transform each run data frame to variable data frame
    for ikey,ckey in enumerate(plot_keys):
        df=[]
        for irunid,runid in enumerate(RUNIDs):
            df.append(datadf[irunid][[ckey]].rename(columns={ckey:line_name[irunid]}).sort_index()*plot_sf[ikey])
        dict_df[ckey]=pd.concat(df, axis=1)
   
    # plot data
    fig=plt.figure(figsize=(16,14), dpi= 100, facecolor='w', edgecolor='k')
    #axes = fig.subplots(3,3)
    axes=[None]*9
    fig.suptitle('Elmer monitoring (basin '+dict_basin[cbasin]+')'+title_ext,fontsize=18)
    count=0
    for r in range(3):
        for c in range(3):
            count+=1
            if (r,c) != (2,2):
                axes[count-1] = fig.add_subplot(3,3,count)
                ckey=list(dict_df.keys())[count-1]
                dict_df[ckey].index=dict_df[ckey].index-dict_df[ckey].index.min()
                dict_df[ckey]=dict_df[ckey].interpolate(limit_area='inside')

                lg=dict_df[ckey].plot(ax=axes[count-1],legend=False,label=line_name,linewidth=2,fontsize=14,color=plot_clr,style=plot_sty)
                axes[count-1].set_title(ckey+' ['+plot_units[count-1]+']',fontsize=16)
                axes[count-1].set_xlabel('')
                #axes[count-1].ticklabel_format(axis='both', style='plain', useOffset=False)
                axes[count-1].ticklabel_format(axis='y', style='sci', scilimits=(0,0), useOffset=False)
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
                axes[count-1] = fig.add_subplot(3,3,9,projection=proj)
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
