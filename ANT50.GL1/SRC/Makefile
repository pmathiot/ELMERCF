
SRC=./

BLD=../MY_BLD/

ELMERINC=-I$(ELMER_NETCDFF_INC) -I$(ELMER_NETCDFC_INC)

ELMERLIB=-L$(ELMER_NETCDFF_LIB) -L$(ELMER_NETCDFC_LIB) -lnetcdf -lnetcdff

VPATH=$(BLD)

FFLAGS=-O3 

LIBS= USFs_Ronnie_inversion INITMIP_Scalar_OUTPUT Save_Netcdf Flotation2 boxmodel_solver nearestpoint FrontThickness_mask USF_CondFront USF_CondDistance USF_WaterPressure PDC_BASALMELT Elmer2NEMO_Interp NCFILE_BASALMELT Time_Mean TransientRestartInit USF_ExitCondition

all: $(LIBS)

NCFILE_BASALMELT: $(SRC)/NCFILE_BASALMELT.F90
	elmerf90 $(ELMERINC) $^ -o $(BLD)/$@ $(FFLAGS) $(ELMERLIB)

Time_Mean:$(SRC)/Time_Mean.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

TransientRestartInit:$(SRC)/TransientRestartInit.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

INITMIP_Scalar_OUTPUT:$(SRC)/INITMIP_Scalar_OUTPUT_modif.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

Save_Netcdf:$(SRC)/Save_Netcdf.F90
	elmerf90 $(ELMERINC) $^ -o $(BLD)/$@ $(FFLAGS) $(ELMERLIB)

Flotation2:$(SRC)/Flotation2.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

boxmodel_solver:$(SRC)/boxmodel_solver.F90
	elmerf90 $(ELMERINC) $^ -o $(BLD)/$@ $(FFLAGS) $(ELMERLIB)

nearestpoint:$(SRC)/nearestpoint.F90
	elmerf90 $(ELMERINC) $^ -o $(BLD)/$@ $(FFLAGS) $(ELMERLIB)

FrontThickness_mask:$(SRC)/FrontThickness_mask.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

USF_CondFront:$(SRC)/USF_CondFront.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

USF_CondDistance:$(SRC)/USF_CondDistance.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

USF_WaterPressure:$(SRC)/USF_WaterPressure.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

USF_ExitCondition:${SRC}/USF_ExitCondition.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

USFs_Ronnie_inversion:$(SRC)/USFs_Ronnie_inversion.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

PDC_BASALMELT:$(SRC)/PDC_BASALMELT.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

Elmer2NEMO_Interp: $(SRC)/Elmer2NEMO_Interp.F90
	elmerf90 $(ELMERINC) $^ -o $(BLD)/$@ $(FFLAGS) $(ELMERLIB)

clean:
	cd $(BLD) && rm -f $(LIBS)
