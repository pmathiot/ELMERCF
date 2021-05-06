
SRC=SRC/

BLD=BLD/

VPATH=$(BLD)

#FFLAGS= 
FFLAGS=-g -CB -O0

LIBS= USFs_Ronnie_inversion INITMIP_Scalar_OUTPUT Save_Netcdf Flotation2 boxmodel_solver nearestpoint FrontThickness_mask USF_CondFront USF_CondDistance USF_WaterPressure PDC_BASALMELT Elmer2NEMO_Interp Temporal_Mean_Step1 Temporal_Mean_Step2 READ_CELL_NETCDF

all: $(LIBS)

READ_CELL_NETCDF: $(SRC)/READ_CELL_NETCDF.F90
	elmerf90 -I$(NETCDF_INC_DIR) $^ -o $(BLD)/$@ $(FFLAGS) -L$(NETCDF_LIB_DIR) -lnetcdf -lnetcdff

Temporal_Mean_Step1:$(SRC)/Temporal_Mean_Step1.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

Temporal_Mean_Step2:$(SRC)/Temporal_Mean_Step2.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

USFs_Ronnie_inversion:$(SRC)/USFs_Ronnie_inversion.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

INITMIP_Scalar_OUTPUT:$(SRC)/INITMIP_Scalar_OUTPUT_modif.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

Save_Netcdf:$(SRC)/Save_Netcdf.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

Flotation2:$(SRC)/Flotation2.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

boxmodel_solver:$(SRC)/boxmodel_solver.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

nearestpoint:$(SRC)/nearestpoint.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

FrontThickness_mask:$(SRC)/FrontThickness_mask.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

USF_CondFront:$(SRC)/USF_CondFront.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

USF_CondDistance:$(SRC)/USF_CondDistance.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

USF_WaterPressure:$(SRC)/USF_WaterPressure.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

PDC_BASALMELT:$(SRC)/PDC_BASALMELT.F90
	elmerf90 $^ -o $(BLD)/$@ $(FFLAGS)

Elmer2NEMO_Interp: $(SRC)/Elmer2NEMO_Interp.F90
	elmerf90 -I$(NETCDF_INC_DIR) $^ -o $(BLD)/$@ $(FFLAGS) -L$(NETCDF_LIB_DIR) -lnetcdf -lnetcdff

clean:
	cd $(BLD) && rm -f $(LIBS)
