# ANT50.GL1

## Configuration Description
- Resolution: Unstructured grid 1 - 50 km
- Stress balance: SSA
- GL treatment: Floatation
- Initialisation: Inversion
- Rheology: Glen's flow law
- Friction law: Weertman linear
- Hydrology: None
- Calving front: fix
- SMB: MAR averaged 1979-2015
- BMB: NEMO output

## Simulations

### ANT50.GL1-EPM007
- The ice shelf melt comes from the NEMO eORCA025.L121-OPM021 simulation over period 1985-1995 drowned and masked by the initial grounding mask (mask constant in time) with melt up to the initial GL. 
### ANT50.GL1-EPM008
- The ice shelf melt comes from the NEMO eORCA025.L121-OPM021 simulation over period 1985-1995 (no specific mask applied, ie closed to NEMO mask)
### ANT50.GL1-EPM009
- The ice shelf melt comes from the NEMO eORCA025.L121-OPM021 simulation over period 1985-1995 drowned and masked at every time step by the current grounded mask (no melt at GL)
### ANT50.GL1-EPM010
- The ice shelf melt comes from the NEMO eORCA025.L121-OPM021 simulation over period 1985-1995 drowned and masked at every time step by the current grounded mask (melt at GL)
### ANT50.GL1-EPM011
- The ice shelf melt comes from the NEMO eORCA025.L121-OPM021 simulation over period 1985-1995 drowned and masked by the initial grounding mask (mask constant in time) without melt at GL. 
