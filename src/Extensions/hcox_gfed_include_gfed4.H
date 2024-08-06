!EOC
!------------------------------------------------------------------------------
!                  Harmonized Emissions Component (HEMCO)                     !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: hcox_gfed_include_gfed4.H
!
! !DESCRIPTION: Include file with GFED emission factor data that was
!  originally contained in file GFED\_emission\_factors.txt.  We have now
!  transformed this file into hardwired F90 commands in order to avoid reading
!  an ASCII file in the ESMF environment.
!
! !REMARKS:
!  ABOUT THIS FILE:
!  ----------------
!  This file was created by script HEMCO/Extensions/Preprocess/gfed.pl.
!  This script can be executed with the following command:
!
!    cd HEMCO/Extensions/Preprocess
!    make gfed
!
!  This will regenerate this include file from the original data and
!  automatically place it in the HEMCO/Extensions directory.
!
!  White space has been removed in order to reduce the file size as much
!  as possible.  If you have to recreate this file, then it is easier to
!  generate via the Perl script than to try to hand edit the code below.
!
!  DATA:
!  -----

!  The Inst%GFED4_EMFAC array contains emission factors in kg/kgDM or kgC/kgDM
!  Inst%GFED4_EMFAC(N,1) = Savanna, grassland, and shrubland fires Emission Factor
!  Inst%GFED4_EMFAC(N,2) = Boreal forest fires Emission Factor
!  Inst%GFED4_EMFAC(N,3) = Temperate forest fires Forest Emission Factor
!  Inst%GFED4_EMFAC(N,4) = Deforestation (tropical) and degradation Emission Factor
!  Inst%GFED4_EMFAC(N,5) = Peat fires Emission Factor
!  Inst%GFED4_EMFAC(N,6) = Agricultural waste burning Emission Factor

! !REVISION HISTORY:
!  08 Aug 2014 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

! CO
GFED4_SPEC_NAME(1)="CO"

Inst%GFED4_EMFAC(1,1)=6.90E-02_hp
Inst%GFED4_EMFAC(1,2)=1.21E-01_hp
Inst%GFED4_EMFAC(1,3)=1.13E-01_hp
Inst%GFED4_EMFAC(1,4)=1.04E-01_hp
Inst%GFED4_EMFAC(1,5)=2.60E-01_hp
Inst%GFED4_EMFAC(1,6)=7.60E-02_hp

! ALK4 (Higher_Alkanes) - convert from kgC to kg
GFED4_SPEC_NAME(2)="ALK4"
Inst%GFED4_EMFAC(2,1)=8.68E-05_hp * 58.12_hp/(4.3_hp*12.0_hp)
Inst%GFED4_EMFAC(2,2)=3.04E-04_hp * 58.12_hp/(4.3_hp*12.0_hp)
Inst%GFED4_EMFAC(2,3)=2.21E-04_hp * 58.12_hp/(4.3_hp*12.0_hp)
Inst%GFED4_EMFAC(2,4)=1.58E-04_hp * 58.12_hp/(4.3_hp*12.0_hp)
Inst%GFED4_EMFAC(2,5)=1.00E-03_hp * 58.12_hp/(4.3_hp*12.0_hp)
Inst%GFED4_EMFAC(2,6)=2.24E-04_hp * 58.12_hp/(4.3_hp*12.0_hp)

! ACET (C3H6O)
GFED4_SPEC_NAME(3)="ACET"
Inst%GFED4_EMFAC(3,1)=4.70E-04_hp
Inst%GFED4_EMFAC(3,2)=1.59E-03_hp
Inst%GFED4_EMFAC(3,3)=7.60E-04_hp
Inst%GFED4_EMFAC(3,4)=6.30E-04_hp
Inst%GFED4_EMFAC(3,5)=9.10E-04_hp
Inst%GFED4_EMFAC(3,6)=7.10E-04_hp

! MEK as 2-butanone
GFED4_SPEC_NAME(4)="MEK"
Inst%GFED4_EMFAC(4,1)=1.30E-04_hp
Inst%GFED4_EMFAC(4,2)=1.60E-04_hp
Inst%GFED4_EMFAC(4,3)=2.30E-04_hp
Inst%GFED4_EMFAC(4,4)=5.00E-04_hp
Inst%GFED4_EMFAC(4,5)=3.40E-04_hp
Inst%GFED4_EMFAC(4,6)=5.80E-04_hp

! ALD2 (C2H4O)
GFED4_SPEC_NAME(5)="ALD2"
Inst%GFED4_EMFAC(5,1)=8.40E-04_hp
Inst%GFED4_EMFAC(5,2)=8.10E-04_hp
Inst%GFED4_EMFAC(5,3)=1.21E-03_hp
Inst%GFED4_EMFAC(5,4)=2.26E-03_hp
Inst%GFED4_EMFAC(5,5)=1.16E-03_hp
Inst%GFED4_EMFAC(5,6)=1.80E-03_hp

! PRPE (C3H6 + Higher_Alkenes)
!tsc added 1,3-butadiene
GFED4_SPEC_NAME(6)="PRPE"
Inst%GFED4_EMFAC(6,1)=4.60E-04_hp + 2.58E-04_hp * 42.08_hp/(3.0_hp*12.0_hp)
Inst%GFED4_EMFAC(6,2)=6.70E-04_hp + 3.64E-04_hp * 42.08_hp/(3.0_hp*12.0_hp)
Inst%GFED4_EMFAC(6,3)=6.00E-04_hp + 4.24E-04_hp * 42.08_hp/(3.0_hp*12.0_hp)
Inst%GFED4_EMFAC(6,4)=8.60E-04_hp + 4.04E-04_hp * 42.08_hp/(3.0_hp*12.0_hp)
Inst%GFED4_EMFAC(6,5)=1.14E-03_hp + 1.13E-03_hp * 42.08_hp/(3.0_hp*12.0_hp)
Inst%GFED4_EMFAC(6,6)=4.70E-04_hp + 3.50E-04_hp * 42.08_hp/(3.0_hp*12.0_hp)

! C3H8
GFED4_SPEC_NAME(7)="C3H8"
Inst%GFED4_EMFAC(7,1)=1.30E-04_hp 
Inst%GFED4_EMFAC(7,2)=2.90E-04_hp 
Inst%GFED4_EMFAC(7,3)=2.80E-04_hp 
Inst%GFED4_EMFAC(7,4)=5.30E-04_hp 
Inst%GFED4_EMFAC(7,5)=9.90E-04_hp 
Inst%GFED4_EMFAC(7,6)=1.70E-04_hp 

! CH2O
GFED4_SPEC_NAME(8)="CH2O"
Inst%GFED4_EMFAC(8,1)=1.23E-03_hp
Inst%GFED4_EMFAC(8,2)=1.75E-03_hp
Inst%GFED4_EMFAC(8,3)=2.04E-03_hp
Inst%GFED4_EMFAC(8,4)=2.40E-03_hp
Inst%GFED4_EMFAC(8,5)=1.07E-03_hp
Inst%GFED4_EMFAC(8,6)=1.80E-03_hp

! C2H6
GFED4_SPEC_NAME(9)="C2H6"
Inst%GFED4_EMFAC(9,1)=4.20E-04_hp
Inst%GFED4_EMFAC(9,2)=9.70E-04_hp
Inst%GFED4_EMFAC(9,3)=6.90E-04_hp
Inst%GFED4_EMFAC(9,4)=8.80E-04_hp
Inst%GFED4_EMFAC(9,5)=1.85E-03_hp
Inst%GFED4_EMFAC(9,6)=7.90E-04_hp

! SO2
GFED4_SPEC_NAME(10)="SO2"
Inst%GFED4_EMFAC(10,1)=4.80E-04_hp
Inst%GFED4_EMFAC(10,2)=1.10E-03_hp
Inst%GFED4_EMFAC(10,3)=1.10E-03_hp
Inst%GFED4_EMFAC(10,4)=4.00E-04_hp
Inst%GFED4_EMFAC(10,5)=4.00E-04_hp
Inst%GFED4_EMFAC(10,6)=4.00E-04_hp

! NH3
GFED4_SPEC_NAME(11)="NH3"
Inst%GFED4_EMFAC(11,1)=5.20E-04_hp
Inst%GFED4_EMFAC(11,2)=2.72E-03_hp
Inst%GFED4_EMFAC(11,3)=8.40E-04_hp
Inst%GFED4_EMFAC(11,4)=1.33E-03_hp
Inst%GFED4_EMFAC(11,5)=1.33E-03_hp
Inst%GFED4_EMFAC(11,6)=2.17E-03_hp

! BC - already per carbon
GFED4_SPEC_NAME(12)="BC"
Inst%GFED4_EMFAC(12,1)=3.70E-04_hp
Inst%GFED4_EMFAC(12,2)=5.00E-04_hp
Inst%GFED4_EMFAC(12,3)=5.00E-04_hp
Inst%GFED4_EMFAC(12,4)=5.20E-04_hp
Inst%GFED4_EMFAC(12,5)=4.00E-05_hp
Inst%GFED4_EMFAC(12,6)=7.50E-04_hp

! OC - already per carbon
GFED4_SPEC_NAME(13)="OC"
Inst%GFED4_EMFAC(13,1)=2.62E-03_hp
Inst%GFED4_EMFAC(13,2)=9.60E-03_hp
Inst%GFED4_EMFAC(13,3)=9.60E-03_hp
Inst%GFED4_EMFAC(13,4)=4.71E-03_hp
Inst%GFED4_EMFAC(13,5)=6.02E-03_hp
Inst%GFED4_EMFAC(13,6)=2.30E-03_hp

! NO (NOx)
GFED4_SPEC_NAME(14)="NO"
Inst%GFED4_EMFAC(14,1)=3.90E-03_hp
Inst%GFED4_EMFAC(14,2)=9.00E-04_hp
Inst%GFED4_EMFAC(14,3)=1.92E-03_hp
Inst%GFED4_EMFAC(14,4)=2.55E-03_hp
Inst%GFED4_EMFAC(14,5)=1.00E-03_hp
Inst%GFED4_EMFAC(14,6)=3.11E-03_hp

! MGLY (CH3COCHO)
GFED4_SPEC_NAME(15)="MGLY"
Inst%GFED4_EMFAC(15,1)=4.00E-04_hp
Inst%GFED4_EMFAC(15,2)=5.70E-04_hp
Inst%GFED4_EMFAC(15,3)=2.70E-04_hp
Inst%GFED4_EMFAC(15,4)=4.90E-04_hp
Inst%GFED4_EMFAC(15,5)=2.30E-04_hp
Inst%GFED4_EMFAC(15,6)=5.50E-04_hp

! BENZ (C6H6)
GFED4_SPEC_NAME(16)="BENZ"
Inst%GFED4_EMFAC(16,1)=3.30E-04_hp
Inst%GFED4_EMFAC(16,2)=5.70E-04_hp
Inst%GFED4_EMFAC(16,3)=4.20E-04_hp
Inst%GFED4_EMFAC(16,4)=3.80E-04_hp
Inst%GFED4_EMFAC(16,5)=8.70E-04_hp
Inst%GFED4_EMFAC(16,6)=2.70E-04_hp

! TOLU (C7H8)
GFED4_SPEC_NAME(17)="TOLU"
Inst%GFED4_EMFAC(17,1)=1.90E-04_hp
Inst%GFED4_EMFAC(17,2)=3.50E-04_hp
Inst%GFED4_EMFAC(17,3)=2.70E-04_hp
Inst%GFED4_EMFAC(17,4)=2.30E-04_hp
Inst%GFED4_EMFAC(17,5)=4.50E-04_hp
Inst%GFED4_EMFAC(17,6)=1.70E-04_hp

! XYLE (C8H10)
GFED4_SPEC_NAME(18)="XYLE"
Inst%GFED4_EMFAC(18,1)=8.60E-05_hp
Inst%GFED4_EMFAC(18,2)=1.10E-04_hp
Inst%GFED4_EMFAC(18,3)=1.60E-04_hp
Inst%GFED4_EMFAC(18,4)=8.60E-05_hp
Inst%GFED4_EMFAC(18,5)=2.30E-04_hp
Inst%GFED4_EMFAC(18,6)=1.00E-04_hp

! C2H4
GFED4_SPEC_NAME(19)="C2H4"
Inst%GFED4_EMFAC(19,1)=8.30E-04_hp
Inst%GFED4_EMFAC(19,2)=1.54E-03_hp
Inst%GFED4_EMFAC(19,3)=1.11E-03_hp
Inst%GFED4_EMFAC(19,4)=1.11E-03_hp
Inst%GFED4_EMFAC(19,5)=1.47E-03_hp
Inst%GFED4_EMFAC(19,6)=1.00E-03_hp

! C2H2
GFED4_SPEC_NAME(20)="C2H2"
Inst%GFED4_EMFAC(20,1)=3.10E-04_hp
Inst%GFED4_EMFAC(20,2)=2.80E-04_hp
Inst%GFED4_EMFAC(20,3)=3.10E-04_hp
Inst%GFED4_EMFAC(20,4)=3.50E-04_hp
Inst%GFED4_EMFAC(20,5)=1.10E-04_hp
Inst%GFED4_EMFAC(20,6)=2.70E-04_hp

! GLYC (HOCH2CHO)
GFED4_SPEC_NAME(21)="GLYC"
Inst%GFED4_EMFAC(21,1)=1.30E-04_hp
Inst%GFED4_EMFAC(21,2)=3.80E-04_hp
Inst%GFED4_EMFAC(21,3)=3.90E-04_hp
Inst%GFED4_EMFAC(21,4)=3.30E-04_hp
Inst%GFED4_EMFAC(21,5)=1.10E-04_hp
Inst%GFED4_EMFAC(21,6)=3.20E-03_hp

! CO2
GFED4_SPEC_NAME(22)="CO2"
Inst%GFED4_EMFAC(22,1)=1.66E+00_hp
Inst%GFED4_EMFAC(22,2)=1.453E+00_hp
Inst%GFED4_EMFAC(22,3)=1.57E+00_hp
Inst%GFED4_EMFAC(22,4)=1.62E+00_hp
Inst%GFED4_EMFAC(22,5)=1.59E+00_hp
Inst%GFED4_EMFAC(22,6)=1.43E+00_hp

! CH4
GFED4_SPEC_NAME(23)="CH4"
Inst%GFED4_EMFAC(23,1)=2.70E-03_hp
Inst%GFED4_EMFAC(23,2)=5.50E-03_hp
Inst%GFED4_EMFAC(23,3)=5.20E-03_hp
Inst%GFED4_EMFAC(23,4)=6.50E-03_hp
Inst%GFED4_EMFAC(23,5)=9.10E-03_hp
Inst%GFED4_EMFAC(23,6)=5.70E-03_hp

! HCOOH
GFED4_SPEC_NAME(24)="HCOOH"
Inst%GFED4_EMFAC(24,1)=2.10E-04_hp
Inst%GFED4_EMFAC(24,2)=1.04E-03_hp
Inst%GFED4_EMFAC(24,3)=9.10E-04_hp
Inst%GFED4_EMFAC(24,4)=4.90E-04_hp
Inst%GFED4_EMFAC(24,5)=2.90E-04_hp
Inst%GFED4_EMFAC(24,6)=5.60E-04_hp

! DMS (C2H6S)
GFED4_SPEC_NAME(25)="DMS"
Inst%GFED4_EMFAC(25,1)=8.00E-06_hp
Inst%GFED4_EMFAC(25,2)=2.30E-06_hp
Inst%GFED4_EMFAC(25,3)=1.40E-05_hp
Inst%GFED4_EMFAC(25,4)=2.20E-06_hp
Inst%GFED4_EMFAC(25,5)=4.50E-05_hp
Inst%GFED4_EMFAC(25,6)=5.00E-05_hp

! ISOP (C5H8)
GFED4_SPEC_NAME(26)="ISOP"
Inst%GFED4_EMFAC(26,1)=1.01E-04_hp
Inst%GFED4_EMFAC(26,2)=7.40E-05_hp
Inst%GFED4_EMFAC(26,3)=1.00E-04_hp
Inst%GFED4_EMFAC(26,4)=1.00E-04_hp
Inst%GFED4_EMFAC(26,5)=5.20E-04_hp
Inst%GFED4_EMFAC(26,6)=1.70E-04_hp

! MTPA (C10H16) - not carried per carbon
GFED4_SPEC_NAME(27)="MTPA"
Inst%GFED4_EMFAC(27,1)=1.04E-04_hp
Inst%GFED4_EMFAC(27,2)=1.53E-03_hp
Inst%GFED4_EMFAC(27,3)=1.17E-03_hp
Inst%GFED4_EMFAC(27,4)=1.50E-04_hp
Inst%GFED4_EMFAC(27,5)=8.00E-05_hp
Inst%GFED4_EMFAC(27,6)=2.70E-05_hp

! MOH (CH3OH) - not carried per carbon
GFED4_SPEC_NAME(28)="MOH"
Inst%GFED4_EMFAC(28,1)=1.35E-03_hp
Inst%GFED4_EMFAC(28,2)=2.33E-03_hp
Inst%GFED4_EMFAC(28,3)=2.20E-03_hp
Inst%GFED4_EMFAC(28,4)=2.80E-03_hp
Inst%GFED4_EMFAC(28,5)=2.50E-03_hp
Inst%GFED4_EMFAC(28,6)=3.30E-03_hp

! EOH (C2H5OH)
GFED4_SPEC_NAME(29)="EOH"
Inst%GFED4_EMFAC(29,1)=3.60E-05_hp
Inst%GFED4_EMFAC(29,2)=5.80E-05_hp
Inst%GFED4_EMFAC(29,3)=7.60E-05_hp
Inst%GFED4_EMFAC(29,4)=6.70E-05_hp
Inst%GFED4_EMFAC(29,5)=1.70E-04_hp
Inst%GFED4_EMFAC(29,6)=5.00E-05_hp

! ACTA (CH3COOH) - not carried per carbon
GFED4_SPEC_NAME(30)="ACTA"
Inst%GFED4_EMFAC(30,1)=2.31E-03_hp
Inst%GFED4_EMFAC(30,2)=3.80E-03_hp
Inst%GFED4_EMFAC(30,3)=2.74E-03_hp
Inst%GFED4_EMFAC(30,4)=3.30E-03_hp
Inst%GFED4_EMFAC(30,5)=4.90E-03_hp
Inst%GFED4_EMFAC(30,6)=6.10E-03_hp

! SOAP
GFED4_SPEC_NAME(31)="SOAP"
Inst%GFED4_EMFAC(31,1)=Inst%GFED4_EMFAC(1,1) ! SAVA
Inst%GFED4_EMFAC(31,2)=Inst%GFED4_EMFAC(1,2) ! BORF
Inst%GFED4_EMFAC(31,3)=Inst%GFED4_EMFAC(1,3) ! TEMP
Inst%GFED4_EMFAC(31,4)=Inst%GFED4_EMFAC(1,4) ! DEFO
Inst%GFED4_EMFAC(31,5)=Inst%GFED4_EMFAC(1,5) ! PEAT
Inst%GFED4_EMFAC(31,6)=Inst%GFED4_EMFAC(1,6) ! AGRI

! HG0
GFED4_SPEC_NAME(32)="Hg0"
Inst%GFED4_EMFAC(32,1)=7.17E-07_hp * Inst%GFED4_EMFAC(1,1) ! SAV -- SAVA
Inst%GFED4_EMFAC(32,2)=7.17E-07_hp * Inst%GFED4_EMFAC(1,2) ! FOR -- BORF
Inst%GFED4_EMFAC(32,3)=7.17E-07_hp * Inst%GFED4_EMFAC(1,3) ! FOR -- TEMP
Inst%GFED4_EMFAC(32,4)=7.17E-07_hp * Inst%GFED4_EMFAC(1,4) ! DEF -- DEFO
Inst%GFED4_EMFAC(32,5)=7.17E-07_hp * Inst%GFED4_EMFAC(1,5) ! PET -- PEAT
Inst%GFED4_EMFAC(32,6)=7.17E-07_hp * Inst%GFED4_EMFAC(1,6) ! AGW -- AGRI

! HAC - not traced per carbon - from Akagi et al, 2011
GFED4_SPEC_NAME(33)="HAC"
Inst%GFED4_EMFAC(33,1)=5.60E-04_hp !SAV - Savannah
Inst%GFED4_EMFAC(33,2)=2.10E-03_hp !BORF - Boreal
Inst%GFED4_EMFAC(33,3)=1.13E-03_hp !TEMP - Temperate
Inst%GFED4_EMFAC(33,4)=1.81E-03_hp !DEFO - Tropical
Inst%GFED4_EMFAC(33,5)=6.40E-04_hp !PET -  Peatland
Inst%GFED4_EMFAC(33,6)=3.12E-03_hp !AGW - Crop residue

! GLYX - not traced per carbon
GFED4_SPEC_NAME(34)="GLYX"
Inst%GFED4_EMFAC(34,1)=3.30E-04_hp
Inst%GFED4_EMFAC(34,2)=5.90E-04_hp
Inst%GFED4_EMFAC(34,3)=5.40E-04_hp
Inst%GFED4_EMFAC(34,4)=5.00E-04_hp
Inst%GFED4_EMFAC(34,5)=1.30E-03_hp
Inst%GFED4_EMFAC(34,6)=2.40E-04_hp

! HCl
GFED4_SPEC_NAME(35)="HCl"
Inst%GFED4_EMFAC(35,1)=1.3E-04_hp
Inst%GFED4_EMFAC(35,2)=1.3E-04_hp
Inst%GFED4_EMFAC(35,3)=3.9E-05_hp
Inst%GFED4_EMFAC(35,4)=1.3E-04_hp
Inst%GFED4_EMFAC(35,5)=8.0E-06_hp
Inst%GFED4_EMFAC(35,6)=1.8E-04_hp

! RCHO
GFED4_SPEC_NAME(36)="RCHO"
Inst%GFED4_EMFAC(36,1)=1.2E-03_hp
Inst%GFED4_EMFAC(36,2)=9.4E-04_hp
Inst%GFED4_EMFAC(36,3)=8.6E-04_hp
Inst%GFED4_EMFAC(36,4)=1.4E-03_hp
Inst%GFED4_EMFAC(36,5)=1.4E-03_hp
Inst%GFED4_EMFAC(36,6)=1.7E-03_hp

! 1,3-butadiene
GFED4_SPEC_NAME(37)="C4H6"
Inst%GFED4_EMFAC(37,1)=9.5E-05_hp
Inst%GFED4_EMFAC(37,2)=8.9E-05_hp
Inst%GFED4_EMFAC(37,3)=1.25E-04_hp
Inst%GFED4_EMFAC(37,4)=1.5E-04_hp
Inst%GFED4_EMFAC(37,5)=2.2E-04_hp
Inst%GFED4_EMFAC(37,6)=1.6E-04_hp

! Dimethylfuran
GFED4_SPEC_NAME(38)="DMFU"
Inst%GFED4_EMFAC(38,1)=6.3E-05_hp !SAV - Savannah
Inst%GFED4_EMFAC(38,2)=1.0E-04_hp !BORF - Boreal
Inst%GFED4_EMFAC(38,3)=7.0E-05_hp !TEMP - Temperate
Inst%GFED4_EMFAC(38,4)=8.5E-05_hp !DEFO - Tropical
Inst%GFED4_EMFAC(38,5)=1.4E-04_hp !PET -  Peatland
Inst%GFED4_EMFAC(38,6)=9.8E-05_hp !AGW - Crop residue

! Furan by itself
! GFED4_SPEC_NAME(39)="FURA"
! Inst%GFED4_EMFAC(39,1)=2.9E-04_hp !SAV - Savannah
! Inst%GFED4_EMFAC(39,2)=3.6E-04_hp !BORF - Boreal
! Inst%GFED4_EMFAC(39,3)=4.1E-04_hp !TEMP - Temperate
! Inst%GFED4_EMFAC(39,4)=3.3E-04_hp !DEFO - Tropical
! Inst%GFED4_EMFAC(39,5)=1.07E-03_hp !PET -  Peatland
! Inst%GFED4_EMFAC(39,6)=9.0E-04_hp !AGW - Crop residue

! 2-methylfuran
!GFED4_SPEC_NAME(41)="MFUR"
!Inst%GFED4_EMFAC(41,1)=2.0E-04_hp !SAV - Savannah
!Inst%GFED4_EMFAC(41,2)=4.2E-04_hp !BORF - Boreal
!Inst%GFED4_EMFAC(41,3)=3.4E-04_hp !TEMP - Temperate
!Inst%GFED4_EMFAC(41,4)=2.8E-04_hp !DEFO - Tropical
!Inst%GFED4_EMFAC(41,5)=3.1E-04_hp !PET -  Peatland
!Inst%GFED4_EMFAC(41,6)=5.3E-04_hp !AGW - Crop residue

! Furan with DMFU and 2-methyl
GFED4_SPEC_NAME(39)="FURA"
Inst%GFED4_EMFAC(39,1)=5.5E-04_hp !SAV - Savannah
Inst%GFED4_EMFAC(39,2)=8.8E-04_hp !BORF - Boreal
Inst%GFED4_EMFAC(39,3)=8.2E-04_hp !TEMP - Temperate
Inst%GFED4_EMFAC(39,4)=7.0E-04_hp !DEFO - Tropical
Inst%GFED4_EMFAC(39,5)=1.52E-03_hp !PET -  Peatland
Inst%GFED4_EMFAC(39,6)=1.53E-03_hp !AGW - Crop residue

! Furfural
GFED4_SPEC_NAME(40)="FURF"
Inst%GFED4_EMFAC(40,1)=7.3E-04_hp !SAV - Savannah
Inst%GFED4_EMFAC(40,2)=6.1E-04_hp !BORF - Boreal
Inst%GFED4_EMFAC(40,3)=5.2E-04_hp !TEMP - Temperate
Inst%GFED4_EMFAC(40,4)=7.7E-04_hp !DEFO - Tropical
Inst%GFED4_EMFAC(40,5)=1.10E-03_hp !PET -  Peatland
Inst%GFED4_EMFAC(40,6)=1.03E-03_hp !AGW - Crop residue

! Acrolein
GFED4_SPEC_NAME(41)="ACR"
Inst%GFED4_EMFAC(41,1)=4.8E-04_hp
Inst%GFED4_EMFAC(41,2)=3.3E-04_hp
Inst%GFED4_EMFAC(41,3)=3.4E-04_hp
Inst%GFED4_EMFAC(41,4)=6.5E-04_hp
Inst%GFED4_EMFAC(41,5)=2.7E-04_hp
Inst%GFED4_EMFAC(41,6)=6.2E-04_hp

! butenenitriles
!GFED4_SPEC_NAME(42)="BNIT"
!Inst%GFED4_EMFAC(42,1)=1.3E-05_hp !SAV - Savannah
!Inst%GFED4_EMFAC(42,2)=1.4E-04_hp !BORF - Boreal
!Inst%GFED4_EMFAC(42,3)=6.2E-05_hp !TEMP - Temperate
!Inst%GFED4_EMFAC(42,4)=1.2E-04_hp !DEFO - Tropical
!Inst%GFED4_EMFAC(42,5)=0E+01_hp !PET -  Peatland
!Inst%GFED4_EMFAC(42,6)=2.2E-04_hp !AGW - Crop residue

! Styrene
GFED4_SPEC_NAME(43)="STYR"
Inst%GFED4_EMFAC(43,1)=5.6E-05_hp !SAV - Savannah
Inst%GFED4_EMFAC(43,2)=1.3E-04_hp !BORF - Boreal
Inst%GFED4_EMFAC(43,3)=6.6E-05_hp !TEMP - Temperate
Inst%GFED4_EMFAC(43,4)=2.8E-05_hp !DEFO - Tropical
Inst%GFED4_EMFAC(43,5)=5.5E-05_hp !PET -  Peatland
Inst%GFED4_EMFAC(43,6)=4.3E-05_hp !AGW - Crop residue

! Phenol
GFED4_SPEC_NAME(44)="PHEN"
Inst%GFED4_EMFAC(44,1)=4.3E-04_hp !SAV - Savannah
Inst%GFED4_EMFAC(44,2)=7.5E-04_hp !BORF - Boreal
Inst%GFED4_EMFAC(44,3)=2.5E-04_hp !TEMP - Temperate
Inst%GFED4_EMFAC(44,4)=2.3E-04_hp !DEFO - Tropical
Inst%GFED4_EMFAC(44,5)=4.7E-04_hp !PET -  Peatland
Inst%GFED4_EMFAC(44,6)=8.9E-04_hp !AGW - Crop residue

! Methylvinylketone
GFED4_SPEC_NAME(45)="MVK"
Inst%GFED4_EMFAC(45,1)=2.3E-04_hp !SAV - Savannah
Inst%GFED4_EMFAC(45,2)=9.9E-05_hp !BORF - Boreal
Inst%GFED4_EMFAC(45,3)=1.65E-04_hp !TEMP - Temperate
Inst%GFED4_EMFAC(45,4)=3.9E-04_hp !DEFO - Tropical
Inst%GFED4_EMFAC(45,5)=5.7E-05_hp !PET -  Peatland
Inst%GFED4_EMFAC(45,6)=4.8E-04_hp !AGW - Crop residue

!EOC
