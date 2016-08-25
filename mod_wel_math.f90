!****************************************************************************
!
!  PROGRAM: MOD-WEL-Math
!
!  PURPOSE:  new well = a * well(1) + b * well(2)
!
!****************************************************************************
! this script can multiply the pumping rate of the MODFLOW Well package based on the zones
! Format of the master file (a line starting with the single quote will be skipped:
! ZONE FILE
! OLD WELL FILE NAME 1
! OLD WELL FILE NAME 2
! NEW WELL FILE NAME
! fmt_input_1
! fmt_imput_2
! fmt_output
! nper
!''''' for each stress period '''''''
! NZONE (number of zones need to be modified'
! Zone1 Multiplier1a Multiplier1b
! Zone2 Multiplier2a Multiplier2b
! '''''
! ZoneN MultiplierNa MultiplierNb
    
program mod_wel_math

implicit none
integer, parameter :: maxMulZone = 999
character*1024 :: oldWelFile1, oldWelFile2, newWelFile, zonFile, masterFile, dataLine, dataLine1
character*256  :: fmt1, fmt2, fmtout    ! well list input and output formats
integer :: ifile_zon, ifile_newwel, ifile_oldwel1, ifile_oldwel2, ifile_master
integer :: nPER,nLAY,nCOL,nROW,iPER,nMUL, iMUL,iWEL
integer, allocatable, dimension(:,:,:) :: iZONE
integer, allocatable, dimension(:) :: welLAY1, welROW1, welCOL1, welLAY2, welROW2, welCOL2, welLAYnew, welROWnew, welCOLnew
doubleprecision, allocatable, dimension(:) :: welRate1, welRate2, welRateNew
doubleprecision, dimension(0:maxMulZone) :: mulRate1, mulRate2
integer :: zon
doubleprecision :: mul1, mul2
logical :: free1, free2, freeout

!! well variables
integer :: MXACTW1, MXACTW2, IWELCB, ITMP1, ITMP2, ITMP1old, ITMP2old

call get_command_argument(1, masterFile)
! open master file
open(newunit = ifile_master, file = masterFile)

! read file names from master file
call readDataLine(ifile_master, zonFile)
call readDataLine(ifile_master, oldWelFile1)
call readDataLine(ifile_master, oldWelFile2)
call readDataLine(ifile_master, newWelFile)

! open files
open(newunit = ifile_zon,     file = zonFile,     action='read',  status='old')
open(newunit = ifile_oldwel1, file = oldWelFile1, action='read',  status='old')
open(newunit = ifile_oldwel2, file = oldWelFile2, action='read',  status='old')
open(newunit = ifile_newwel,  file = newWelFile,  action='write', status='replace')

! read input and output format for the well list
call readDataLine(ifile_master, fmt1); call UPPERCASE(fmt1)
call readDataLine(ifile_master, fmt2); call UPPERCASE(fmt2)
call readDataLine(ifile_master, fmtout); call UPPERCASE(fmtout)

! check if it's free format
free1 = .false.; free2 = .false.; freeout = .false.
fmt1 = trim(fmt1); fmt2 = trim(fmt2); fmtout = trim(fmtout); 
if (fmt1(1:4) == "FREE") free1 = .true.
if (fmt2(1:4) == "FREE") free2 = .true.
if (fmtout(1:4) == "FREE") freeout = .true.

! read zone file
read(ifile_zon, *) nLAY,nROW,nCOL
if (nLAY>0 .and. nROW>0 .and. nCOL>0) then
 allocate(iZONE(nCOL,nROW,nLAY))
endif
call IZREAD(iZONE,nLAY,nROW,nCOL,ifile_zon)

! read old well header1
call readDataLine(ifile_oldwel1, dataLine); dataLine1 = trim(dataLine)
if (dataLine1(1:9) == "PARAMETER") then
 print *, "not supporting parameter in WELL file 1"
 stop
end if
read(dataLine, *) MXACTW1,IWELCB
if (MXACTW1 <= 0) stop
allocate(welLAY1(MXACTW1), welROW1(MXACTW1), welCOL1(MXACTW1), welRate1(MXACTW1))
mulRate1 = 1.D0

! read old well header2
call readDataLine(ifile_oldwel2, dataLine); dataLine1 = trim(dataLine)
if (dataLine1(1:9) == "PARAMETER") then
 print *, "not supporting parameter in WELL file 2"
 stop
end if
read(dataLine, *) MXACTW2,IWELCB
if (MXACTW2 <= 0) stop
allocate(welLAY2(MXACTW2), welROW2(MXACTW2), welCOL2(MXACTW2), welRate2(MXACTW2))
mulRate2 = 1.D0

! write header of new well file
write(ifile_newwel, "(A)") "# Produced by MOD_WEL_Math program, developed by Michael Ou."
write(ifile_newwel, "(2I10)") MXACTW1 + MXACTW2, IWELCB
! allocate(welLAYNew(MXACTW1 + MXACTW2), welROWNew(MXACTW1 + MXACTW2), welCOLNew(MXACTW1 + MXACTW2), welRateNew(MXACTW1 + MXACTW2)))

! start stress period
call readDataLine(ifile_master, dataLine); read(dataLine, *) nPER 
do iPER = 1, nPER
 ! read multipliers
 read(ifile_master, *) nMUL
 if (nMUL > 0) then 
  mulRate1 = 1.D0; mulRate2 = 1.D0
  do iMUL = 1, nMUL
   read(ifile_master, *) zon, mul1, mul2
   mulRate1(zon) = mul1
   mulRate2(zon) = mul2
  end do
 end if
 
 ! read old well file 1
 call readDataLine(ifile_oldwel1, dataLine)
 read(dataLine, *) ITMP1
 if (ITMP1 > 0) then 
  do iWEL = 1, ITMP1
   if (free1) then
    read(ifile_oldwel1, *)    welLAY1(iWEL), welROW1(iWEL), welCOL1(iWEL), welRate1(iWEL) 
   else
    read(ifile_oldwel1, fmt1) welLAY1(iWEL), welROW1(iWEL), welCOL1(iWEL), welRate1(iWEL) 
   end if
  end do
 end if
 if (ITMP1 < 0) ITMP1 = ITMP1old
 
 ! read old well file 2
 call readDataLine(ifile_oldwel2, dataLine)
 read(dataLine, *) ITMP2
 if (ITMP2 > 0) then 
  do iWEL = 1, ITMP2
   if (free2) then
    read(ifile_oldwel2, *)    welLAY2(iWEL), welROW2(iWEL), welCOL2(iWEL), welRate2(iWEL) 
   else
    read(ifile_oldwel2, fmt2) welLAY2(iWEL), welROW2(iWEL), welCOL2(iWEL), welRate2(iWEL) 
   end if
  end do
 end if
 if (ITMP2 < 0) ITMP2 = ITMP2old
 
 ! write to new well file
 write(ifile_newwel, "(2I10)") ITMP1 + ITMP2, 0
 do iWEL = 1, ITMP1
  zon = iZONE(welCOL1(iWEL),welROW1(iWEL),welLAY1(iWEL))
  if (freeout) then
   write(ifile_newwel,"(3I5,1PE15.7,A,1PE15.7,A,I0)") welLAY1(iWEL), welROW1(iWEL), welCOL1(iWEL), welRate1(iWEL) * mulRate1(zon), '     Original rate:', welRate1(iWEL), '    Well File 1 Zone: ', zon
  else
   write(ifile_newwel,fmtout) welLAY1(iWEL), welROW1(iWEL), welCOL1(iWEL), welRate1(iWEL) * mulRate1(zon)
  end if
 end do
 do iWEL = 1, ITMP2
  zon = iZONE(welCOL2(iWEL),welROW2(iWEL),welLAY2(iWEL))
  if (freeout) then
   write(ifile_newwel,"(3I5,1PE15.7,A,1PE15.7,A,I0)") welLAY2(iWEL), welROW2(iWEL), welCOL2(iWEL), welRate2(iWEL) * mulRate2(zon), '     Original rate:', welRate2(iWEL), '    Well File 2 Zone: ', zon
  else
   write(ifile_newwel,fmtout) welLAY2(iWEL), welROW2(iWEL), welCOL2(iWEL), welRate2(iWEL) * mulRate2(zon)
  end if
 end do
 
 ITMP1old = ITMP1; ITMP2old = ITMP2
end do
close(ifile_newwel)
end program mod_wel_math


subroutine readDataLine(f, line)
character(*), intent(inout) :: line
integer,      intent(in)    :: f
do
 read(f,"(a)") line
 if (line(1:1) /= "#") return
enddo  
end subroutine