
SUBROUTINE sdr2
   IMPLICIT NONE
   INTEGER Acc , Any , Casecc , Displ , Icb(7) , Icstm , Intap , Isopl , Ivec , Ivecn , Knset , Ktype , Kwdcc , Kwdedt , Kwdest ,   &
         & Kwdgpt , Line , Loadnn , Loads , Maxlin , Mcb(7) , Method , Mpcn , Mset , Nam(2) , Ncstm , Nharms , Nogo , Nrigds ,      &
         & Nrings , Plots , Spcf , Vel
   REAL All , Axic , Bk0(2) , Bk1(2) , Branch , Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Cei(2) , Date(3) , Ddrmm , Deform , Ds0(2) ,     &
      & Ds1(2) , Dtype(8) , Echo , Eldef , End , File , Force , Frq(2) , Ocb(7) , Opte , Page , Pla(22) , Rei(2) , Spcn , Sta(2) ,  &
      & Stftmp , Stress , Strspt , Symflg , Symm , Sysbuf , Temp , Time , Tline , Tloads , Trn(2)
   COMMON /sdr2x2/ Casecc
   COMMON /sdr2x4/ Nam , End , Mset , Icb , Ocb , Mcb , Dtype , Icstm , Ncstm , Ivec , Ivecn , Temp , Deform , File , Buf1 , Buf2 , &
                 & Buf3 , Buf4 , Buf5 , Any , All , Tloads , Eldef , Symflg , Branch , Ktype , Loads , Spcf , Displ , Vel , Acc ,   &
                 & Stress , Force , Kwdest , Kwdedt , Kwdgpt , Kwdcc , Nrigds , Sta , Rei , Ds0 , Ds1 , Frq , Trn , Bk0 , Bk1 ,     &
                 & Cei , Pla , Nrings , Nharms , Axic , Knset , Isopl , Strspt , Ddrmm
   COMMON /system/ Sysbuf , Opte , Nogo , Intap , Mpcn , Spcn , Method , Loadnn , Symm , Stftmp , Page , Line , Tline , Maxlin ,    &
                 & Date , Time , Echo , Plots
   INTEGER k
!
!     SDR2 IS THE EXECUTIVE CONTROL PROGRAM FOR THE SDR2 MODULE.
!
!
!     EXECUTE THE PHASES OF SDR2.
!
   Casecc = 101
   CALL sdr2aa
   CALL sdr2a
   IF ( Any/=0 ) CALL sdr2b
   k = Loads + Spcf + Displ + Vel + Acc + Plots
   IF ( k/=0 ) CALL sdr2c
   IF ( Any/=0 ) CALL sdr2d
END SUBROUTINE sdr2