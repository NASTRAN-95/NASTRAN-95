
SUBROUTINE biotsv(Xx,Yy,Zz,Hcx,Hcy,Hcz)
   IMPLICIT NONE
   INTEGER Buf2 , Hest , Iout , Ist , Iz(1) , Load , Mcore , Ng1 , Ng2 , Nslt , Ntot , Scr1 , Subcas
   REAL Remfl , Sysbuf , X1 , X2 , Y1 , Y2 , Z(1) , Z1 , Z2
   COMMON /biot  / Ng1 , Ng2 , Ist , Subcas , X1 , Y1 , Z1 , X2 , Y2 , Z2 , Buf2 , Remfl , Mcore , Load , Nslt , Scr1 , Hest , Ntot
   COMMON /system/ Sysbuf , Iout
   COMMON /zzzzzz/ Z
   REAL Hcx , Hcy , Hcz , Xx , Yy , Zz
   REAL alls , buf(50) , fac , factor , hc(3) , hc1(3) , hc2(3) , hca , hcb , hcc , ratio , tlen , xlen
   INTEGER bgpdt , file , i , ibuf(50) , ic , ido , ing1 , ing2 , isimp , isimp1 , isub , j , k , ktype , ltype , mcb(7) , mwords , &
         & n3 , nam(2) , nc , ncards , ngrids , nobld , nrowsp , ns , nsimp
!
!     THIS ROUTINE COMPUTES THE MAGNETIC FIELD AT A POINT (XX,YY,ZZ)
!     DUE TO MAGNETIC SOIRCES. THE ROUTINE IS USED BY PROLATE IN
!     COMPUTING HC POTENTIALS USING LINE INTEGRALS. AT Z(IST) IS STORED
!     LOAD INFO. NEEDED FOR THIS SUBCASE (WHICH COULD BE A LOAD
!     COMBINATION) AS STORED BY ROUTINE LOADSU. THE INFO. IS STORED AS
!     FOLLOWS -
!
!     OVERALL SCALE FACTOR - ALLS
!     NUMBER OF SIMPLE LOADS - NSIMP
!     SCALE FACTOR FOR 1ST SIMPLE LOAD
!     NUMBER OF LOAD CARDS FOR 1ST SIMPLE LOAD
!     SCALE FACTOR FOR 2ND SIMPLE LOAD
!     NUMBER OF LOAD CARDS FOR 2ND SIMPLE LOAD
!      .
!     ETC.
!      .
!     TYPE(NOBLD) OF 1ST CARD FOR 1ST SIMPLE LOAD
!     NUMBER OF CARDS FOR THIS TYPE - IDO
!     LOAD INFO FOR THIS TYPE FOR 1ST SIMPLE LOAD
!     ANOTHER TYPE FOR 1ST SIMPLE LOAD
!      .
!     ETC
!      .
!     LOAD CARDS FOR SUBSEQUENT SIMPLE LOADS FOR THIS SUBCASE
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (buf(1),ibuf(1))
   DATA nam/4HBIOT , 4HSV  /
!
   Hcx = 0.
   Hcy = 0.
   Hcz = 0.
   Scr1 = 301
   bgpdt = 103
   mcb(1) = bgpdt
   CALL rdtrl(mcb)
   nrowsp = mcb(2)
   mcb(1) = Scr1
   CALL rdtrl(mcb)
   n3 = mcb(3)
   ngrids = n3/3
!
   alls = Z(Ist+1)
   nsimp = Iz(Ist+2)
   isimp = Ist + 2*nsimp + 2
!
!     LOOP ON NUMBER OF SIMPLE LOADS
!
   DO ns = 1 , nsimp
      nc = 0
      hc(1) = 0.
      hc(2) = 0.
      hc(3) = 0.
!
      factor = Z(Ist+2*ns+1)
      ncards = Iz(Ist+2*ns+2)
      DO
         nobld = Iz(isimp+1)
         ido = Iz(isimp+2)
         isimp = isimp + 2
!
!
         ktype = nobld - 19
         IF ( ktype==2 ) THEN
            mwords = 12
         ELSEIF ( ktype==3 ) THEN
            mwords = 48
         ELSEIF ( ktype==4 ) THEN
            mwords = 9
         ELSEIF ( ktype==5 ) THEN
            mwords = 0
         ELSE
            mwords = 3*nrowsp
         ENDIF
!
         DO j = 1 , ido
!
            IF ( ktype==2 .OR. ktype==3 .OR. ktype==4 ) THEN
!
!     CEMLOOP,GEMLOOP,MDIPOLE
!
               DO k = 1 , mwords
                  buf(k) = Z(isimp+k)
               ENDDO
               ltype = ktype - 1
               IF ( ltype==2 ) THEN
                  CALL geloop(buf,ibuf,Xx,Yy,Zz,hca,hcb,hcc)
               ELSEIF ( ltype==3 ) THEN
                  CALL dipole(buf,ibuf,Xx,Yy,Zz,hca,hcb,hcc)
               ELSE
                  CALL axloop(buf,ibuf,Xx,Yy,Zz,hca,hcb,hcc)
               ENDIF
!
               hc(1) = hc(1) + hca
               hc(2) = hc(2) + hcb
               hc(3) = hc(3) + hcc
               GOTO 30
            ELSEIF ( ktype==5 ) THEN
!
!     REMFLUX - BRING IN VALUES FROM SCR1 AFTER POSITIONING TO PROPER
!     CASE
!
               CALL gopen(Scr1,Z(Buf2),0)
               ic = Subcas - 1
               IF ( ic/=0 ) THEN
                  DO i = 1 , ic
                     CALL fwdrec(*200,Scr1)
                  ENDDO
               ENDIF
!
               isimp1 = 6*ngrids + Ntot
               CALL fread(Scr1,Z(isimp1+1),n3,1)
!
               CALL close(Scr1,1)
!
!     MUST MATCH NG1 AND NG2 TO SIL-S IN CORE TO LOCATE REMFLUX INFO ON
!    SCR1
!
               ing1 = 0
               ing2 = 0
               DO i = 1 , ngrids
                  IF ( Ng1==Iz(i) ) THEN
                     ing1 = i
                     IF ( ing2/=0 ) GOTO 20
                  ELSEIF ( Ng2==Iz(i) ) THEN
                     ing2 = i
                     IF ( ing1/=0 ) GOTO 20
                  ENDIF
               ENDDO
               GOTO 100
            ELSE
!
!     SPCFLD DATA STARTS AT Z(ISIMP+1)
!
!
!
!     NG1 AND NG2 ARE THE SIL NUMBERS OF THE END POINTS OF THE LINE
!     INTEGRL WITH (X1,Y1,Z1) AND (X2,Y2,Z2) BEING THE COORDINATES.
!     LINEARLY INTERPOLATE TO (XX,YY,ZZ). THE SILS ARE POINTERS INTO
!     THE SPCFLD DATA
!
               isub = isimp + 3*Ng1
               hc1(1) = Z(isub-2)
               hc1(2) = Z(isub-1)
               hc1(3) = Z(isub)
               isub = isimp + 3*Ng2
               hc2(1) = Z(isub-2)
               hc2(2) = Z(isub-1)
               hc2(3) = Z(isub)
            ENDIF
 10         tlen = sqrt((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
            xlen = sqrt((Xx-X1)**2+(Yy-Y1)**2+(Zz-Z1)**2)
            ratio = xlen/tlen
            hc(1) = hc(1) + (1.-ratio)*hc1(1) + ratio*hc2(1)
            hc(2) = hc(2) + (1.-ratio)*hc1(2) + ratio*hc2(2)
            hc(3) = hc(3) + (1.-ratio)*hc1(3) + ratio*hc2(3)
            GOTO 30
 20         isub = 3*ing1 + isimp1
            hc1(1) = Z(isub-2)
            hc1(2) = Z(isub-1)
            hc1(3) = Z(isub)
            isub = 3*ing2 + isimp1
            hc2(1) = Z(isub-2)
            hc2(2) = Z(isub-1)
            hc2(3) = Z(isub)
!
!     INTERPOLATE AS WITH SPCFLD
!
            GOTO 10
!
!     DONE FOR ONE CARD OF PRESENT TYPE  - GET ANOTHER
!
 30         isimp = isimp + mwords
            nc = nc + 1
!
         ENDDO
!
!     CHECK TO SEE IF WE ARE DONE WITH THIS LOAD FACTOR
!
         IF ( nc>=ncards ) THEN
!
!     DONE WITH THIS SIMPLE LOAD. APPLY INDIVIDUAL AND OVERALL SCALE
!     FACTORS THEN GET ANOTHER SIMPLE LOAD
!
            fac = factor*alls
            Hcx = Hcx + fac*hc(1)
            Hcy = Hcy + fac*hc(2)
            Hcz = Hcz + fac*hc(3)
            EXIT
         ENDIF
      ENDDO
!
   ENDDO
!
!     DONE
!
   RETURN
!
 100  WRITE (Iout,99001) Ng1 , Ng2
99001 FORMAT ('0*** LOGIC ERROR, SILS',2I8,' CANNOT BE FOUND IN PROLATE LIST IN BIOTSV')
   CALL mesage(-61,0,0)
!
 200  CALL mesage(-2,file,nam)
END SUBROUTINE biotsv