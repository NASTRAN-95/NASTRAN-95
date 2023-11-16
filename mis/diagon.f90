
SUBROUTINE diagon
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , Core(20000) , Param(3) , Power , Val(4)
   DOUBLE PRECISION D(2) , Dcore(1) , Dval(2)
   INTEGER Ii , Incru , Itypeu , Iu , Jrow , Ju , Ksystm(60) , Last , Nout , Opt(2) , Sysbuf
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Param
   COMMON /system/ Ksystm
   COMMON /unpakx/ Itypeu , Iu , Ju , Incru
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zblpkx/ Val , Jrow
   COMMON /zntpkx/ A , Ii , Last
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   INTEGER col , i , ia(7) , ib(7) , ib4 , ib5 , ibuf , iform , in1 , incol , inrow , inull , iout , ipow , itype , jju , lcore ,   &
         & name(2) , nowcol , nprec , num , numm , sq
   INTEGER korsz
!
! End of declarations
!
!
!     DMAP FUNCTIONAL MODULE
!
!     DIAGONAL  A / B / V,Y,OPT=COLUMN / V,Y,POWER $
!
!     INPUT  - A IS ANY MATRIX, EXCEPT RECTANGULAR AND ROW VECTOR
!            - OPT IS OUTPUT MATRIX TYPE V,Y FLAG
!            - POWER IS A VALUE TO WHICH THE REAL PART OF EACH ELEMENT
!              ON THE DIAGONAL OF A IS RAISED. (DEFAULT OF POWER IS 1.0)
!     OUTPUT - B IS A REAL SYMMETRIC MATRIX (OPT='SQUARE'), OR A COLUMN
!              VECTOR CONTAINING THE DIAGONAL OF A (OPT='COLUMN'), OR
!              A DIAGONAL MATRIX (OPT='DIAGONAL'
!
!     WRITTEN BY R. MITCHELL, CODE 324, GSFC, DECEMBER 7,1972
!
!     LAST MODIFIED BY G.CHAN/UNISYS   11/1991
!     TO MAKE SUERE  0.0**0 = 1.0, NOT 0.0
!
!ZZ   COMMON /ZZDIAG/  CORE(1)
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout) , (ia(2),incol) , (ia(3),inrow) , (ia(4),iform) , (ia(5),itype) , (A(1),D(1)) ,&
    & (Val(1),Dval(1)) , (Param(1),Opt(1)) , (Param(3),Power) , (Core(1),Dcore(1))
   DATA col , sq/4HCOLU , 4HSQUA/ , in1 , iout/101 , 201/
   DATA name/4HDIAG , 4HONAL/
!
!
!     CHECK FOR VALID PARAMETER.
!
   IF ( Opt(1)/=sq .AND. Opt(1)/=col .AND. Opt(1)/=name(1) ) THEN
      WRITE (Nout,99001) Swm , Opt
99001 FORMAT (A27,' 3300, INVALID PARAMETER ',2A4,' SUPPLIED TO MODULE DIAGONAL, COLUMN SUBSTITUTED')
      Opt(1) = col
   ENDIF
!
!     GET INFO ON INPUT MATRIX
!
   ia(1) = in1
   CALL rdtrl(ia)
!
!     CHECK FOR PURGED INPUT.
!
   IF ( ia(1)<0 ) THEN
!
!     ERROR MESSAGES
!
      RETURN
   ELSE
!
!     CHECK FOR PROPER FORM OF MATRIX
!
      IF ( iform==2 .OR. iform==7 ) THEN
!
!     WRONG TYPE OF INPUT MATRIX = MSG.3016
!
         numm = -16
!
         CALL mesage(numm,num,name)
         GOTO 99999
      ELSEIF ( iform/=8 ) THEN
!
!     SET OUTPUT CONTROL BLOCK TO MATCH INPUT AND REQUESTS.
!
         ib4 = 6
         IF ( Opt(1)==col ) ib4 = 2
         IF ( Opt(1)==name(1) ) THEN
            ib4 = 3
            Opt(1) = col
         ENDIF
         ib5 = 1
         IF ( itype==2 .OR. itype==4 ) ib5 = 2
         CALL makmcb(ib,iout,inrow,ib4,ib5)
!
!     CHECK FOR SPECIAL CASES OF POWER PARAMETER.
!
!     CHECK FOR 1.0 = NO ARITHMETIC REQUIRED.
!
         IF ( abs(Power-1.0)<=1.0E-6 ) THEN
            ipow = 1
!
!     CHECK FOR 0.5 = SQUARE ROOT
!
         ELSEIF ( abs(Power-0.5)<=1.0E-6 ) THEN
            ipow = 2
!
!     CHECK FOR 2.0 = SQUARE
!
         ELSEIF ( abs(Power-2.0)<=1.0E-6 ) THEN
            ipow = 3
!
!     CHECK FOR 0.0 = IDENTITY MATRIX
!
         ELSEIF ( Power/=0 ) THEN
!
!     GENERAL CASE
!
            ipow = 5
         ELSE
            ipow = 4
         ENDIF
!
!     DO OPEN CORE BOOKKEEPING
!
!     OBTAIN LENGTH OF OPEN CORE
!
         lcore = korsz(Core)
!
!     NEED ROOM FOR 2 GINO BUFFERS
!
         IF ( lcore<2*Sysbuf ) THEN
!
!     NOT ENOUGH CORE (MESSAGE 3008)
!
            numm = -8
            CALL mesage(numm,num,name)
            GOTO 99999
!
!     IF INPUT MATRIX IS A DIAGONAL MATRIX, NEED ADDITIONAL
!     ROOM FOR A FULL COLUMN
!
         ELSEIF ( iform==3 .AND. lcore<(2*Sysbuf+ib(5)*inrow+1) ) THEN
            numm = -8
            CALL mesage(numm,num,name)
            GOTO 99999
         ELSE
            ibuf = lcore - Sysbuf + 1
!
!     OPEN INPUT FILE AND SKIP HEADER
!
            CALL gopen(ia,Core(ibuf),0)
!
!     OPEN OUTPUT FILE AND WRITE HEADER
!
            nprec = ib(5)
            ibuf = ibuf - Sysbuf
            CALL gopen(ib,Core(ibuf),1)
!
!     PRIME PACK ROUTINE IF COLUMN OUTPUT
!
            IF ( Opt(1)==col ) CALL bldpk(nprec,nprec,iout,0,0)
!
!     READ INPUT MATRIX AND SEARCH COLUMNS FOR DIAGONAL ELEMENTS.
!
            DO nowcol = 1 , incol
!
!     CHECK IF THE INPUT MATRIX IS A DIAGONAL MATRIX (IFORM = 3)
!
               IF ( iform/=3 ) THEN
!
!     START A NEW COLUMN IF SYMMETRIC OUTPUT MATRIX.
!
                  IF ( Opt(1)==sq ) CALL bldpk(nprec,nprec,iout,0,0)
!WKBI 9/93
                  inull = 0
!
!     START READING A COLUMN
!
!     NOTE THAT NULL INPUT COLUMN RESULTS IN NULL OUTPUT ELEMENT ONLY
!     IF POWER IS NOT ZERO.
!
                  CALL intpk(*15,ia,0,itype,0)
                  DO
!
!     GET AN ELEMENT
!
                     CALL zntpki
!
!     CHECK FOR DESIRED ELEMENT (ROW = COLUMN)
!
                     IF ( Ii<nowcol ) THEN
!
!     CHECK FOR LAST NON-ZERO ELEMENT IN COLUMN.
!
                        IF ( Last>0 ) GOTO 20
                     ELSEIF ( Ii==nowcol ) THEN
                        GOTO 25
                     ELSE
                        GOTO 20
                     ENDIF
                  ENDDO
               ELSE
!
!     UNPACK THE FULL COLUMN OF THE INPUT DIAGONAL MATRIX
!
                  Itypeu = nprec
                  Iu = 1
                  Ju = inrow
                  Incru = 1
                  CALL unpack(*5,ia,Core)
                  Ii = 0
                  GOTO 10
               ENDIF
 5             jju = nprec*Ju
               DO i = 1 , jju
                  Core(i) = 0.0
               ENDDO
               IF ( ipow==4 ) THEN
                  IF ( nprec==1 ) Core(nowcol) = 1.0
                  IF ( nprec==2 ) Dcore(nowcol) = 1.0D0
               ENDIF
               Ii = 0
 10            Ii = Ii + 1
               A(1) = Core(Ii)
               IF ( nprec==2 ) D(1) = Dcore(Ii)
               IF ( Opt(1)==sq ) CALL bldpk(nprec,nprec,iout,0,0)
               GOTO 25
 15            IF ( ipow/=4 ) GOTO 35
!WKBI 9/93
               inull = 1
               Val(2) = 0.0
               Dval(2) = 0.0D0
               IF ( nprec==1 ) Val(1) = 1.0
               IF ( nprec==2 ) Dval(1) = 1.0D0
               Ii = nowcol
               GOTO 30
!
!     SET ELEMENT VALUE TO 0. IF NOT IN COLUMN
!
 20            Val(1) = 0.
               Dval(1) = 0.0D0
               GOTO 30
!
!     PROCESS RETURNED VALUE.
!
!     CHECK FOR PRECISION REQUIRED
!
 25            IF ( nprec==2 ) THEN
!
!     DOUBLE PRECISION PROCESSING OF REAL PART OF DIAGONAL ELEMENT
!
!     PERFORM REQUESTED OPERATION
!
                  IF ( ipow==2 ) THEN
                     Dval(1) = dsqrt(D(1))
                  ELSEIF ( ipow==3 ) THEN
                     Dval(1) = D(1)*D(1)
                  ELSEIF ( ipow==4 ) THEN
                     Dval(1) = 1.0D0
                  ELSEIF ( ipow==5 ) THEN
                     Dval(1) = D(1)**Power
                  ELSE
                     Dval(1) = D(1)
                  ENDIF
!
!     SINGLE PRECISION PROCESSING OF REAL PART OF DIAGONAL ELEMENT
!
!     PERFORM REQUESTED OPERATION
!
               ELSEIF ( ipow==2 ) THEN
                  Val(1) = sqrt(A(1))
               ELSEIF ( ipow==3 ) THEN
                  Val(1) = A(1)*A(1)
               ELSEIF ( ipow==4 ) THEN
                  Val(1) = 1.0
               ELSEIF ( ipow==5 ) THEN
                  Val(1) = A(1)**Power
               ELSE
                  Val(1) = A(1)
               ENDIF
!
!     PACK COMPUTED VALUE INTO OUTPUT MATRIX
!
 30            Jrow = nowcol
               IF ( iform==3 ) Jrow = Ii
               CALL zblpki
!
!     TEST FOR SPECIAL CASE OF DIAGONAL INPUT MATRIX (1 COLUMN).
!
               IF ( iform/=3 ) THEN
!
!     SKIP REST OF INPUT COLUMN IF NOT ON LAST ELEMENT.
!
!WKBI 9/93
                  IF ( inull/=1 ) THEN
                     IF ( Last==0 ) CALL skprec(in1,1)
                  ENDIF
               ENDIF
!WKBI 9/93
!
!     TEST FOR SQUARE MATRIX CASE
!     FINISHED WITH COLUMN IF SQUARE MATRIX
!
 35            IF ( Opt(1)==sq ) CALL bldpkn(iout,0,ib)
!
!     FINISHED WITH ONE OUTPUT ELEMENT.
!
               IF ( iform==3 .AND. Ii<inrow ) GOTO 10
            ENDDO
!
!     FINISH PACKING VECTOR IF COLUMN OUTPUT OPTION.
!
            IF ( Opt(1)==col ) CALL bldpkn(iout,0,ib)
!
!     WRITE TRAILER IN FIAT.
!
            CALL wrttrl(ib)
!
!     FINISHED WITH ALL OF MATRIX, CLOSE UNITS
!
            CALL close(in1,1)
            CALL close(ib,1)
         ENDIF
      ENDIF
      RETURN
   ENDIF
!
99999 RETURN
END SUBROUTINE diagon
