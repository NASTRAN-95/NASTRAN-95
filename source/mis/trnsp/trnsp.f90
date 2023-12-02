!*==trnsp.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trnsp(Core)
   USE c_machin
   USE c_packx
   USE c_system
   USE c_trnspx
   USE c_unpakx
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Core
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: a
   INTEGER :: i , ibuf , ibuf1 , icol , iii , ij1 , ik , iloop , im1 , ioloop , iover , iparm1 , j , k , km , l , last , lcore , m ,&
            & m2 , n1 , nbrut , ncalat , nl , nrem , nrow , nrow2 , nrowo , nscrth , ntype
   INTEGER , DIMENSION(2) , SAVE :: iparm , name
   INTEGER , DIMENSION(7,8) :: trb1
   REAL , DIMENSION(4) , SAVE :: zero
   EXTERNAL close , conmsg , fname , fwdrec , mesage , open , pack , rewind , sswtch , trnsps , unpack , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     OUT-OF-CORE MATRIX TRANSPOSE USING 1 TO 8 SCRATCH FILES - NASTRAN
!     ORIGINAL ROUTINE.
!
!     (SEE TRANSP FOR IN-CORE MATRIX TRANSPOSE FOR UPPER TRIAG. MATRIX,
!      AND TRNSPS FOR OUT-OF-CORE MATRIX TRANSPOSE WITH 1 SCRATCH FILE,
!      A NASTRAN NEW ROUTINE)
!
!     REVERT TO NASTRAN ORIGINAL TRNSP IF DIAG 41 IS ON, OR 94TH WORD OF
!     /SYSTEM/ IS 1000. OTHERWISE SEND THE TRANSPOSE JOB TO THE NEW
!     TRNSPS ROUTINE, EXECPT LOWER AND UPPER TRIANGULAR MATRICES
!
   DATA iparm/4HTRAN , 4HPOSE/ , zero/4*0.0/
   DATA name/4HTRNS , 4HP   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( nscrh/=8 ) CALL conmsg(iparm,2,0)
         iat(1) = 0
         iat(2) = 0
         incr1 = 1
         ii = 1
         IF ( itypat==0 ) itypat = itypa
         iotyp = min0(itypat,itypa)
         iotypa = iotyp
         iotyp1 = iotyp
         IF ( iforma/=4 .AND. iforma/=5 ) THEN
!               LOWER            UPPER TRIANG. MATRICES
!
            j = mod(ksys94,10000)/1000
            IF ( j/=1 ) THEN
               CALL sswtch(41,j)
               IF ( j/=1 ) THEN
!
!     NASTRAN MAINTENANCE WORK IS DONE ON VAX
!
                  IF ( mach==5 .AND. iforma>=3 .AND. iforma/=6 ) THEN
!               VAX      NOT SQUARE, RECTANG., AND SYMM.
                     CALL fname(namea,a)
                     WRITE (4,99001) a , iforma
99001                FORMAT (40X,'MATRIX ',2A4,', FORM =',I2,' ===>TRNSPS')
                  ENDIF
                  ncolat = 0
                  nscrth = 1
                  CALL trnsps(Core,Core)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
!
         iparm1 = namea
         nscrth = nscrh
         im1 = 1
         ncalat = ncolat
         ncolat = 0
         ij1 = 0
         last = 1
         ntype = iotypa
         IF ( ntype==3 ) ntype = 2
         lcore = lcare
         ibuf1 = lcore - sysbuf
         ibuf = ibuf1 - sysbuf
         lcore = ibuf - 1
         IF ( lcore<=0 ) THEN
            n1 = -8
            CALL mesage(n1,iparm1,name)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     COMMENT FROM G.CHAN/UNISYS    1/91
!     ABOUT THE SQUARE OR RECTANGULAR MATRIX TRANSPOSE BY THE VAX -
!     DATA, 1.0**-10 OR SMALLER, ON THE TRANSPOSED MATRIX MAY DIFFER
!     FROM THE ORIGINAL VALUES. CAN NOT EXPLAIN WHY.
!     THE NORMAL DATA, 1.0**+5 OR LARGER, ARE ALL OK.
!     (NO CHECK ON THE OTHER MACHINES)
!
            nrowo = min0(nrowat,ncola)
            nbrut = lcore/(nrowo*ntype)
            IF ( nbrut==0 ) THEN
               n1 = -8
               CALL mesage(n1,iparm1,name)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               nrem = nbrut
               IF ( nbrut>ncalat ) THEN
!
!     ONE PASS ONLY
!
                  nscrth = 1
                  scrth(1) = namea
                  nbrut = ncalat
                  k = 1
                  ij1 = 1
                  iotyp = itypa
               ELSE
                  k = amax1(float(nrowat)*sqrt(float(ntype)/float(lcore)),1.0)
               ENDIF
               nrow2 = nbrut*k
               nrow = min0(nscrth*nrow2,ncalat)
               km = (ncalat+nrow-1)/nrow
               icol = nbrut*ntype
               IF ( lcore<nrow*ntype+(nscrth-1)*sysbuf ) THEN
                  n1 = -8
                  CALL mesage(n1,iparm1,name)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
!
!     THERE ARE NROW2 ROWS IN EACH SUBMATRIX
!     WE GENERATE NROW ROWS PER PASS OF FULL MATRIX
!     THERE WILL BE KM SUCH PASSES
!
                  ioloop = 1
               ENDIF
            ENDIF
         ENDIF
         DO
            IF ( ij1==0 ) THEN
               IF ( ioloop==km ) THEN
                  iover = ncalat - (km-1)*nrow
                  nbrut = min0(nbrut,iover)
                  icol = nbrut*ntype
                  nrow = iover
                  nrow2 = min0(nbrut*k,nrow)
                  k = (nrow2+nbrut-1)/nbrut
                  nscrth = min0((iover+k*nbrut-1)/(k*nbrut),nscrth)
                  IF ( nscrth==0 ) nscrth = 1
                  nrow1 = ncalat
               ELSE
                  nrow1 = nrow*ioloop
               ENDIF
               is1 = nrow1 - nrow + 1
               IF ( ioloop==1 ) THEN
                  iparm1 = namea
                  CALL open(*20,namea,Core(ibuf1),0)
               ENDIF
               CALL fwdrec(*40,namea)
               nl = nrow*ntype
!
!     OPEN SCRATCHES
!
               j = ibuf
               DO i = 1 , nscrth
                  iparm1 = scrth(i)
                  CALL open(*20,scrth(i),Core(j),1)
                  j = j - sysbuf
                  DO iii = 1 , 7
                     trb1(iii,i) = 0
                  ENDDO
               ENDDO
               DO iloop = 1 , nrowo
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        CALL unpack(*2,namea,Core)
                        spag_nextblock_2 = 2
                     CASE (2)
                        ik = 1
                        jj = nrow2
                        incr = 1
                        DO i = 1 , nscrth
                           CALL pack(Core(ik),scrth(i),trb1(1,i))
                           ik = ik + nrow2*ntype
                        ENDDO
!
!     END LOOP ON BUILDING 1 COL OF SUB MATRICES
!
                        CYCLE
!
 2                      DO i = 1 , nl
                           Core(i) = 0.0
                        ENDDO
                        spag_nextblock_2 = 2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
               CALL rewind(namea)
!
!     END LOOP ON BUILDING NSCRATH SUB MATRICES
!
               DO i = 1 , nscrth
                  CALL close(scrth(i),1)
               ENDDO
            ENDIF
            DO j = 1 , nscrth
               IF ( ij1==0 ) THEN
                  IF ( ioloop==km .AND. j==nscrth ) last = 0
               ENDIF
               SPAG_Loop_3_1: DO m = 1 , k
                  iparm1 = scrth(j)
                  CALL open(*20,scrth(j),Core(ibuf),0)
                  IF ( last==1 .OR. ncalat-ncolat>=nrem ) THEN
                     IF ( ij1/=0 ) CALL fwdrec(*40,scrth(j))
                     is1 = (m-1)*nbrut + 1
                     nrow1 = nbrut*m
                  ELSE
                     nbrut = ncalat - ncolat
                     icol = nbrut*ntype
                     is1 = (m-1)*nrem + 1
                     nrow1 = is1 + nbrut
                  ENDIF
                  l = 1
                  DO i = 1 , nrowo
                     spag_nextblock_3 = 1
                     SPAG_DispatchLoop_3: DO
                        SELECT CASE (spag_nextblock_3)
                        CASE (1)
                           CALL unpack(*4,scrth(j),Core(l))
                           spag_nextblock_3 = 2
                           CYCLE SPAG_DispatchLoop_3
 4                         DO nl = 1 , icol
                              m2 = nl + l - 1
                              Core(m2) = 0.0
                           ENDDO
                           spag_nextblock_3 = 2
                        CASE (2)
                           l = l + icol
                           EXIT SPAG_DispatchLoop_3
                        END SELECT
                     ENDDO SPAG_DispatchLoop_3
                  ENDDO
                  CALL close(scrth(j),1)
                  iparm1 = nameat
                  CALL open(*20,nameat,Core(ibuf),im1)
                  IF ( im1/=3 ) THEN
                     CALL fname(nameat,a(1))
                     CALL write(nameat,a(1),2,1)
                     im1 = 3
                  ENDIF
                  incr = nbrut
                  jj = nrowo
                  DO l = 1 , nbrut
                     m2 = ntype*(l-1) + 1
                     CALL pack(Core(m2),nameat,nameat)
                  ENDDO
                  CALL close(nameat,2)
!
!     END LOOP ON SUBMATRIX
!
                  IF ( ncolat>=ncalat ) EXIT SPAG_Loop_3_1
               ENDDO SPAG_Loop_3_1
!
!     END LOOP ON EACH SCRATCH
!
            ENDDO
!
!     END LOOP ON EACH PASS THROUGH LARGE MATRIX
!
            ioloop = ioloop + 1
            IF ( ioloop>km ) THEN
               iparm1 = nameat
               CALL open(*20,nameat,Core(ibuf),3)
               CALL close(nameat,1)
               CALL close(namea,1)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     ERROR MESSAGES
!
 20      n1 = -1
         CALL mesage(n1,iparm1,name)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      n1 = -2
         CALL mesage(n1,iparm1,name)
         spag_nextblock_1 = 2
      CASE (2)
!
!     ONE FINAL CHECK BEFORE RETURN
!
         IF ( iforma/=3 .AND. iforma/=7 ) THEN
            IF ( ncolat/=nrowa .OR. nrowat/=ncola ) THEN
               CALL fname(namea,a)
               WRITE (otpe,99002) swm , a , iforma , ncola , nrowa , iforat , ncolat , nrowat
99002          FORMAT (A27,' FORM TRNSP. TRANSPOSED MATRIX APPEARS IN ERROR',/5X,'ORIGINAL ',2A4,' - FORM =',I3,',  (',I6,' X',I6,  &
                      &')',/5X,'TRNASPOSED MATRIX - FORM =',I3,',  (',I6,' X',I6,')')
            ENDIF
         ENDIF
         IF ( nscrh/=8 ) CALL conmsg(iparm,2,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE trnsp
