!*==optpx.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE optpx(Dtyp)
   IMPLICIT NONE
   USE C_BLANK
   USE C_GPTA1
   USE C_NAMES
   USE C_OPTPW1
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Dtyp
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: all , blank
   INTEGER :: any , i , i1 , i2 , i3 , i4 , iall , ide , idx , imhere , iret , j , j1 , j2 , l , loc1 , loc2 , m , maxw , n , nde , &
            & nen , nocor , nogo , nwds , nx
   INTEGER , DIMENSION(21) , SAVE :: etp
   INTEGER , DIMENSION(1) :: iy
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(21) :: stor
   EXTERNAL bckrec , eject , eof , mesage , optpx1 , page2 , read , sort , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     PROCESS PLIMIT CARDS INTO ELEMENT SECTIONS THAT MAY BE READ BY
!     OPTP1D
!     MPT ASSUMED PREPOSITIONED TO PLIMIT CARDS.
!
   !>>>>EQUIVALENCE (Stor(1),K(10)) , (Core(1),X(1)) , (X(7),Iy(1))
   DATA etp/21*0/ , all/4HALL / , blank/1H / , name/4H OPT , 4HPX  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         maxw = 0
         iall = 0
         any = 0
         nocor = 0
         nogo = 0
         nx = 1
         ASSIGN 20 TO iret
 20      SPAG_Loop_1_1: DO
!
!     MAKE PRELIMINARY PASS
!
            imhere = 10
            CALL read(*80,*40,Mpt,K,9,0,nwds)
            IF ( K(1)==all ) THEN
!
!     ALL SPECIFIED
!
               iall = iall + 1
            ELSE
               DO i = 1 , Ntypes
                  IF ( Dtyp(i)/=0 ) THEN
                     idx = Incr*(i-1) + 1
                     IF ( Ne(idx)==K(1) ) THEN
                        IF ( Ne(idx+1)==K(2) ) GOTO 25
                     ENDIF
                  ENDIF
               ENDDO
!
!     ILLEGAL ELEMENT TYPE
!
               nogo = nogo + 1
               IF ( nogo<=1 ) THEN
                  CALL page2(-4)
                  WRITE (Outtap,99005) Ufm
               ENDIF
               stor(nx) = K(1)
               stor(nx+1) = K(2)
               nx = nx + 2
               IF ( nx>=20 ) EXIT SPAG_Loop_1_1
               CYCLE
!
!     LEGAL ELEMENT TYPE
!
 25            i = Dtyp(i)
               etp(i) = etp(i) + 1
               any = any + 1
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
         i = eject(2)
         IF ( i/=0 ) THEN
            CALL page2(-2)
            WRITE (Outtap,99005) Ufm
         ENDIF
         WRITE (Outtap,99001) stor
99001    FORMAT (1H0,9X,10(2A4,1X))
         nx = 1
         GOTO iret
!
!     LAST PLIMIT
!
 40      IF ( nx>1 ) THEN
            ASSIGN 60 TO iret
            DO i = nx , 20
               stor(i) = blank
            ENDDO
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CONTINUE PROCESSING LEGAL CARDS UNLESS ANY = 0
!
 60      IF ( any/=0 .OR. iall/=0 ) THEN
            CALL bckrec(Mpt)
            imhere = 130
            CALL read(*80,*100,Mpt,stor(1),3,Noeor,nwds)
!
            loc1 = 1
!
!     START OF OUTPUT LOOP
!
            DO n = 1 , Ntypes
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     ide = Dtyp(n)
                     IF ( ide<=0 ) CYCLE
                     idx = Entry(ide)
                     idx = Incr*(idx-1)
                     nen = 0
                     nde = etp(ide)
                     IF ( nde>0 ) THEN
                        nwds = 0
!
                        imhere = 140
                        DO m = 1 , nde
                           SPAG_Loop_3_2: DO
                              CALL read(*80,*100,Mpt,stor(1),9,Noeor,nwds)
                              IF ( stor(1)==Ne(idx+1) ) THEN
                                 IF ( stor(2)==Ne(idx+2) ) THEN
                                    CALL optpx1(*62,stor,nogo,nen,loc1)
                                    EXIT SPAG_Loop_3_2
                                 ENDIF
                              ENDIF
                           ENDDO SPAG_Loop_3_2
                        ENDDO
                        CALL bckrec(Mpt)
                        imhere = 150
                        CALL read(*80,*100,Mpt,stor(1),3,Noeor,nwds)
                     ENDIF
!
!     CHECK IF ALL SPECIFIED
!
                     IF ( iall>0 ) THEN
                        imhere = 170
                        DO m = 1 , iall
                           SPAG_Loop_3_3: DO
                              CALL read(*80,*100,Mpt,stor(1),9,Noeor,nwds)
                              IF ( stor(1)==all ) THEN
                                 CALL optpx1(*62,stor,nogo,nen,loc1)
                                 EXIT SPAG_Loop_3_3
                              ENDIF
                           ENDDO SPAG_Loop_3_3
                        ENDDO
                        CALL bckrec(Mpt)
                        imhere = 180
                        CALL read(*80,*100,Mpt,stor(1),3,Noeor,nwds)
                     ENDIF
!
!     CONTINUE PROCESSING LEGAL CARDS - SORT ON SECOND WORD
!
                     IF ( nen==0 ) CYCLE
                     CALL sort(0,0,4,2,iy(loc1),nen)
!
!     CHECK SECOND WORD
!
                     i1 = iy(loc1)
                     i2 = iy(loc1+1)
                     i3 = iy(loc1+2)
                     i4 = iy(loc1+3)
                     loc2 = loc1 + nen
                     l = loc2
                     IF ( l+4>Ycor ) nwds = 1
                     nx = nen - 3
                     IF ( nx>=5 ) THEN
                        SPAG_Loop_2_4: DO m = 5 , nx , 4
                           j = loc1 + m - 1
                           j1 = iy(j)
                           j2 = iy(j+1)
!
                           IF ( i1<j1 ) THEN
                              IF ( i2<j1 ) THEN
!
!     CHECK FOR EXPANDING THE THRU
!
                                 IF ( i2==j1-1 ) THEN
                                    IF ( i3==iy(j+2) ) THEN
                                       IF ( i4==iy(j+3) ) THEN
                                         i2 = j2
                                         IF ( m/=nx ) CYCLE
                                         iy(nx) = i1
                                         EXIT SPAG_Loop_2_4
                                       ENDIF
                                    ENDIF
                                 ENDIF
!
!     OUTPUT PLIMIT DATA IN SETS OF 4
!
                                 IF ( nogo<=0 .AND. nwds<=0 ) THEN
                                    iy(l) = i1
                                    iy(l+1) = i2
                                    iy(l+2) = i3
                                    iy(l+3) = i4
                                 ENDIF
                                 l = l + 4
                                 IF ( l+3>Ycor ) nwds = nwds + 4
                                 i1 = j1
                                 i2 = j2
                                 i3 = iy(j+2)
                                 i4 = iy(j+3)
                                 CYCLE
                              ENDIF
                           ENDIF
!
!     OVERLAPPING RANGE ERROR CONDITION
!
                           CALL page2(-2)
                           WRITE (Outtap,99002) Ufm , i1 , i2 , j1 , j2
99002                      FORMAT (A23,' 2291, PLIMIT RANGE INCORRECT FOR',I8,' THRU',I8,' AND',I8,' THRU',I8,'.')
                           i1 = j1
                           i2 = j2
                           nogo = nogo + 1
                        ENDDO SPAG_Loop_2_4
                     ENDIF
!
!     AFTER ELEMENTS THAT MAY BE OPTIMIZED, FLUSH BUFFER.
!
                     IF ( l+3<=Ycor ) THEN
                        iy(l) = iy(nx)
                        iy(l+1) = iy(nx+1)
                        iy(l+2) = iy(nx+2)
                        iy(l+3) = iy(nx+3)
                        l = l + 3
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
!
!     INSUFFICIENT CORE FOR ELEMENTS OF THIS TYPE
!
 62                  CALL page2(-2)
                     nocor = 1
                     nwds = nwds + 3
                     WRITE (Outtap,99003) Ufm , Ne(idx+1) , Ne(idx+2) , nwds
99003                FORMAT (A23,' 2292, INSUFFICIENT CORE FOR PLIMIT DATA, ELEMENT ',2A4,I5,' WORDS SKIPPED.')
                     nogo = nogo + 1
                     spag_nextblock_2 = 2
                  CASE (2)
!
!     WRITE ONTO SCRATCH FILE
!
                     IF ( nogo<=0 ) THEN
                        maxw = max0(l,maxw)
                        stor(1) = ide
                        stor(2) = (l-loc2+1)/4
                        CALL write(Scrth1,stor(1),2,Noeor)
!
!     AFTER ELEMENT TYPE, NUMBER WORDS - WRITE DATA
!
                        CALL write(Scrth1,iy(loc2),l-loc2+1,Nweor)
                     ENDIF
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
!
            ENDDO
!
!     END OF OUTPUT LOOP
!
            CALL eof(Scrth1)
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         IF ( nogo==0 ) Nklw = maxw
         IF ( nogo>0 ) Count = -1
         IF ( nocor/=0 ) Nklw = -64
         RETURN
!
!     ILLEGAL EOF (310), EOR (320)
!
 80      j = -2
         nwds = -222
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 100     j = -3
         spag_nextblock_1 = 4
      CASE (4)
         WRITE (Outtap,99004) imhere , nwds
99004    FORMAT ('  ERROR IN OPTPX.  IMHERE=',I4,',  NWDS=',I6)
         CALL mesage(j,Mpt,name)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99005 FORMAT (A23,' 2290, THE FOLLOWING ILLEGAL ELEMENT TYPES FOUND ON',' PLIMIT CARD')
END SUBROUTINE optpx
