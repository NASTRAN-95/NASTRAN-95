
SUBROUTINE optpx(Dtyp)
   IMPLICIT NONE
   INTEGER B1p1 , Count , Entry(21) , Ept , Incr , Iy(1) , K(10) , Kcor , Last , Mpt , Ne(1) , Neltyp , Nklw , Noeor , Npow , Nrd , &
         & Ntypes , Nweor , Nwrt , Outtap , Scrth1 , Stor(21) , Sysbuf , X(7) , Ycor
   REAL Core(1) , Skp1(2) , Skp2(2) , Skp3(4) , Skp5(4)
   CHARACTER*23 Ufm
   COMMON /blank / Skp1 , Count , Skp2 , Ycor , B1p1 , Npow , Skp3 , Nklw , Mpt , Ept , Skp5 , Scrth1 , Neltyp , Entry
   COMMON /gpta1 / Ntypes , Last , Incr , Ne
   COMMON /names / Nrd , Noeor , Nwrt , Nweor
   COMMON /optpw1/ Kcor , K
   COMMON /system/ Sysbuf , Outtap
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Core
   INTEGER Dtyp(1)
   INTEGER all , any , blank , etp(21) , i , i1 , i2 , i3 , i4 , iall , ide , idx , imhere , iret , j , j1 , j2 , l , loc1 , loc2 , &
         & m , maxw , n , name(2) , nde , nen , nocor , nogo , nwds , nx
   INTEGER eject
!
!     PROCESS PLIMIT CARDS INTO ELEMENT SECTIONS THAT MAY BE READ BY
!     OPTP1D
!     MPT ASSUMED PREPOSITIONED TO PLIMIT CARDS.
!
   EQUIVALENCE (Stor(1),K(10)) , (Core(1),X(1)) , (X(7),Iy(1))
   DATA etp/21*0/ , all/4HALL / , blank/1H / , name/4H OPT , 4HPX  /
!
   maxw = 0
   iall = 0
   any = 0
   nocor = 0
   nogo = 0
   nx = 1
   ASSIGN 100 TO iret
 100  DO
!
!     MAKE PRELIMINARY PASS
!
      imhere = 10
      CALL read(*600,*300,Mpt,K,9,0,nwds)
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
                  IF ( Ne(idx+1)==K(2) ) GOTO 120
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
         Stor(nx) = K(1)
         Stor(nx+1) = K(2)
         nx = nx + 2
         IF ( nx>=20 ) EXIT
         CYCLE
!
!     LEGAL ELEMENT TYPE
!
 120     i = Dtyp(i)
         etp(i) = etp(i) + 1
         any = any + 1
      ENDIF
   ENDDO
 200  i = eject(2)
   IF ( i/=0 ) THEN
      CALL page2(-2)
      WRITE (Outtap,99005) Ufm
   ENDIF
   WRITE (Outtap,99001) Stor
99001 FORMAT (1H0,9X,10(2A4,1X))
   nx = 1
   GOTO iret
!
!     LAST PLIMIT
!
 300  IF ( nx>1 ) THEN
      ASSIGN 400 TO iret
      DO i = nx , 20
         Stor(i) = blank
      ENDDO
      GOTO 200
   ENDIF
!
!     CONTINUE PROCESSING LEGAL CARDS UNLESS ANY = 0
!
 400  IF ( any/=0 .OR. iall/=0 ) THEN
      CALL bckrec(Mpt)
      imhere = 130
      CALL read(*600,*700,Mpt,Stor(1),3,Noeor,nwds)
!
      loc1 = 1
!
!     START OF OUTPUT LOOP
!
      DO n = 1 , Ntypes
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
               DO
                  CALL read(*600,*700,Mpt,Stor(1),9,Noeor,nwds)
                  IF ( Stor(1)==Ne(idx+1) ) THEN
                     IF ( Stor(2)==Ne(idx+2) ) THEN
                        CALL optpx1(*420,Stor,nogo,nen,loc1)
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
            CALL bckrec(Mpt)
            imhere = 150
            CALL read(*600,*700,Mpt,Stor(1),3,Noeor,nwds)
         ENDIF
!
!     CHECK IF ALL SPECIFIED
!
         IF ( iall>0 ) THEN
            imhere = 170
            DO m = 1 , iall
               DO
                  CALL read(*600,*700,Mpt,Stor(1),9,Noeor,nwds)
                  IF ( Stor(1)==all ) THEN
                     CALL optpx1(*420,Stor,nogo,nen,loc1)
                     EXIT
                  ENDIF
               ENDDO
            ENDDO
            CALL bckrec(Mpt)
            imhere = 180
            CALL read(*600,*700,Mpt,Stor(1),3,Noeor,nwds)
         ENDIF
!
!     CONTINUE PROCESSING LEGAL CARDS - SORT ON SECOND WORD
!
         IF ( nen==0 ) CYCLE
         CALL sort(0,0,4,2,Iy(loc1),nen)
!
!     CHECK SECOND WORD
!
         i1 = Iy(loc1)
         i2 = Iy(loc1+1)
         i3 = Iy(loc1+2)
         i4 = Iy(loc1+3)
         loc2 = loc1 + nen
         l = loc2
         IF ( l+4>Ycor ) nwds = 1
         nx = nen - 3
         IF ( nx>=5 ) THEN
            DO m = 5 , nx , 4
               j = loc1 + m - 1
               j1 = Iy(j)
               j2 = Iy(j+1)
!
               IF ( i1<j1 ) THEN
                  IF ( i2<j1 ) THEN
!
!     CHECK FOR EXPANDING THE THRU
!
                     IF ( i2==j1-1 ) THEN
                        IF ( i3==Iy(j+2) ) THEN
                           IF ( i4==Iy(j+3) ) THEN
                              i2 = j2
                              IF ( m/=nx ) CYCLE
                              Iy(nx) = i1
                              EXIT
                           ENDIF
                        ENDIF
                     ENDIF
!
!     OUTPUT PLIMIT DATA IN SETS OF 4
!
                     IF ( nogo<=0 .AND. nwds<=0 ) THEN
                        Iy(l) = i1
                        Iy(l+1) = i2
                        Iy(l+2) = i3
                        Iy(l+3) = i4
                     ENDIF
                     l = l + 4
                     IF ( l+3>Ycor ) nwds = nwds + 4
                     i1 = j1
                     i2 = j2
                     i3 = Iy(j+2)
                     i4 = Iy(j+3)
                     CYCLE
                  ENDIF
               ENDIF
!
!     OVERLAPPING RANGE ERROR CONDITION
!
               CALL page2(-2)
               WRITE (Outtap,99002) Ufm , i1 , i2 , j1 , j2
99002          FORMAT (A23,' 2291, PLIMIT RANGE INCORRECT FOR',I8,' THRU',I8,' AND',I8,' THRU',I8,'.')
               i1 = j1
               i2 = j2
               nogo = nogo + 1
            ENDDO
         ENDIF
!
!     AFTER ELEMENTS THAT MAY BE OPTIMIZED, FLUSH BUFFER.
!
         IF ( l+3<=Ycor ) THEN
            Iy(l) = Iy(nx)
            Iy(l+1) = Iy(nx+1)
            Iy(l+2) = Iy(nx+2)
            Iy(l+3) = Iy(nx+3)
            l = l + 3
            GOTO 440
         ENDIF
!
!     INSUFFICIENT CORE FOR ELEMENTS OF THIS TYPE
!
 420     CALL page2(-2)
         nocor = 1
         nwds = nwds + 3
         WRITE (Outtap,99003) Ufm , Ne(idx+1) , Ne(idx+2) , nwds
99003    FORMAT (A23,' 2292, INSUFFICIENT CORE FOR PLIMIT DATA, ELEMENT ',2A4,I5,' WORDS SKIPPED.')
         nogo = nogo + 1
!
!     WRITE ONTO SCRATCH FILE
!
 440     IF ( nogo<=0 ) THEN
            maxw = max0(l,maxw)
            Stor(1) = ide
            Stor(2) = (l-loc2+1)/4
            CALL write(Scrth1,Stor(1),2,Noeor)
!
!     AFTER ELEMENT TYPE, NUMBER WORDS - WRITE DATA
!
            CALL write(Scrth1,Iy(loc2),l-loc2+1,Nweor)
         ENDIF
!
      ENDDO
!
!     END OF OUTPUT LOOP
!
      CALL eof(Scrth1)
   ENDIF
 500  IF ( nogo==0 ) Nklw = maxw
   IF ( nogo>0 ) Count = -1
   IF ( nocor/=0 ) Nklw = -64
   RETURN
!
!     ILLEGAL EOF (310), EOR (320)
!
 600  j = -2
   nwds = -222
   GOTO 800
 700  j = -3
 800  WRITE (Outtap,99004) imhere , nwds
99004 FORMAT ('  ERROR IN OPTPX.  IMHERE=',I4,',  NWDS=',I6)
   CALL mesage(j,Mpt,name)
   GOTO 500
99005 FORMAT (A23,' 2290, THE FOLLOWING ILLEGAL ELEMENT TYPES FOUND ON',' PLIMIT CARD')
END SUBROUTINE optpx
