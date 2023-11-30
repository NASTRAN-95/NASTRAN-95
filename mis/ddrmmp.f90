
SUBROUTINE ddrmmp(Z,Ncore,Lused,Ixytyp,Icase,Buff,Anyxy) !HIDESTARS (*,Z,Ncore,Lused,Ixytyp,Icase,Buff,Anyxy)
   IMPLICIT NONE
   REAL Cls , Clsrew , Dummy(362) , Rd , Rdrew , Sysbuf , Wrt , Wrtrew
   INTEGER Ierror , Iout
   COMMON /ddrmc1/ Dummy , Ierror
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Sysbuf , Iout
   LOGICAL Anyxy
   INTEGER Icase , Ixytyp , Lused , Ncore
   INTEGER Buff(1) , Z(1)
   INTEGER i , j , loc(6) , noeor , nwds , xycdb
!*****
!  BUILD LIST OF POINTS IN SORT FOR WHICH XYCDB OUTPUT REQUESTS EXIST
!  OF FILE TYPE -IXYTYP- AND OF SUBCASE 0 AND SUBCASE -ICASE-.
!*****
!
!/////
!/////
!
   DATA xycdb/108/ , noeor/0/
!
   Lused = 0
   Anyxy = .FALSE.
   CALL open(*200,xycdb,Buff,Rdrew)
   CALL fwdrec(*100,xycdb)
   CALL fwdrec(*100,xycdb)
   DO
!
!     FIND ENTRIES IN SUBCASE 0 OF THIS TYPE IF ANY.
!
      CALL read(*100,*100,xycdb,loc,6,noeor,nwds)
      IF ( loc(1)>0 ) THEN
         DO
            IF ( loc(1)<Icase ) THEN
!
!     FIND ENTRIES IN SUBCASE -ICASE- OF THIS TYPE IF ANY EXIST.
!
               CALL read(*100,*100,xycdb,loc,6,noeor,nwds)
            ELSEIF ( loc(1)==Icase ) THEN
               IF ( loc(2)<Ixytyp ) THEN
                  CALL read(*100,*100,xycdb,loc,6,noeor,nwds)
               ELSEIF ( loc(2)==Ixytyp ) THEN
                  Lused = Lused + 1
                  IF ( Lused>Ncore ) GOTO 300
                  Z(Lused) = loc(3)
                  CALL read(*100,*100,xycdb,loc,6,noeor,nwds)
               ELSE
                  GOTO 100
               ENDIF
            ELSE
               GOTO 100
            ENDIF
         ENDDO
      ELSEIF ( loc(2)==Ixytyp ) THEN
!
!     SAVE ID IN TABLE
!
         IF ( Lused>0 ) THEN
!
!      ADD TO LIST IF NOT A REPEAT ID
!
            IF ( loc(3)==Z(Lused) ) CYCLE
         ENDIF
         Lused = Lused + 1
         IF ( Lused>Ncore ) GOTO 300
         Z(Lused) = loc(3)
      ENDIF
   ENDDO
!
!     LIST IS NOW COMPLETE THUS SORT IT, AND REMOVE REPEATED IDS.
!
 100  CALL close(xycdb,Clsrew)
   IF ( Lused>0 ) THEN
      CALL sort(0,0,1,1,Z(1),Lused)
      Anyxy = .TRUE.
!
      j = 1
      IF ( Lused/=1 ) THEN
         DO i = 2 , Lused
            IF ( Z(i)/=Z(j) ) THEN
               j = j + 1
               Z(j) = Z(i)
            ENDIF
         ENDDO
      ENDIF
!
      Lused = j
   ENDIF
 200  RETURN
!
!     INSUFFICIENT CORE ALTERNATE RETURN.
!
 300  Ierror = 859
   RETURN 1
END SUBROUTINE ddrmmp