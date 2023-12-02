!*==ddrmmp.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddrmmp(*,Z,Ncore,Lused,Ixytyp,Icase,Buff,Anyxy)
   USE c_ddrmc1
   USE c_names
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Z
   INTEGER :: Ncore
   INTEGER :: Lused
   INTEGER :: Ixytyp
   INTEGER :: Icase
   INTEGER , DIMENSION(1) :: Buff
   LOGICAL :: Anyxy
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , nwds
   INTEGER , DIMENSION(6) :: loc
   INTEGER , SAVE :: noeor , xycdb
   EXTERNAL close , fwdrec , open , read , sort
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!*****
!  BUILD LIST OF POINTS IN SORT FOR WHICH XYCDB OUTPUT REQUESTS EXIST
!  OF FILE TYPE -IXYTYP- AND OF SUBCASE 0 AND SUBCASE -ICASE-.
!*****
!
!/////
!/////
!
   DATA xycdb/108/ , noeor/0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         Lused = 0
         Anyxy = .FALSE.
         CALL open(*40,xycdb,Buff,rdrew)
         CALL fwdrec(*20,xycdb)
         CALL fwdrec(*20,xycdb)
         SPAG_Loop_1_1: DO
!
!     FIND ENTRIES IN SUBCASE 0 OF THIS TYPE IF ANY.
!
            CALL read(*20,*20,xycdb,loc,6,noeor,nwds)
            IF ( loc(1)>0 ) THEN
               DO
                  IF ( loc(1)<Icase ) THEN
!
!     FIND ENTRIES IN SUBCASE -ICASE- OF THIS TYPE IF ANY EXIST.
!
                     CALL read(*20,*20,xycdb,loc,6,noeor,nwds)
                  ELSEIF ( loc(1)==Icase ) THEN
                     IF ( loc(2)<Ixytyp ) THEN
                        CALL read(*20,*20,xycdb,loc,6,noeor,nwds)
                     ELSEIF ( loc(2)==Ixytyp ) THEN
                        Lused = Lused + 1
                        IF ( Lused>Ncore ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        Z(Lused) = loc(3)
                        CALL read(*20,*20,xycdb,loc,6,noeor,nwds)
                     ELSE
                        EXIT SPAG_Loop_1_1
                     ENDIF
                  ELSE
                     EXIT SPAG_Loop_1_1
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
               IF ( Lused>Ncore ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Z(Lused) = loc(3)
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     LIST IS NOW COMPLETE THUS SORT IT, AND REMOVE REPEATED IDS.
!
 20      CALL close(xycdb,clsrew)
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
 40      RETURN
      CASE (2)
!
!     INSUFFICIENT CORE ALTERNATE RETURN.
!
         ierror = 859
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ddrmmp
