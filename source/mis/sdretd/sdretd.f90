!*==sdretd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdretd(Elid,Ti,Grids)
   IMPLICIT NONE
   USE C_SDR2DE
   USE C_SDR2X2
   USE C_SDRETT
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Elid
   INTEGER , DIMENSION(33) :: Ti
   INTEGER :: Grids
!
! Local variable declarations rewritten by SPAG
!
   REAL :: flag
   INTEGER :: i , id , nwords
   INTEGER , SAVE :: maxwds
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL mesage , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE (CALLED BY -SDR2E-) READS ELEMENT TEMPERATURE
!     DATA FROM A PRE-POSITIONED RECORD
!
!     ELID   = ID OF ELEMENT FOR WHICH DATA IS DESIRED
!     TI     = BUFFER DATA IS TO BE RETURNED IN
!     GRIDS  = 0 IF EL-TEMP FORMAT DATA IS TO BE RETURNED
!            = NO. OF GRID POINTS IF GRID POINT DATA IS TO BE RETURNED.
!     ELTYPE = ELEMENT TYPE TO WHICH -ELID- BELONGS
!     OLDEL  = ELEMENT TYPE CURRENTLY BEING WORKED ON (INITIALLY 0)
!     OLDEID = ELEMENT ID FROM LAST CALL
!     EORFLG = .TRUE. WHEN ALL DATA HAS BEEN EXHAUSTED IN RECORD
!     ENDID  = .TRUE. WHEN ALL DATA HAS BEEN EXHAUSTED WITHIN AN ELEMENT
!              TYPE.
!     BUFFLG = NOT USED
!     ITEMP  = TEMPERATURE LOAD SET ID
!     IDEFT  = NOT USED
!     IDEFM  = NOT USED
!     RECORD = .TRUE. IF A RECORD OF DATA IS INITIALLY AVAILABLE
!     DEFALT = THE DEFALT TEMPERATURE VALUE OR -1 IF IT DOES NOT EXIST
!     AVRAGE = THE AVERAGE ELEMENT TEMPERATURE
!
   DATA name/4HSDRE , 4HTD  / , maxwds/33/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( Oldeid==Elid ) RETURN
         Oldeid = Elid
         IF ( Itemp==0 ) THEN
            DO i = 1 , maxwds
               Ti(i) = 0
            ENDDO
            RETURN
!
         ELSEIF ( .NOT.Record .OR. Eorflg ) THEN
!
!     NO MORE DATA FOR THIS ELEMENT TYPE
!
            Endid = .TRUE.
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( Eltype/=Oldel ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Endid ) THEN
               Endid = .TRUE.
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
!
!     HERE WHEN ELTYPE IS AT HAND AND END OF THIS TYPE DATA
!     HAS NOT YET BEEN REACHED.  READ AN ELEMENT ID
!
            CALL read(*40,*60,Gptt,id,1,0,flag)
            IF ( id==0 ) THEN
               Endid = .TRUE.
               EXIT SPAG_Loop_1_1
            ELSEIF ( iabs(id)==Elid ) THEN
               IF ( id<=0 ) EXIT SPAG_Loop_1_1
!
!     MATCH ON ELEMNT ID MADE AND IT WAS WITH DATA
!
               CALL read(*40,*60,Gptt,Ti,nwords,0,flag)
!
!     IF QUAD4 (ELTYPE 64) OR TRIA3 (ELTYPE 83) ELEMENT, SET FLAG FOR
!     SQUD42 OR STRI32
!
               IF ( Eltype/=64 .OR. Eltype/=83 ) RETURN
               Ti(7) = 13
               IF ( Ti(6)/=1 ) Ti(7) = 2
               RETURN
            ELSEIF ( id>0 ) THEN
               CALL read(*40,*60,Gptt,Ti,nwords,0,flag)
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
!
!     NO DATA FOR ELEMENT ID DESIRED, THUS USE DEFALT
!
         IF ( Defalt==-1 ) THEN
!
!     NO TEMP DATA OR DEFALT
!
            WRITE (Iout,99001) Ufm , Elid , Itemp
99001       FORMAT (A23,' 4016, THERE IS NO TEMPERATURE DATA FOR ELEMENT',I9,' IN SET',I9)
            CALL mesage(-61,0,0)
         ELSEIF ( Grids>0 ) THEN
!
            IF ( Eltype==64 .AND. Eltype==83 ) THEN
!                 QUAD4             TRIA3
               Ti(4) = 0
               Ti(5) = 0
               Ti(6) = 0
               Ti(7) = 0
            ENDIF
            DO i = 1 , Grids
               Ti(i) = Defalt
            ENDDO
            Ti(Grids+1) = Defalt
            RETURN
         ELSE
            DO i = 2 , maxwds
               Ti(i) = 0
            ENDDO
            Ti(1) = Defalt
            IF ( Eltype==34 ) Ti(2) = Defalt
            RETURN
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     LOOK FOR MATCH ON ELTYPE (FIRST SKIP ANY UNUSED ELEMENT DATA)
!
         IF ( .NOT.(Endid) ) THEN
            SPAG_Loop_1_2: DO
               CALL read(*40,*60,Gptt,id,1,0,flag)
               IF ( id<0 ) THEN
               ELSEIF ( id==0 ) THEN
                  EXIT SPAG_Loop_1_2
               ELSE
                  CALL read(*40,*60,Gptt,Ti,nwords,0,flag)
               ENDIF
            ENDDO SPAG_Loop_1_2
         ENDIF
!
!     READ ELTYPE AND COUNT
!
         CALL read(*40,*20,Gptt,Ti,2,0,flag)
         Oldel = Ti(1)
         nwords = Ti(2)
         Endid = .FALSE.
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!     END OF RECORD HIT
!
 20      Eorflg = .TRUE.
         Endid = .TRUE.
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 40      CALL mesage(-2,Gptt,name)
 60      CALL mesage(-3,Gptt,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdretd
