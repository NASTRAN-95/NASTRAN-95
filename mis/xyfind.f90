
SUBROUTINE xyfind(*,*,*,Majid,Idz)
   IMPLICIT NONE
   REAL Buf(100) , Center , Outopn , Paplot , Plot , Print , Punch , Steps , Tcurve(32) , Xaxis(32) , Yaxis(32) , Ybaxis(32) ,      &
      & Ytaxis(32)
   INTEGER File , Iat , Idin(153) , Idout(300) , Ifile , Ivalue(60) , Knt , Major , Nat , Nbots , Ntops , Subc(5) , Vecid(5) ,      &
         & Vector , Z(1)
   LOGICAL Random
   COMMON /xywork/ File , Tcurve , Ntops , Print , Ifile , Xaxis , Nbots , Plot , Vector , Yaxis , Vecid , Punch , Major , Ytaxis , &
                 & Subc , Center , Random , Ybaxis , Idin , Buf , Ivalue , Iat , Idout , Outopn , Steps , Nat , Paplot , Knt
   COMMON /zzzzzz/ Z
   INTEGER Idz
   INTEGER Majid(11)
   INTEGER eor , flag , isav , itemp , k
   LOGICAL retry
!
   DATA eor/1/
!
!     THIS SUBROUTINE LOCATES THE ID RECORD FOR A PARTICULAR ELEMENT OR
!     POINT ID AND IF THIS IS A RANDOM PLOT IT CONSIDERS THE COMPONENT
!
   k = 1
   retry = .FALSE.
   itemp = Idz
   IF ( Subc(File)>=0 ) THEN
      IF ( Knt<0 ) THEN
         isav = Idin(4)
         DO
            CALL read(*500,*700,Ifile,Idin(1),146,1,flag)
            IF ( isav==Idin(4) ) GOTO 300
            CALL fwdrec(*600,Ifile)
         ENDDO
      ELSEIF ( Knt/=0 ) THEN
         isav = Idin(4)
         DO
            CALL read(*500,*700,Ifile,Idin(1),146,1,flag)
            IF ( Idin(4)/=isav ) GOTO 300
            CALL fwdrec(*600,Ifile)
         ENDDO
      ENDIF
   ENDIF
 100  CALL rewind(Ifile)
   CALL fwdrec(*600,Ifile)
   Vecid(File) = 0
 200  CALL read(*500,*700,Ifile,Idin(1),146,eor,flag)
 300  IF ( Major==Idin(2) ) THEN
      IF ( Subc(File)==0 ) THEN
!
!     MATCH ON MAJOR ID MADE
!
         Vecid(File) = Vector
         GOTO 400
      ELSEIF ( Subc(File)==Idin(4) ) THEN
         Vecid(File) = Vector
         GOTO 400
      ENDIF
   ENDIF
   CALL fwdrec(*600,Ifile)
   k = k + 1
   GOTO 200
 400  IF ( Idin(5)/10/=Z(Idz) ) THEN
      itemp = -1
!
!     IF RANDOM CHECK COMPONENT FOR MATCH
!
   ELSEIF ( .NOT.(Z(Idz+1)/=Idin(6) .AND. Random) ) THEN
      IF ( Subc(File)==0 ) RETURN
      IF ( Subc(File)==Idin(4) ) RETURN
   ENDIF
   CALL fwdrec(*600,Ifile)
   CALL read(*500,*700,Ifile,Idin(1),146,eor,flag)
   IF ( Major==Idin(2) ) GOTO 400
!
!     ELEMENT DATA ARE NOT IN ASCENDING SORT LIKE GRID DATA, BUT ARE
!     SORTED BY ELEMENT NAME, THEN BY ELEMENT NUMBER.
!     SINCE IT IS POSSIBLE FOR THE DESIRED ELEMENT TO BE AHEAD OF THE
!     CURRENT POSITION OF FILE, REWIND AND TRY AGAIN TO FIND MISSING
!     ELEMENT DATA FOR FORCES AND STRESSES.
!
 500  IF ( .NOT.(Knt==0 .OR. retry .OR. Subc(File)==0) ) THEN
      retry = .TRUE.
      GOTO 100
   ELSEIF ( Subc(File)/=0 ) THEN
!
      Vecid(File) = 0
      Idz = itemp
      CALL rewind(Ifile)
      CALL fwdrec(*600,Ifile)
      RETURN 3
   ELSE
      Subc(File) = -1
      RETURN
   ENDIF
!
!     EOF HIT WHEN AN EOF SHOULD NOT HAVE BEEN HIT
!
 600  RETURN 1
!
!     EOR HIT WHEN AN EOR SHOULD NOT HAVE BEEN HIT
!
 700  RETURN 2
!
END SUBROUTINE xyfind