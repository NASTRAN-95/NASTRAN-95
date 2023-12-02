!*==fmdi.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fmdi(I,J)
   IMPLICIT NONE
   USE C_MACHIN
   USE C_SOF
   USE C_SYS
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: I
   INTEGER :: J
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ibl , iblock , icount , k , ll , max , min , ndir , nxtk
   INTEGER , SAVE :: indsbr , ird , iwrt
   LOGICAL :: newblk
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , errmkn , fnxt , getblk , mesage , rshift , sofcls , sofio
!
! End of declarations rewritten by SPAG
!
!
!     THE SUBROUTINE FETCHES FROM THE RANDOM ACCESS STORAGE DEVICE THE
!     BLOCK OF MDI CONTAINING THE I-TH DIRECTORY, AND STORES THAT BLOCK
!     IN THE ARRAY BUF STARTING AT LOCATION (MDI+1) AND EXTENDING TO
!     LOCATION (MDI+BLKSIZ).  IT ALSO RETURNS IN J THE (INDEX-1) OF THE
!     DIRECTORY IN BUF.
!
   DATA ird , iwrt/1 , 2/
   DATA indsbr/7/ , nmsbr/4HFMDI , 4H    /
!
!     NDIR IS THE NUMBER OF DIRECTORIES ON ONE BLOCK OF THE MDI.
!
   CALL chkopn(nmsbr(1))
   ndir = Blksiz/Dirsiz
!
!     COMPUTE THE LOGICAL BLOCK NUMBER, AND THE WORD NUMBER WITHIN
!     BUF IN WHICH THE ITH SUBSTRUCTURE DIRECTORY IS STORED.  STORE THE
!     BLOCK NUMBER IN IBLOCK, AND THE WORD NUMBER IN J.
!
   iblock = I/ndir
   IF ( I/=iblock*ndir ) iblock = iblock + 1
   J = Dirsiz*(I-(iblock-1)*ndir-1) + Mdi
   IF ( Mdilbn==iblock ) RETURN
   IF ( Mdipbn/=0 ) THEN
      IF ( Mdiup ) THEN
!
!     THE MDI BLOCK CURRENTLY IN CORE HAS BEEN UPDATED.  MUST THEREFORE
!     WRITE IT OUT BEFORE READING IN A NEW BLOCK.
!
         CALL sofio(iwrt,Mdipbn,Buf(Mdi-2))
         Mdiup = .FALSE.
      ENDIF
   ENDIF
!
!     THE DESIRED MDI BLOCK IS NOT PRESENTLY IN CORE, MUST THEREFORE
!     FETCH IT.
!
   newblk = .FALSE.
!
!     FIND THE PHYSICAL BLOCK NUMBER OF THE BLOCK ON WHICH THE LOGICAL
!     BLOCK IBLOCK IS STORED.
!
   k = Mdibl
   icount = 1
   DO WHILE ( icount/=iblock )
      icount = icount + 1
      CALL fnxt(k,nxtk)
      IF ( mod(k,2)==1 ) THEN
         ibl = andf(Buf(nxtk),Jhalf)
      ELSE
         ibl = rshift(Buf(nxtk),Ihalf)
      ENDIF
      IF ( ibl==0 ) THEN
!
!     WE NEED A FREE BLOCK FOR THE MDI.
!
         CALL getblk(k,ibl)
         IF ( ibl==-1 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
         newblk = .TRUE.
         k = ibl
         min = Mdi + 1
         max = Mdi + Blksiz
         DO ll = min , max
            Buf(ll) = 0
         ENDDO
         CALL sofio(iwrt,k,Buf(Mdi-2))
      ELSE
         k = ibl
      ENDIF
   ENDDO
   IF ( Mdipbn==k ) THEN
!
!     ERROR IN UPDATING EITHER MDIPBN OR MDILBN.
!
      CALL errmkn(indsbr,6)
   ELSE
!
!     READ THE DESIRED MDI BLOCK INTO CORE.
!
      Mdipbn = k
      Mdilbn = iblock
      IF ( newblk ) RETURN
      CALL sofio(ird,Mdipbn,Buf(Mdi-2))
      RETURN
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     ERROR MESSAGES.
!
      WRITE (Nout,99001) Ufm
99001 FORMAT (A23,' 6223, SUBROUTINE FMDI - THERE ARE NO MORE FREE ','BLOCKS AVAILABLE ON THE SOF.')
      CALL sofcls
      CALL mesage(-61,0,0)
   END SUBROUTINE spag_block_1
END SUBROUTINE fmdi
