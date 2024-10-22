program ejerciciomatrices
	implicit none
	double precision :: resultado, mejor = -1e38 !Inicializamos mejor muy negativo (igual que en submatriz LU)
	integer i, j, fil, col
	double precision :: A(20,10), B(5,5), difcenesq, C(5,5)
	
	
	open(10, file = 'datosmatrices.txt')
	open(11, file = 'respuestasmatrices.txt')
	
	do i = 1,20
		read(10, '(10X,10F8.5)') (A(i,j), j=1,10)
	end do
	
	
	fil = 1; col = 1
	do j = 1,6
		do i = 1,16
			B = A(i:i+4, j:j+4)
			resultado = difcenesq(B)
			if (resultado > mejor) then
				fil = i
				col = j
				mejor = resultado
			end if
		end do
	end do
	
	write(11, '("      ======     ======                                               ============")')
	write(11, '("   i= |",I4,"|  j= |",I4,"|    difcenesq(B) = difcenesq(A(i:i+4,j:j+4)) = |", F10.5,"|")') fil, col, mejor
	write(11, '("      ======     ======                                               ============")')
	
	B = A(fil:fil+4, col:col+4)
	C = B
	do j = 2,9
		C = matmul(C,B)
	end do
	
	write(11, '(/,"=============================")')
	write(11, '("| c(1,1) =",F18.5,"|")') C(1,1)
	write(11, '("| c(2,2) =",F18.5,"|")') C(2,2)
	write(11, '("| c(3,3) =",F18.5,"|")') C(3,3)
	write(11, '("| c(4,4) =",F18.5,"|")') C(4,4)
	write(11, '("| c(5,5) =",F18.5,"|")') C(5,5)
	write(11, '("=============================")')
	
	close(10); close(11)
end program ejerciciomatrices




double precision function difcenesq(S)
	implicit none
	integer, parameter :: n = 5
	double precision, dimension(n,n), intent(in) :: S
	
	difcenesq = S((n+1)/2,(n+1)/2) - (S(1,1) +  S(1,n) + S(n,1) + S(n,n)) 

end function difcenesq


!double precision function mult_mat(S,P) result(T)
!	implicit none
!	integer, parameter :: n = 5
!	integer i, j
!	double precision, dimension(n,n), intent(in) :: S, P
!		
!	do i=1,n
!		do j=1,n
!			T(i,j) = dot_product(S(i,:),P(:,j))
!		end do
!	end do
!	
!end function potencia_matriz
