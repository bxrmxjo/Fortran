program PERMUTACOL_MARIO_BERMEJO
	implicit none
	integer ::  i, j,n1, n2, n3, n4, n5, n6, p(60) !En realidad p usa solo 59 componentes. Es para facilitar lectura
	integer, parameter :: filasc = 103 !Se establece como parametro por si se quiere probar con otros ejemplos
	character(len=1) :: linea(59), C(filasc,59) !objetos de 59 y nfilascx59 caracteres cuya longitud es 1
	9001 format (8X,I2,9(2X,I2))
	
	open(unit = 10, file = "datosPERMUTACOL.txt", status = "old")
	open(unit = 11, file = "respuestasPERMUTACOL.txt")
	! ---------------------------------------------------------
	! Guardamos p en un vector de 60 componentes que solo usara
	! sus primeras 59; rellenando la 60 con un 0 para que se 
	! leyeran mas facilmente los datos del fichero
	! ---------------------------------------------------------
	
	do i = 1,6
		read(10, 9001) (p((i-1)*10 + j), j=1,10) !Guardamos la permutacion en forma de vector
	end do
	
	read(10,*) !Salto de linea que separa la permutacion de la matriz C
	
	do i = 1,filasc
		read(10,'(59A)') (C(i,j), j= 1,59) !Guardamos los caracteres del fichero en C
		write(11, '(59(A1))') (C(i, p(j)), j = 1, 59)  !Escribimos la matriz D en el fichero de salida sin necesidad de crear dicha matriz
	end do


  call contar_elementos(C,filasc,n1,n2,n3,n4,n5,n6)
  !Escribimos los resultados obtenidos
  write(11, '(/,"nblan = ", I4)') n1
  write(11, '("nvoc1 = ", I4)') n2
  write(11, '("nvoc2 = ", I4)') n3
  write(11, '("ncon1 = ", I4)') n4
  write(11, '("ncon2 = ", I4)') n5
  write(11, '("ncif = ", I4)') n6
	
	close(10); close(11)
end program PERMUTACOL_MARIO_BERMEJO


!Subrutina que cuenta los tipos de caracteres de nuestra matriz C
subroutine contar_elementos(C,filasc,n1,n2,n3,n4,n5,n6)
implicit none
integer, intent(in) :: filasc
character, intent(in) :: C(filasc,59)
integer, intent(out) :: n1,n2,n3,n4,n5,n6
integer i,j
character x

n1 = 0; n2 = 0; n3 = 0; n4 = 0; n5 = 0; n6 = 0

!--------------------------------------------------------------
!Puesto que cada caracter tiene su correspondiente representacion 
!numerica en ASCII, podemos realizar comparaciones lógicas, 
!desigualdades e igualdades numericas para contar la cantidad de 
!cada tipo de caracteres que hay en la matriz C
!--------------------------------------------------------------
do j = 1,59
	do i = 1,filasc
    x = C(i,j)	!Ahorramos asignaciones
    
    if (x == ' ') then !Espacios en blanco
        n1 = n1 + 1
    
    else if (x == 'a' .or. x == 'e' .or. x == 'i' .or. x == 'o' .or. x == 'u') then !Vocales minusculas
        n2 = n2+ 1
    
    else if (x == 'A' .or. x == 'E' .or. x == 'I' .or. x == 'O' .or. x == 'U') then !Vocales mayusculas
        n3 = n3 + 1
    
    else if (x >= 'a' .and. x <= 'z' .and. (x /= 'a' .and. x /= 'e' .and. x /= 'i' .and. x /= 'o' .and. x /= 'u')) then !Cons. minusculas
        n4 = n4 + 1
    
    else if (x >= 'A' .and. x <= 'Z' .and. (x /= 'A' .and. x /= 'E' .and. x /= 'I' .and. x /= 'O' .and. x /= 'U')) then !Cons. mayusculas
        n5 = n5 + 1
    
    else if (x >= '0' .and. x <= '9') then !Digitos
        n6 = n6 + 1
        
 		end if
	end do
end do

end subroutine contar_elementos
