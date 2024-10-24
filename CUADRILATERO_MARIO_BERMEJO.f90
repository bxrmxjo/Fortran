program CUADRILATERO_MARIO_BERMEJO
implicit none
double precision :: x(5), puntos(2,4), beta, area, p, argumento, RST
character(len=100) :: linea, ta
integer :: ios, pos, i

!---------------------------------------------------------
!                    Observaciones: 
!A la hora de escribir sobre el nuevo fichero se han usado
!los datos leidos previamente en vez de la linea completa
!para mostrar que se conoce como usar los distintos
!formatos que ofrece Fortran.
!---------------------------------------------------------


open(unit=10, file='datosCUADRILATERO.txt', iostat=ios) ! Abrimos el archivo
if (ios /= 0) then !Informamos si hubiera error
print*, "Error al abrir el archivo: iostat =", ios
stop
end if

open(unit = 11, file='respuestasCUADRILATERO.txt', iostat=ios) !Abrimos/creamos archivo de salida
if (ios /= 0) then !Informamos si hubiera error
print*, "Error al abrir/crear el archivo: iostat =", ios
stop
end if


! Bucle para leer el archivo de entrada línea por línea
do
  read(10, '(A)', iostat=ios) linea  ! Lee la línea completa y la guarda en "linea"

  if (ios /= 0) then
    if (ios == -1) exit  ! Fin de archivo y se finaliza la ejecucion
    print*, "Error de lectura: iostat =", ios
    stop
  end if
  
  if (index(linea, "Caso") > 0) then !Si se lee "Caso" se ejecuta el bloque
  
    write(11, '("----------------------------------------------------",/,A7)') trim(linea) !Escribimos el caso en el que estamos

    select case (trim(linea)) !Usamos trim() para eliminar espacios en blanco
    
    case ("Caso 1:", "Caso 2:", "Caso 3:")
      
      do i = 1,5
        read(10, "(A7,F18.14)") ta, x(i) !A7 para guardar los nombres de las variables y poder escribirlas en el fichero de salida.
        write(11,'(A6,1X,F18.14)') trim(ta), x(i) !A6 para que quepa "alfa","beta"...
      end do
           
    case ("Caso 4:", "Caso 5:")
      do i = 1,4
        read(10, "(A3,2X, F18.14, 2X, F18.14)") ta, puntos(1,i), puntos(2,i)
        write(11,'(A3,1X,"(",F18.14,", ",F18.14,")")') ta, puntos(1,i), puntos(2,i) !Escribimos los datos leidos
      end do
      do i = 1,4
        x(i) = sqrt(dot_product(puntos(:,mod(i,4)+1)-puntos(:,i) , puntos(:,mod(i,4)+1)-puntos(:,i))) !guardamos el modulo de los vectores que "conforman" el cuadrilatero.
      end do
      RST = dot_product(puntos(:,1) - puntos(:,2),puntos(:,3) - puntos(:,2))/(x(1)*x(2)) 
      x(5) = acos(RST) !RST es alfa
      write(11,'("alfa =",1X,F12.10)') x(5)
      
    end select
    
    
    p = sum(x(1:4))/2 !Calculamos semiperimetro
    argumento = (abs(x(1)**2 + x(2)**2 - 2*x(1)*x(2)*cos(x(5))) - x(3)**2 - x(4)**2)/(-2*x(3)*x(4))
    if (argumento < -1 .OR. argumento > 1) then
      write(11, '("Argumento de acos() = ",F20.14)') argumento
      write(11, '(A10,1X,A7,1X,A63)') 'Acerca del', trim(linea), 'No se corresponde con un cuadrilatero. No se harán los calculos'
    else
      beta = acos(argumento) !La formula de "argumento" se deduce aplicandole dos veces el Th. Coseno a un cuadrilatero cualquiera.
      Area = sqrt((p-x(1))*(p-x(2))*(p-x(3))*(p-x(4)) - x(1)*x(2)*x(3)*x(4) * ((cos((x(5)+beta)/2))**2)) 
      write(11,'(A6,1X,F18.10,/,A6,1X,F18.10)') 'beta =', beta, 'Area =', Area !beta tiene 10 decimales.
    end if 

  end if
end do

! Cerramos los archivos
close(unit=10)
close(unit=11)
end program
