; Tecnológico de Costa Rica
; Administración de Tecnologías de Información
; TI3404 Lenguajes de Programación
; Prof.: Andréi Fuentes Leiva

; III Tarea Programada                 PPPPP   DDDDD   FFFFFF
; Alumnos:                             P    P  D    D  F
;		  Fernanda Fernández           PPPPP   D    D  FFFFF
;		  Melissa Gutiérrez            P       D    D  F
;		  Jean Carlo Alfaro            P       DDDDD   F



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Variables Globles;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *ht* (make-hash-table))
(defparameter *objetos* (make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Clases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass pdf ()
	((nombre-archivo
	:initarg :nombre-archivo
	:initform "")
	(titulo
	:initarg :titulo
	:initform "")
	(autor
	:initarg :autor
	:initform "")
	(fecha-creacion
	:initarg :fecha-creacion
	:initform "")
	(creador
	:initarg :creador
	:initform "")
	(palabras-clave
	:initarg :palabras-clave
	:initform "")))
	
;getters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-nombre-archivo (pdf-a-consultar)
	(slot-value pdf-a-consultar 'nombre-archivo))
	
(defun get-tiulo (pdf-a-consultar)
	(slot-value pdf-a-consultar 'titulo))
	
(defun get-autor (pdf-a-consultar)
	(slot-value pdf-a-consultar 'autor))
	
(defun get-fecha-creacion (pdf-a-consultar)
	(slot-value pdf-a-consultar 'fecha-creacion ))
	
(defun get-creador(pdf-a-consultar)
	(slot-value pdf-a-consultar 'creador))
	
(defun get-palabras-clave (pdf-a-consultar)
	(slot-value pdf-a-consultar 'palabras-clave))

;setters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod set-nombre-archivo (nuevo-nombre nuevo-pdf)
	(setf (slot-value nuevo-pdf 'nombre-archivo) nuevo-nombre))

(defmethod set-titulo (nuevo-titulo nuevo-pdf)
	(setf (slot-value nuevo-pdf 'titulo) nuevo-titulo))

(defmethod set-autor (nuevo-autor nuevo-pdf)
	(setf (slot-value nuevo-pdf 'autor) nuevo-autor))

(defmethod set-fecha-creacion (nueva-fecha nuevo-pdf)
	(setf (slot-value nuevo-pdf 'fecha-creacion) nueva-fecha))

(defmethod set-creador (nuevo-creador nuevo-pdf)
	(setf (slot-value nuevo-pdf 'creador) nuevo-creador))

(defmethod set-palabras-clave (nuevas-palabras nuevo-pdf)
	(setf (slot-value nuevo-pdf 'palabras-clave) nuevas-palabras))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Recorrido de una carpeta;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun recorre(directorio)
	(let((archivo 0))
		(dolist (file (directory (make-pathname :type "pdf" :name :wild :defaults directorio)))
			(setf (gethash archivo *ht*) file)
			(setf archivo (+ archivo 1)))
		(itera *ht*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Recorrido de la TH;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun itera(tabla)
	"Funcion que recorre la Tabla de Hash y despliega el contenido de la misma"
	(loop for k being the hash-keys in tabla using(hash-value value)
		do (lectura(gethash k tabla )k))
	(desplegar-info  *objetos*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Stream de entrada de un archivo;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lectura (file pos) 
	(let ( (in (open file :element-type '(unsigned-byte 8))) (contador 0) nuevo-pdf)
	
		(when in 
			(let ((str (make-string (file-length in))))
				(dotimes (i (file-length in))
					(setf (char str i) (code-char (read-byte in))))

				;(format t "~a~%"(buscar-autor str))
				;(format t "~a~%"(buscar-creador str))
				;(format t "~a~%"(buscar-fecha str))
				;(format t "~a~%"(buscar-titulo str))
				;(format t "~a~%"(buscar-palabras-claves str))
				(setf (gethash pos *objetos*)(crear-objeto "nuevo-pdf" "" (buscar-titulo str) (buscar-autor str) (buscar-fecha str) (buscar-creador str) (buscar-palabras-claves str)) )))))
				
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Inserción de objeto en la TH;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insertar-pdf(objeto indice)
	(setf (gethash indice *objetos*) objeto))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Parseo del Archivo;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun buscar-etiqueta (etiqueta str)
	(search etiqueta str))
	
(defun buscar-titulo(str)
	(let ((titulo ""))
		(if (not(eq(buscar-etiqueta "/Title" str) nil))
			(setf titulo (subseq str( + (buscar-etiqueta "/Title" str) 7)			  
									  ( + (buscar-etiqueta ")" (subseq str(buscar-etiqueta "/Title" str)(-(length str) 1))) (buscar-etiqueta "/Title" str)))))
	(return-from buscar-titulo titulo)))

(defun buscar-autor(str)
	(let ((autor ""))
		(if (not(eq(buscar-etiqueta "/Author" str) nil))
			(setf autor (subseq str( + (buscar-etiqueta "/Author" str) 8 )			  
							( + (buscar-etiqueta ")" (subseq str(buscar-etiqueta "/Author" str)(-(length str) 1))) (buscar-etiqueta "/Author" str)))))
	(return-from buscar-autor autor)))
	
(defun buscar-creador(str)
	(let ((creador ""))
		(if(not(eq(buscar-etiqueta "/Creator" str) nil))
			(setf creador (subseq str( + (buscar-etiqueta "/Creator" str) 11 )  
							  ( + (buscar-etiqueta ")" (subseq str(buscar-etiqueta "/Creator" str)(-(length str) 1))) (buscar-etiqueta "/Creator" str)))))
	(return-from buscar-creador creador)))

(defun buscar-fecha(str)
	(let ((fecha ""))
		(if(not(eq(buscar-etiqueta "/CreationDate" str) nil))
			(setf fecha(subseq str( + (buscar-etiqueta "/CreationDate" str) 16 )
							( +  (buscar-etiqueta "/CreationDate" str) 24))))
	(return-from buscar-fecha fecha)))

;(defun buscar-nombre-archivo(str)
;	(let ((nombre-archivo ""))
;		(if(not(eq(buscar-etiqueta "/Producer" str) nil))
;			(setf archivo(subseq str( + (buscar-etiqueta "/Producer" str) 12 )
;							         ( + (buscar-etiqueta ")" (subseq str(buscar-etiqueta "/Producer" str)(-(length str) 1))) (buscar-etiqueta "/Producer" str)))))
;	(return-from buscar-nombre-archivo nombre-archivo)))

(defun buscar-palabras-claves(str)
	(let ((palabras-claves ""))
		(if(not(eq(buscar-etiqueta "/Keywords" str)nil))
			(setf palabras-claves(subseq str( + (buscar-etiqueta "/Keywords" str) 12 )
							( + (buscar-etiqueta ")" (subseq str(buscar-etiqueta "/Keywords" str)(-(length str) 1))) (buscar-etiqueta "/Keywords" str)))))
	(return-from buscar-palabras-claves palabras-claves)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Creación de un nuevo objeto pdf;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun crear-objeto(nuevo-pdf archivo titulo autor creador fecha claves)
	
	(defparameter nuevo-pdf (make-instance 'pdf))
	
	(set-nombre-archivo archivo nuevo-pdf)
	(set-titulo titulo nuevo-pdf)
	(set-autor autor nuevo-pdf)
	(set-fecha-creacion fecha nuevo-pdf)
	(set-creador creador nuevo-pdf)
	(set-palabras-clave claves nuevo-pdf)
	
	;(format t "~a~%"(get-nombre-archivo nuevo-pdf))
	;(format t "~a~%"(get-nombre-archivo nuevo-pdf))
	;(format t "~a~%"(get-tiulo nuevo-pdf))
	;(format t "~a~%"(get-autor nuevo-pdf))
	;(format t "~a~%"(get-fecha-creacion nuevo-pdf))
	;(format t "~a~%"(get-creador nuevo-pdf))
	;(format t "~a~%"(get-palabras-clave nuevo-pdf))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Recorrido de la TH de PDF's;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun desplegar-info(tabla)
	"Funcion que despliega la informacion de los objetos n la tabla de hash"
	(let ((objeto))
		(loop for j being the hash-keys in tabla using(hash-value value)
;			do(
;		(maphash #'(lambda (k v) (desplegar-info v)) *ht
		
		)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Consultas;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-gen(pdf nombre-archivo)
  (if (STRING-EQUAL genero (slot-value pdf 'nombre-archivo))
    (progn 
 	 (format t (slot-value pdf 'nombre-archivo))
 	 (format t "~%")
 	 (format t (slot-value pdf 'creador))
 	 (format t "~%")
 	 (format t (slot-value pdf 'autor))
 	 (format t "~%")
 	 (format t (slot-value pdf 'titulo)) 
 	 (format t "~%")
	 (format t "~%")
))
)

(defun consulta-nombre-archivo (nombre-archivo) 
  (maphash #'(lambda (k v) (print-gen v nombre-archivo)) *ht*))

(defun print-pdf (pdf)
(progn 
 	 (format t (slot-value pdf 'nombre-archivo))
 	 (format t "~%")
 	 (format t (slot-value pdf 'creador))
 	 (format t "~%")
 	 (format t (slot-value pdf 'autor))
 	 (format t "~%")
 	 (format t (slot-value pdf 'titulo)) 
 	 (format t "~%")
	 (format t "~%")
))

(defun print-ht ()
 (maphash #'(lambda (k v) (print-pdf v)) *ht*))

(defun split-by-$(string)    
    (loop for i = 0 then (1+ j)
          as j = (position #\$ string :start i)
          collect (subseq string i j)
          while j))


 ;;con esto lo prueba (split-by-$(revisar *word* *tag*))
(defun insert (pdf) (setf (gethash(slot-value pdf 'nombre) *ht*) a-mp3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Pruebas;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(recorre "/home/fer/Escritorio/")
;(lectura #P"/home/jealfaro/Escritorio/minuta.pdf")
