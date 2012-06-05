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
	
(defun get-titulo (pdf-a-consultar)
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
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Stream de entrada de un archivo;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lectura (file pos) 
	(let ( (in (open file :element-type '(unsigned-byte 8))) (contador 0) nuevo-pdf)
	
		(when in 
			(let ((str (make-string (file-length in))))
				(dotimes (i (file-length in))
					(setf (char str i) (code-char (read-byte in))))

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
	
	(return-from crear-objeto nuevo-pdf)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Consultas;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun consulta-titulo(titulo)
	(maphash #'(lambda (x y) (cmp-titulo x titulo)) *objetos*))
	
(defun consulta-autor(autor)
	(maphash #'(lambda (x y) (cmp-autor x autor)) *objetos*))
	
(defun consulta-fecha-creacion(fecha-creacion)
	(maphash #'(lambda (x y) (cmp-fecha-creacion x fecha-creacion)) *objetos*))
	
(defun consulta-palabras-clave(palabras-claves)
	(maphash #'(lambda (x y) (cmp-claves x palabras-claves)) *objetos*))

(defun cmp-claves(idx-pdf claves)
  (if (STRING-EQUAL claves (get-palabras-clave (gethash idx-pdf *objetos*)))
    (progn
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'nombre-archivo))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'titulo))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'autor))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'creador)) 
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'fecha-creacion))
	 (format t "~%")
))
)

(defun cmp-titulo(idx-pdf titulo)
  (if (STRING-EQUAL titulo (get-titulo (gethash idx-pdf *objetos*)))
    (progn
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'nombre-archivo))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'titulo))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'autor))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'creador)) 
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'fecha-creacion))
	 (format t "~%")
))
)

(defun cmp-autor(idx-pdf autor)
  (if (STRING-EQUAL autor (get-autor (gethash idx-pdf *objetos*)))
    (progn
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'nombre-archivo))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'titulo))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'autor))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'creador)) 
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'fecha-creacion))
	 (format t "~%")
))
)

(defun cmp-fecha-creacion(idx-pdf fecha-creacion)
  (if (STRING-EQUAL fecha-creacion (get-fecha-creacion (gethash idx-pdf *objetos*)))
    (progn
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'nombre-archivo))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'titulo))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'autor))
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'creador)) 
 	 (format t "~%")
 	 (format t (slot-value (gethash idx-pdf *objetos*) 'fecha-creacion))
	 (format t "~%")
))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Ejecución;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun principio ();funcion para la ejecución inicial
	(format t "Ingrese un directorio: ")
	(let (a) (setq a (read))
	(recorre a)))

(principio);ejecución principal del programa
