(in-package "ACL2")

(include-book "arithmetic-5/top" :dir :system)

(defun u8p (x)
  (and (integerp x)
       (<= 0 x)
       (< x 256)))

(defun u8-fix (x)
  (if (u8p x)
      x
    0))

(defun u8-to-n-bits (n x)
  (if (zp n)
      nil
    (cons (oddp x)
	  (u8-to-n-bits (- n 1) (floor x 2)))))

(defun u8-to-bits (x)
  (u8-to-n-bits 8 x))

(defun u8-from-bits (bs)
  (if (endp bs)
      0
    (+ (if (first bs) 1 0)
       (* 2 (u8-from-bits (rest bs))))))

(defun u8-cases (n)
  (if (zp n)
      nil
    (cons `(equal x ,(- n 1))
	  (u8-cases (- n 1)))))

(defmacro defthm-by-u8-cases (name body &rest args)
  `(defthm ,name
     ,body
     ,@args
     :hints (("Goal"
	      :cases ,(u8-cases 256)))))

(defthm-by-u8-cases u8-from-to-bits
  (implies (u8p x)
	   (equal (u8-from-bits (u8-to-bits x))
		  x)))

(defthm u8-to-from-n-bits
  (implies (and (boolean-listp bs)
		(equal (length bs) n))
	   (equal (u8-to-n-bits n (u8-from-bits bs))
		  bs)))

(defthm u8-to-from-bits
  (implies (and (boolean-listp bs)
		(equal (length bs) 8))
	   (equal (u8-to-bits (u8-from-bits bs))
		  bs)))


(defun u8-set-bit (byte bit-index on)
  (u8-from-bits (update-nth bit-index on (u8-to-bits byte))))

(defun u8-get-bit (byte bit-index)
  (nth bit-index (u8-to-bits byte)))

(defthm u8-to-n-bits-len
  (implies (natp n)
	   (equal (len (u8-to-n-bits n x))
		  n)))
(defthm u8-to-bits-len
  (equal (len (u8-to-bits x))
	 8))

(defthm boolean-listp-n-bits
  (boolean-listp (u8-to-n-bits n x))
  :rule-classes (:type-prescription))
(defthm boolean-listp-bits
  (boolean-listp (u8-to-bits x))
  :rule-classes (:type-prescription))

(defthm boolean-list-over-update-nth
  (implies (and (boolean-listp bs)
		(booleanp b))
	   (boolean-listp (update-nth idx b bs))))

(defthm collapse-u8-get-set
  (implies (and (natp bit-index)
		(< bit-index 8)
		(booleanp x))
	   (equal (u8-get-bit (u8-set-bit byte bit-index x)
			      bit-index)
		  x)))

(defthm u8-get-over-set
  (implies (and (natp a) (< a 8)
		(natp b) (< b 8)
		(not (equal a b))
		(booleanp x))
	   (equal (u8-get-bit (u8-set-bit byte b x) a)
		  (u8-get-bit byte a))))

(in-theory (disable u8-get-bit))
(in-theory (disable u8-set-bit))
