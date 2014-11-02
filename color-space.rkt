(module color-space racket
  (struct RGB (r g b) #:transparent)
  (struct RGB-linear (r g b) #:transparent)
  (struct XYZ (x y z) #:transparent)
  (struct LAB (l a b) #:transparent)

  (provide LAB RGB)

  (define (->string c)
    (format "rgb(~a,~a,~a)" (RGB-r c) (RGB-g c) (RGB-b c)))
  (provide ->string)

  (define (add-LAB c1 c2)
    (LAB (+ (LAB-l c1) (LAB-l c2))
         (+ (LAB-a c1) (LAB-a c2))
         (+ (LAB-b c1) (LAB-b c2))))

  (define D65-XYZ (XYZ 95.047 100.0 108.883))

  (define (LAB->XYZ in)
    (local
      [(define whitepoint D65-XYZ)
       (define (f t)
         (if (> (* t t t) 0.008856) (* t t t) (/ (- t 16/116) 7.787)))
       (define Y (/ (+ (LAB-l in) 16) 116))
       (define X (+ (/ (LAB-a in) 500) Y))
       (define Z (- Y (/ (LAB-b in) 200)))]
      (XYZ (* (XYZ-x whitepoint) (f X))
           (* (XYZ-y whitepoint) (f Y))
           (* (XYZ-z whitepoint) (f Z)))))

  (define (XYZ->RGB-linear in)
    (local
      [(define X (/ (XYZ-x in) 100))
       (define Y (/ (XYZ-y in) 100))
       (define Z (/ (XYZ-z in) 100))]
       (RGB-linear (+ (* 3.2406 X) (* -1.5372 Y) (* -0.4986 Z))
                   (+ (* -0.9689 X) (* 1.8758 Y) (* 0.0415 Z))
                   (+ (* 0.0557 X) (* -0.2040 Y) (* 1.0570 Z)))))

  (define (delinearize-RGB in)
    (local
      [(define (f x)
        (* 255 (if (> x 0.0031308) (- (* 1.055 (expt x (/ 1 2.4))) 0.055) (* 12.92 x))))
       (define (g x)
        (max 0 (min 255 (inexact->exact (round (f x))))))]
      (RGB (g (RGB-linear-r in)) (g (RGB-linear-g in)) (g (RGB-linear-b in)))))

  (define (LAB->RGB in)
    (delinearize-RGB (XYZ->RGB-linear (LAB->XYZ in))))
  (provide LAB->RGB)
)
