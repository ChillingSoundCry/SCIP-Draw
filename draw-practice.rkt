#lang racket
(require graphics/graphics)
;;;;;;;;;;;;
;;;图的尺寸标注都保持在1*1的框架里。
;;;;;;;;;;;;
;;;画框大小，虽然长宽可以不同，但以下函数主要为方形准备。
(define pic-h 500) ;画框高
(define pic-w 500) ;画框宽
(open-graphics) ;加载绘图函数
(define pap1 (open-viewport "paper-1" pic-h pic-w)) ;设置画框
;;;;;;;;;;;;
;;点操作 make-vect,xcor,ycor
(define (make-vect x y) 
  (make-posn x y))
(define (xcor p) 
  (let ((op '()))
    (cond ((list? p) (set! op car))
          ((posn? p) (set! op posn-x))
          (else (error "Can't identify type!" p)))
    (op p)))
(define (ycor p)
  (let ((op '()))
    (cond ((list? p) (set! op cadr))
          ((posn? p) (set! op posn-y))
          (else (error "Can't identify type!" p)))
    (op p)))
;;;线段操作 make-segment,start-segment,end-segment
(define (make-segment p1 p2)
  (list p1 p2))
(define (start-segment s1)
  (car s1))
(define (end-segment s1)
  (cadr s1))
;;;生成矩形(没用上) make-rectangle,origin,horiz,vert
(define (make-rectangle origin horiz vert)
  (list origin horiz vert))
(define (origin m-r)
  (car m-r))
(define (horiz m-r)
  (cadr m-r))
(define (vert m-r)
  (caddr m-r))
;;;向量操作 +vect,-vect,scale-vect,rotate-vect
(define (+vect v1 v2)
  (make-vect (+ (xcor v1) (xcor v2))
             (+ (ycor v1) (ycor v2))))
(define (scale-vect vect factor)
  (make-vect (* factor (xcor vect))
             (* factor (ycor vect))))
(define (-vect v1 v2)
  (+vect v1 (scale-vect v2 -1)))
(define (rotate-vect v angle)
  (let ((c (cos angle))
        (s (sin angle)))
    (make-vect (- (* c (xcor v))
                  (* s (ycor v)))
               (+ (* c (ycor v))
                  (* s (xcor v))))))
(define pi 3.1415927)
(define (deg->rad deg)
  (* pi (/ deg 180)))
(define (rad->deg rad)
  (* 180 (/ rad pi)))
;;;框架操作
;;绕原点旋转
(define (posn->list posn)
  (list (posn-x posn) (posn-y posn)))
(define (rotate-xframe xframe angle-deg)
  (let ((weiyi (car xframe))
        (f1 (cadr xframe))
        (f2 (caddr xframe))
        (angle-rad (deg->rad angle-deg))
        (p->l (lambda (x) (posn->list x))))
    (list (p->l (+vect '(0.5 0.5) (rotate-vect '(-0.5 -0.5) angle-rad))) 
          (p->l (rotate-vect f1 angle-rad))
          (p->l (rotate-vect f2 angle-rad)))))
(define base-frame '((0 0) (1 0) (0 1)))
;;框架通用操作
;操作有 比例，位移，x/y方向的比例位移等
(define (conver-xframe xframe op para)
  1)


;;;比例函数包括上面的scale-vect,scale-seg,scale-pic
(define (scale-seg segment factor)
  (let ((p1 (start-segment segment))
        (p2 (end-segment segment)))
    (make-segment (scale-vect p1 factor) 
                  (scale-vect p2 factor))))
(define (scale-pic picture factor)
  (define (s-p-help picture result)
    (if (null? picture)
        result
        (s-p-help (cdr picture) 
                  (append result 
                          (list (scale-seg (car picture) factor))))))
  (s-p-help picture '()))
;;;;;;;;;;;
;;;生成小人模型 
;;;图片是形式是(list (posn posn) (posn posn) ....)
(define p1 (make-posn .25 0))
(define p2 (make-posn .35 .5))
(define p3 (make-posn .3 .6))
(define p4 (make-posn .15 .4)) (define p5 (make-posn 0 .65))
(define p6 (make-posn .4 0))
(define p7 (make-posn .5 .3)) (define p8 (make-posn .6 0))
(define p9 (make-posn .75 0))
(define p10 (make-posn .6 .45)) (define p11 (make-posn 1 .15))
(define p12 (make-posn 1 .35))
(define p13 (make-posn .75 .65))
(define p14 (make-posn .6 .65))
(define p15 (make-posn .65 .85)) (define p16 (make-posn .6 1))
(define p17 (make-posn .4 1))
(define p18 (make-posn .35 .85))
(define p19 (make-posn .4 .65))
(define p20 (make-posn .3 .65))
(define p21 (make-posn .15 .6)) (define p22 (make-posn 0 .85))
;1*1方格中的比例
(define ggg
  (list (make-segment p1 p2)
        (make-segment p2 p3)
        (make-segment p3 p4)
        (make-segment p4 p5)
        (make-segment p6 p7)
        (make-segment p7 p8)
        (make-segment p9 p10)
        (make-segment p10 p11)
        (make-segment p12 p13)
        (make-segment p13 p14)
        (make-segment p14 p15)
        (make-segment p15 p16)
        (make-segment p17 p18)
        (make-segment p18 p19)
        (make-segment p19 p20)
        (make-segment p20 p21)
        (make-segment p21 p22)))
;;图形放大pic-w倍
(define g100 (scale-pic ggg pic-w))
;;;;;;;;;;;;;;;
;;;绘图函数 draw-l,show-vect,xf->jf,jf->xf,conver-p,clear
;;绘制线段图
(define (draw-l picture) 
  (define (con-y p)
    (make-vect (xcor p) (- pic-h (ycor p))))
  (let ((dl (draw-line pap1)))
    (define (d-help picture)
      (if (null? picture) 
          'ok
          (begin 
            (dl (con-y (start-segment (car picture)))
                (con-y (end-segment (car picture))))
            (draw-l (cdr picture)))))
    (d-help picture)))

;;显示坐标
(define (show-vect vect)
  (display "(")
  (display (xcor vect))
  (display ",")
  (display (ycor vect))
  (display ")"))

;;框相对坐标转换为绝对坐标和绝对坐标转换为相对坐标
;;框架形式 (weiyi x边 y边) (list (x y) (x y) (x y))
;;似乎已不适用于此需要修改
(define (xf->jf jframe)
  (let ((weiyi (car jframe))
        (f1 (scale-vect (cadr jframe) pic-w))
        (f2 (scale-vect (caddr jframe) pic-w)))
    (list weiyi (+vect weiyi f1) (+vect weiyi f2))))
(define (jf->xf xframe)
  (let ((weiyi (car xframe))
        (f1 (cadr xframe))
        (f2 (caddr xframe))
        (factor (/ 1 pic-h)))
    (list weiyi 
          (scale-vect (-vect f1 weiyi) factor) 
          (scale-vect (-vect f2 weiyi) factor))))

;;;图像变换 lstframe使用相对坐标相量
(define (conver-p picture lstframe)
  (define factor 
    (max pic-h pic-w))
  (define (scale-frame frame factor) 
    (append (list (scale-vect (car frame) factor)) (cdr frame)))
  (define (con-p p weiyi xedge1 xedge2)
    (begin
      ;;;调试输出
      ;(show-vect weiyi)(show-vect xedge1)(show-vect xedge2)
      ;(show-vect (+vect weiyi (+vect xedge1 xedge2))) (newline)
      (+vect weiyi (+vect (scale-vect xedge1 (xcor p))
                          (scale-vect xedge2 (ycor p))))))
  (let* ((lstf (scale-frame lstframe pic-h))
         (weiyi (car lstf))
         (edge1 (cadr lstf))
         (edge2 (caddr lstf)))
    (define (c-p-help picture result)
      (if (null? picture)
          result
          (let* ((p-s1 (car picture))
                 (s-p1 (start-segment p-s1))
                 (s-p2 (end-segment p-s1)))
            (c-p-help (cdr picture) 
                      (append result 
                              (list (make-segment (con-p s-p1 weiyi edge1 edge2)
                                                  (con-p s-p2 weiyi edge1 edge2))))))))
    (c-p-help picture '())))

;;清除画框内线条
(define (clear) ((clear-viewport pap1)))

;;;示例
;(draw-l (conver-p g100 '((0 0) (1 0) (0 1))))
;;;;;;;;;;;
;;;绘图函数2 draw-op,beside,below,flip-vert,flip-horiz,wave2,wave4
;;;right-split,up-split,corner-split,square-limit
(define (draw-op op picture1 . picture2)
  (if (null? picture2) 
      (draw-l (op picture1))
      (draw-l (op picture1 (car picture2)))))
(define (beside wave1 wave2) 
  (append (conver-p wave1 '((0 0) (0.5 0) (0 1)));左
          (conver-p wave2 '((0.5 0) (0.5 0) (0 1)))));右
(define (below wave1 wave2) 
  (append (conver-p wave1 '((0 0.5) (1 0) (0 0.5)));上
          (conver-p wave2 '((0 0) (1 0) (0 0.5)))));下
(define (flip-vert wave) 
  (conver-p wave '((0 1) (1 0) (0 -1)))) ;上下颠倒
(define (flip-horiz wave) 
  (conver-p wave '((1 0) (-1 0) (0 1))));左右翻转
(define (wave2 wave)
  (beside wave (flip-vert wave)))
(define (wave4 wave)
  (below (wave2 wave) (wave2 wave)))
;;左一个大的,右两个四个。。。
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
;;下一个，上两个四个。。。
(define (up-split painter n) 
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below (beside smaller smaller) painter))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below top-left painter )
                  (below corner bottom-right))))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below half (flip-vert half)))))
(define (rotate-90 painter)
  (conver-p painter (rotate-xframe base-frame 90)))
(define (rotate-deg painter degree)
  (conver-p painter (rotate-xframe base-frame degree)))
(define (square-rotate painter n)
  (let ((quarter (corner-split painter n)))
    (let* ((r180 (rotate-deg quarter 180))
          (r90 (rotate-deg quarter 90))
          (r270 (rotate-deg quarter 270)))
      (below (beside r90 quarter) (beside r180 r270)))))
;;示例
(draw-op square-rotate g100 4)
;(draw-l (rotate-90 g100))
;(draw-l (rotate-deg g100 300))
;(draw-l (conver-p g100 '((1 0) (0 1) (-1 0))))
;(draw-l (conver-p g100 '((0 0) (1 0) (0 1))))
;(clear)
;(draw-l (square-limit g100 4))

