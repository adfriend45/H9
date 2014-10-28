lini = FLTARR (12)
OPENR, 10, 'diag.txt'
READF, 10, NROWS
JYEAR      = FLTARR (NROWS)
Cv         = FLTARR (NROWS)
Aheart     = FLTARR (NROWS)
ib         = FLTARR (NROWS)
ih         = FLTARR (NROWS)
Acrown     = FLTARR (NROWS)
Afoliage   = FLTARR (NROWS)
Dfoliage_p = FLTARR (NROWS)
Dfoliage   = FLTARR (NROWS)
rwidth     = FLTARR (NROWS)
LAI        = FLTARR (NROWS)
NIND_alive = FLTARR (NROWS)
FOR I = 1, NROWS DO BEGIN
  READF, 10, lini
  JYEAR      [I-1] = lini [ 0]
  Cv         [I-1] = lini [ 1]
  Aheart     [I-1] = lini [ 2]
  ib         [I-1] = lini [ 3]
  ih         [I-1] = lini [ 4]
  Acrown     [I-1] = lini [ 5]
  Afoliage   [I-1] = lini [ 6]
  Dfoliage_p [I-1] = lini [ 7]
  Dfoliage   [I-1] = lini [ 8]
  rwidth     [I-1] = lini [ 9]
  LAI        [I-1] = lini [10]
  NIND_alive [I-1] = lini [11]
ENDFOR
CLOSE, 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
orig_device = !d.name
set_plot, 'ps'
device, file='diag.eps',bits_per_pixel=8,color=1,xsize=20, $
        ysize=15, /encapsulated
white     = cgcolor("white"      , !D.Table_Size-1)
black     = cgcolor("black"      , !D.Table_Size-2)
imgcolors = !D.Table_Size-2
loadct, 5, ncolors=imgcolors
!P.Color     =black
!P.Background=white
!P.Charthick = 3
!X.Thick     = 5
!Y.Thick     = 5
;----------------------------------------------------------------------;
!P.MULTI = [0, 3, 3]
plot, JYEAR,Cv, $
      color=black, $
      xtitle='Year', $
      ytitle='Tree wood C (kg)', $
      charsize=1.2, xstyle=1, ystyle=1
PLOT, JYEAR, Aheart,XTITLE='Year',YTITLE='Aheart (m^2)'
PLOT, JYEAR,     ih,XTITLE='Year',YTITLE='ib, ih (m)'
  OPLOT, JYEAR,    ib, LINE=2
  OPLOT, JYEAR, ih-ib, LINE=3
PLOT, JYEAR, Acrown    ,XTITLE='Year',YTITLE='Acrown (m^2)'
PLOT, JYEAR, Afoliage  ,XTITLE='Year',YTITLE='Afoliage (m^2)'
PLOT, JYEAR, Dfoliage  ,XTITLE='Year',YTITLE='Dfoliage (m^2/m^3)'
  OPLOT, JYEAR, Dfoliage_p, LINE=2
PLOT, JYEAR, rwidth    ,XTITLE='Year',YTITLE='Annual ring width (mm/yr)'
PLOT, JYEAR, LAI       ,XTITLE='Year',YTITLE='Leaf Area Index (m^2/m^2)'
PLOT, JYEAR, NIND_alive,XTITLE='Year',YTITLE='No. trees (n)'
device, /close
set_plot, orig_device
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

END