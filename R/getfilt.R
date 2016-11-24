getfilt=function(filter){
  out=NA
  if(filter=='FUV'){data('filt_FUV_GALEX');out=filt_FUV_GALEX}
  if(filter=='NUV'){data('filt_NUV_GALEX');out=filt_NUV_GALEX}
  if(filter=='u'){data('filt_u_SDSS');out=filt_u_SDSS}
  if(filter=='g'){data('filt_g_SDSS');out=filt_g_SDSS}
  if(filter=='r'){data('filt_r_SDSS');out=filt_r_SDSS}
  if(filter=='i'){data('filt_i_SDSS');out=filt_i_SDSS}
  if(filter=='z'){data('filt_z_SDSS');out=filt_z_SDSS}
  if(filter=='Z'){data('filt_Z_VISTA');out=filt_Z_VISTA}
  if(filter=='Y'){data('filt_Y_VISTA');out=filt_Y_VISTA}
  if(filter=='J'){data('filt_J_VISTA');out=filt_J_VISTA}
  if(filter=='H'){data('filt_H_VISTA');out=filt_H_VISTA}
  if(filter=='K' | filter=='Ks'){data('filt_K_VISTA');out=filt_K_VISTA}
  if(filter=='W1'){data('filt_W1_WISE');out=filt_W1_WISE}
  if(filter=='W2'){data('filt_W2_WISE');out=filt_W2_WISE}
  if(filter=='W3'){data('filt_W3_WISE');out=filt_W3_WISE}
  if(filter=='W4'){data('filt_W4_WISE');out=filt_W4_WISE}
  if(filter==100 | filter=='100'){data('filt_P100_Herschel');out=filt_P100_Herschel}
  if(filter==160 | filter=='160'){data('filt_P160_Herschel');out=filt_P160_Herschel}
  if(filter==250 | filter=='250'){data('filt_S250_Herschel');out=filt_S250_Herschel}
  if(filter==350 | filter=='350'){data('filt_S350_Herschel');out=filt_S350_Herschel}
  if(filter==450 | filter=='450'){data('filt_S450_Herschel');out=filt_S450_Herschel}
  colnames(out)<-c('index','Wavelength', 'Trans')
  return=out
}
