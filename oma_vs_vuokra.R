############ JUOKSUVAKIOT ############

plotting=T
viewing=T
debugging=T
timing=T

############ VAKIOT ############

sijoituksen_oletustuotto = 0.075
inflaatio = 0.025
asuntojen_hintakehitys = 0.02
lainan_korko = 0.03
palkan_kehitys = 0.02
vuokran_kehitys = 0.025
po_tulovero = 0.3
oma_paaoma = 0.1
laina_aika = 40
nettopalkka = 2500
muut_kulut = 500
asunnon_hinta = 300000

asuntolainan_korkovahennys = 0.7

############ LASKETUT ############ 

alkupaaoma = asunnon_hinta * oma_paaoma
lainan_maara = asunnon_hinta - alkupaaoma

############ LAHTOTILANNE ############ 

# omistusasunto

omistusasunto_vuokra = 0
omistusasunto_sahko = -70
omistusasunto_vesi = -20
omistusasunto_vastike = -130
omistusasunto_sahko_vesi_vastike = sum( omistusasunto_sahko
                                        , omistusasunto_vesi
                                        , omistusasunto_vastike)
omistusasunto_korkokustannukset = - lainan_korko / 12 * lainan_maara

omistusasunto_yhteensa = sum( omistusasunto_vuokra
                              , omistusasunto_sahko
                              , omistusasunto_vesi
                              , omistusasunto_vastike
                              , omistusasunto_korkokustannukset )

# http://www.math.jyu.fi/matyl/peruskurssi/talousmatematiikkaa/korkor3.8.htm

pmt = function( lainan_korko = lainan_korko
                , laina_aika = laina_aika
                , asunnon_hinta = asunnon_hinta){
   osoittaja = (1+lainan_korko)^laina_aika * lainan_korko * asunnon_hinta
   nimittaja = (1+lainan_korko)^laina_aika - 1
   return(-osoittaja/nimittaja / 12)
}

omistusasunto_lainan_tasoera_osoittaja = (1+lainan_korko)^laina_aika * lainan_korko * asunnon_hinta
omistusasunto_lainan_tasoera_nimittaja = (1+lainan_korko)^laina_aika - 1
omistusasunto_lainan_tasoera = omistusasunto_lainan_tasoera_osoittaja / omistusasunto_lainan_tasoera_nimittaja / 12
omistusasunto_korko = -lainan_maara * lainan_korko/12

omistusasunto_kustannukset_yht = sum( omistusasunto_sahko
                                      , omistusasunto_vesi
                                      , omistusasunto_vastike
                                      , -omistusasunto_lainan_tasoera
                                      , -muut_kulut )

omistusasunto_lyhennys = -( omistusasunto_lainan_tasoera + omistusasunto_korko )
omistusasunto_korkovahennyksen_tulovaikutus = -asuntolainan_korkovahennys * omistusasunto_korko * po_tulovero
omistusasunto_sijoitus = sum( nettopalkka
                              , korkovahennyksen_tulovaikutus
                              , omistusasunto_kustannukset_yht )
omistusasunto_sijoituksen_tuotto = 0                             
omistusasunto_asunnon_omapaaoma = asunnon_hinta - lainan_maara

# vuokra

vuokra_vuokra = -1200
vuokra_sahko = 0
vuokra_vesi = 0
vuokra_vastike = 0
vuokra_korkokustannukset = 0

vuokra_kk_kustannukset = sum( vuokra_vuokra
                              , vuokra_sahko
                              , vuokra_vesi
                              , vuokra_vastike
                              , vuokra_korkokustannukset )

############ SIMULOINTI ############ 

simuloi_asuminen = function( 
   ### yleiset paramterit
   nettopalkka = nettopalkka
   ### omistusasunto parametrit
   , korkovahennyksen_tulovaikutus = korkovahennyksen_tulovaikutus 
   , omistusasunto_sahko_vesi_vastike =omistusasunto_sahko_vesi_vastike
   , omistusasunto_lainan_tasoera = omistusasunto_lainan_tasoera
   , muut_kulut = muut_kulut 
   , omistusasunto_kustannukset_yht = omistusasunto_kustannukset_yht
   , omistusasunto_asunnon_omapaaoma = omistusasunto_asunnon_omapaaoma 
   , lainan_maara = lainan_maara
   , asunnon_hinta = asunnon_hinta
   , omistusasunto_lyhennys = omistusasunto_lyhennys
   , omistusasunto_korko = omistusasunto_korko 
   , omistusasunto_korkoprosentti = lainan_korko
   , laina_aika = laina_aika
   , palkan_kehitys = palkan_kehitys
   , inflaatio = inflaatio
   , asuntojen_hintakehitys = asuntojen_hintakehitys
   , sijoituksen_oletustuotto = sijoituksen_oletustuotto
   , asuntolainan_korkovahennys = asuntolainan_korkovahennys
   , po_tulovero = po_tulovero 
   ### vuokraparametrit
   , vuokra = vuokra){
   
   vuodet <- rep( seq(1,laina_aika) , each=12 )
   kuukaudet = 1:length(vuodet)
   
   omistusasunto_kertynyt_korko = omistusasunto_korko
   omistusasunto_kertynyt_kustannus = omistusasunto_sahko_vesi_vastike
   omistusasunto_maksettu_yhteensa = omistusasunto_kertynyt_korko + omistusasunto_kertynyt_kustannus
   omistusasunto_maksuera = -omistusasunto_lainan_tasoera
   
   omistusasunto_matriisi = data.frame( matrix(NA , ncol=18 , nrow=length(kuukaudet)) )
   colnames(omistusasunto_matriisi) = c('Vuosi' , 'Kuukausi','Nettopalkka'
                                        ,'Korkovahennyksen_tulovaikutus', 'Sahko_vesi_vastike'
                                        #, 'Maksuera_yht'
                                        ,'Muut_kulut','Kustannukset_yht'
                                        , 'Asunnon_omapaaoma','Lainan_maara','Asunnon_arvo'
                                        , 'Lyhennys','Korko','Kertynyt_korko'
                                        , 'Kertynyt_kustannus' ,'Maksettu_yhteensa'
                                        , 'Sijoitus','Sijoituksen_tuotto','Sijoituksen_arvo')
   
   omistusasunto_matriisi$Vuosi = vuodet
   omistusasunto_matriisi$Kuukausi = kuukaudet
   omistusasunto_matriisi$Nettopalkka[1] = nettopalkka
   omistusasunto_matriisi$Korkovahennyksen_tulovaikutus[1] = korkovahennyksen_tulovaikutus
   omistusasunto_matriisi$Sahko_vesi_vastike[1] = omistusasunto_sahko_vesi_vastike
   #omistusasunto_matriisi$Maksuera_yht = -omistusasunto_lainan_tasoera
   omistusasunto_matriisi$Muut_kulut[1] = muut_kulut
   omistusasunto_matriisi$Kustannukset_yht[1] = omistusasunto_kustannukset_yht
   omistusasunto_matriisi$Asunnon_omapaaoma[1] = omistusasunto_asunnon_omapaaoma
   omistusasunto_matriisi$Lainan_maara[1] = lainan_maara
   omistusasunto_matriisi$Asunnon_arvo[1] = asunnon_hinta
   omistusasunto_matriisi$Lyhennys[1] = omistusasunto_lyhennys
   omistusasunto_matriisi$Korko[1] = omistusasunto_korko
   omistusasunto_matriisi$Kertynyt_korko[1] = omistusasunto_korko
   omistusasunto_matriisi$Kertynyt_kustannus[1] = omistusasunto_kertynyt_kustannus
   omistusasunto_matriisi$Maksettu_yhteensa[1] = omistusasunto_maksettu_yhteensa
   omistusasunto_matriisi$Sijoitus[1] = omistusasunto_sijoitus
   omistusasunto_matriisi$Sijoituksen_tuotto[1] = omistusasunto_sijoituksen_tuotto
   omistusasunto_matriisi$Sijoituksen_arvo[1] = omistusasunto_matriisi$Sijoitus[1]
   
   for( kk in kuukaudet[2:length(kuukaudet)]){
      
      omistusasunto_matriisi$Nettopalkka[kk] = omistusasunto_matriisi$Nettopalkka[kk-1]*(1+palkan_kehitys/12)
      omistusasunto_matriisi$Sahko_vesi_vastike[kk] = omistusasunto_matriisi$Sahko_vesi_vastike[kk-1]*(1+inflaatio/12)
      omistusasunto_matriisi$Lainan_maara[kk] = ifelse(
         omistusasunto_matriisi$Lainan_maara[kk-1] + omistusasunto_matriisi$Lyhennys[kk-1] > 0 
         , omistusasunto_matriisi$Lainan_maara[kk-1] + omistusasunto_matriisi$Lyhennys[kk-1]
         , 0)
      #omistusasunto_matriisi$Maksuera_yht[kk] = omistusasunto_matriisi$Maksettu_yhteensa[kk-1] - pmt(lainan_korko , laina_aika*12 - kk-1 , omistusasunto_matriisi$Lainan_maara[kk] )
      omistusasunto_matriisi$Korko[kk] = - omistusasunto_matriisi$Lainan_maara[kk] * omistusasunto_korkoprosentti / 12
      #omistusasunto_matriisi$Maksuera_yht[kk] = pmt(lainan_korko , laina_aika*12-(kk-1) , omistusasunto_matriisi$Lainan_maara[kk-1] )
      omistusasunto_matriisi$Muut_kulut[kk] = omistusasunto_matriisi$Muut_kulut[kk-1]*( 1+inflaatio / 12 )
      omistusasunto_matriisi$Kustannukset_yht[kk] = sum(omistusasunto_matriisi$Sahko_vesi_vastike[kk]
                                                        , omistusasunto_maksuera
                                                        , -omistusasunto_matriisi$Muut_kulut[kk])
      omistusasunto_matriisi$Asunnon_arvo[kk]  = omistusasunto_matriisi$Asunnon_arvo[kk-1] + omistusasunto_matriisi$Asunnon_arvo[kk-1]*(asuntojen_hintakehitys/12) 
      omistusasunto_matriisi$Korko[kk] = -omistusasunto_matriisi$Lainan_maara[kk] * lainan_korko/12
      omistusasunto_matriisi$Asunnon_omapaaoma[kk] = omistusasunto_matriisi$Asunnon_arvo[kk] - omistusasunto_matriisi$Lainan_maara[kk]
      omistusasunto_matriisi$Korkovahennyksen_tulovaikutus[kk] = ifelse( 
         -asuntolainan_korkovahennys * omistusasunto_matriisi$Korko[kk] * po_tulovero >= 0
         , -asuntolainan_korkovahennys * omistusasunto_matriisi$Korko[kk] * po_tulovero
         , 0)
      omistusasunto_matriisi$Lyhennys[kk] = omistusasunto_maksuera - omistusasunto_matriisi$Korko[kk]
      #omistusasunto_matriisi$Kertynyt_korko[kk] = omistusasunto_matriisi$Kertynyt_korko[kk-1] + omistusasunto_matriisi$Korko[kk]
      omistusasunto_matriisi$Kertynyt_korko[kk] = sum( omistusasunto_matriisi$Korko[1:kk] )
      omistusasunto_matriisi$Kertynyt_kustannus[kk] = omistusasunto_matriisi$Kertynyt_kustannus[kk-1] + omistusasunto_matriisi$Sahko_vesi_vastike[kk]
      omistusasunto_matriisi$Maksettu_yhteensa[kk] = omistusasunto_matriisi$Kertynyt_korko[kk] + omistusasunto_matriisi$Kertynyt_kustannus[kk]
      omistusasunto_matriisi$Sijoitus[kk] = sum(omistusasunto_matriisi$Nettopalkka[kk]
                                                , omistusasunto_matriisi$Korkovahennyksen_tulovaikutus[kk]
                                                , omistusasunto_matriisi$Kustannukset_yht[kk])
      omistusasunto_matriisi$Sijoituksen_tuotto[kk] = omistusasunto_matriisi$Sijoituksen_arvo[kk-1]*sijoituksen_oletustuotto/12
      omistusasunto_matriisi$Sijoituksen_arvo[kk] = sum(omistusasunto_matriisi$Sijoituksen_arvo[kk-1]
                                                        , omistusasunto_matriisi$Sijoitus[kk]
                                                        , omistusasunto_matriisi$Sijoituksen_tuotto[kk])
   }
   
   #omistusasunto_matriisi$Kertynyt_korko = cumsum( omistusasunto_matriisi$Korko )
   
   return(omistusasunto_matriisi)
}

if(timing){
   aloitusaika = Sys.time()
}

omistusasunto_matriisi = simuloi_asuminen(nettopalkka = nettopalkka 
                                              , korkovahennyksen_tulovaikutus = korkovahennyksen_tulovaikutus 
                                              , omistusasunto_sahko_vesi_vastike =omistusasunto_sahko_vesi_vastike
                                              , omistusasunto_lainan_tasoera = omistusasunto_lainan_tasoera
                                              , muut_kulut = muut_kulut 
                                              , omistusasunto_kustannukset_yht = omistusasunto_kustannukset_yht
                                              , omistusasunto_asunnon_omapaaoma = omistusasunto_asunnon_omapaaoma 
                                              , lainan_maara = lainan_maara
                                              , asunnon_hinta = asunnon_hinta
                                              , omistusasunto_lyhennys = omistusasunto_lyhennys
                                              , omistusasunto_korko = omistusasunto_korko 
                                              , omistusasunto_korkoprosentti = lainan_korko
                                              , laina_aika = laina_aika
                                              , palkan_kehitys = palkan_kehitys
                                              , inflaatio = inflaatio
                                              , asuntojen_hintakehitys = asuntojen_hintakehitys
                                              , sijoituksen_oletustuotto = sijoituksen_oletustuotto
                                              , asuntolainan_korkovahennys = asuntolainan_korkovahennys
                                              , po_tulovero = po_tulovero)

if(timing){
   paattymisaika = Sys.time()
   cat( paattymisaika - aloitusaika  )
}
if(viewing){
   View(omistusasunto_matriisi)
}
if(plotting){
   plot(omistusasunto_matriisi$Asunnon_arvo)
}






