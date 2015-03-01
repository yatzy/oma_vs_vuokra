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
tulovero = 0.3
oma_paaoma = 0.1
laina_aika = 40
nettopalkka = 2500
muut_kulut = 500
asunnon_hinta = 300000

asuntolainan_korkovahennys = 0.7

# vuokra

vuokra_vuokra = -1200
vuokra_sahko = 0
vuokra_vesi = 0
vuokra_vastike = 0
alkupaaoma = oma_paaoma*asunnon_hinta

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
omistusasunto_korkovahennyksen_tulovaikutus = -asuntolainan_korkovahennys * omistusasunto_korko * tulovero
omistusasunto_sijoitus = sum( nettopalkka
                              , korkovahennyksen_tulovaikutus
                              , omistusasunto_kustannukset_yht )
omistusasunto_sijoituksen_tuotto = 0                             
omistusasunto_asunnon_omapaaoma = asunnon_hinta - lainan_maara


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
   , tulovero = tulovero 
   ### vuokraparametrit
   , vuokran_kehitys = vuokran_kehitys
   , vuokra_vuokra = vuokra_vuokra
   , vuokra_sahko = vuokra_sahko
   , vuokra_vesi = vuokra_vesi
   , vuokra_vastike = vuokra_vastike
   , alkupaaoma = alkupaaoma ){
   
   vuodet <- rep( seq(1,laina_aika) , each=12 )
   kuukaudet = 1:length(vuodet)
   
   omistusasunto_kertynyt_korko = omistusasunto_korko
   omistusasunto_kertynyt_kustannus = omistusasunto_sahko_vesi_vastike
   omistusasunto_maksettu_yhteensa = omistusasunto_kertynyt_korko + omistusasunto_kertynyt_kustannus
   omistusasunto_maksuera = -omistusasunto_lainan_tasoera
   
   nimet = c('Vuosi' 
             , 'Kuukausi'
             ,'Nettopalkka'
             ,'Korkovahennyksen_tulovaikutus', 'Sahko_vesi_vastike'
             #, 'Maksuera_yht'
             ,'Muut_kulut','Kustannukset_yht'
             , 'Asunnon_omapaaoma','Lainan_maara','Asunnon_arvo'
             , 'Lyhennys','Korko','Kertynyt_korko'
             , 'Kertynyt_kustannus' ,'Maksettu_yhteensa'
             , 'Sijoitus','Sijoituksen_tuotto','Sijoituksen_arvo'
             , 'Vuokra' , 'Vuokra_kustannukset'
             , 'Vuokra_maksettu_yht'
             , 'Vuokra_sijoitus'
             , 'Vuokra_sijoituksen_tuotto'
             , 'Vuokra_sijoituksen_arvo')
   
   omistusasunto_matriisi =  matrix(NA , ncol=length(nimet) , nrow=length(kuukaudet)) 
   colnames(omistusasunto_matriisi) = nimet
   
   omistusasunto_matriisi[,'Vuosi'] = vuodet
   omistusasunto_matriisi[,'Kuukausi'] = kuukaudet
   omistusasunto_matriisi[1,'Nettopalkka'] = nettopalkka
   omistusasunto_matriisi[1,'Korkovahennyksen_tulovaikutus'] = korkovahennyksen_tulovaikutus
   omistusasunto_matriisi[1,'Sahko_vesi_vastike'] = omistusasunto_sahko_vesi_vastike
   #omistusasunto_matriisi$Maksuera_yht = -omistusasunto_lainan_tasoera
   omistusasunto_matriisi[1,'Muut_kulut'] = muut_kulut
   omistusasunto_matriisi[1,'Kustannukset_yht'] = omistusasunto_kustannukset_yht
   omistusasunto_matriisi[1,'Asunnon_omapaaoma'] = omistusasunto_asunnon_omapaaoma
   omistusasunto_matriisi[1,'Lainan_maara'] = lainan_maara
   omistusasunto_matriisi[1,'Asunnon_arvo'] = asunnon_hinta
   omistusasunto_matriisi[1,'Lyhennys'] = omistusasunto_lyhennys
   omistusasunto_matriisi[1,'Korko'] = omistusasunto_korko
   omistusasunto_matriisi[1,'Kertynyt_korko'] = omistusasunto_korko
   omistusasunto_matriisi[1,'Kertynyt_kustannus'] = omistusasunto_kertynyt_kustannus
   omistusasunto_matriisi[1,'Maksettu_yhteensa'] = omistusasunto_maksettu_yhteensa
   omistusasunto_matriisi[1,'Sijoitus'] = omistusasunto_sijoitus
   omistusasunto_matriisi[1,'Sijoituksen_tuotto'] = omistusasunto_sijoituksen_tuotto
   omistusasunto_matriisi[1,'Sijoituksen_arvo'] = omistusasunto_matriisi[1,'Sijoitus']
   
   Vuokra_kustannukset = sum( vuokra_vuokra
                              , vuokra_sahko
                              , vuokra_vesi
                              , vuokra_vastike
                              , -muut_kulut)
   omistusasunto_matriisi[1,'Vuokra'] = vuokra_vuokra
   omistusasunto_matriisi[1,'Vuokra_kustannukset'] = Vuokra_kustannukset
   omistusasunto_matriisi[1,'Vuokra_maksettu_yht'] = Vuokra_kustannukset + muut_kulut
   omistusasunto_matriisi[1,'Vuokra_sijoitus'] = nettopalkka + Vuokra_kustannukset
   omistusasunto_matriisi[1,'Vuokra_sijoituksen_tuotto'] = 0
   omistusasunto_matriisi[1,'Vuokra_sijoituksen_arvo'] = nettopalkka + Vuokra_kustannukset + alkupaaoma
   
   for( kk in kuukaudet[2:length(kuukaudet)]){
      
      #### yleiset ####
      
      omistusasunto_matriisi[kk,'Nettopalkka'] = omistusasunto_matriisi[kk-1,'Nettopalkka']*(1+palkan_kehitys/12)
      omistusasunto_matriisi[kk,'Muut_kulut'] = omistusasunto_matriisi[kk-1,'Muut_kulut']*( 1+inflaatio / 12 )
      
      #### omistusasunto ####
      
      omistusasunto_matriisi[kk,'Sahko_vesi_vastike'] = omistusasunto_matriisi[kk-1,'Sahko_vesi_vastike']*(1+inflaatio/12)
      omistusasunto_matriisi[kk,'Lainan_maara'] = ifelse(
         omistusasunto_matriisi[kk-1,'Lainan_maara'] + omistusasunto_matriisi[kk-1,'Lyhennys'] > 0 
         , omistusasunto_matriisi[kk-1,'Lainan_maara'] + omistusasunto_matriisi[kk-1,'Lyhennys']
         , 0)
      #omistusasunto_matriisi$Maksuera_yht[kk] = omistusasunto_matriisi$Maksettu_yhteensa[kk-1] - pmt(lainan_korko , laina_aika*12 - kk-1 , omistusasunto_matriisi$Lainan_maara[kk] )
      omistusasunto_matriisi[kk,'Korko'] = -omistusasunto_matriisi[kk,'Lainan_maara'] * omistusasunto_korkoprosentti / 12
      #omistusasunto_matriisi$Maksuera_yht[kk] = pmt(lainan_korko , laina_aika*12-(kk-1) , omistusasunto_matriisi$Lainan_maara[kk-1] )
      
      omistusasunto_matriisi[kk,'Kustannukset_yht'] = sum(omistusasunto_matriisi[kk,'Sahko_vesi_vastike']
                                                          , omistusasunto_maksuera
                                                          , -omistusasunto_matriisi[kk,'Muut_kulut'])
      omistusasunto_matriisi[kk,'Asunnon_arvo']  = omistusasunto_matriisi[kk-1,'Asunnon_arvo'] + omistusasunto_matriisi[kk-1,'Asunnon_arvo']*(asuntojen_hintakehitys/12) 
      omistusasunto_matriisi[kk,'Korko'] = -omistusasunto_matriisi[kk,'Lainan_maara'] * lainan_korko/12
      omistusasunto_matriisi[kk,'Asunnon_omapaaoma'] = omistusasunto_matriisi[kk,'Asunnon_arvo'] - omistusasunto_matriisi[kk,'Lainan_maara']
      omistusasunto_matriisi[kk,'Korkovahennyksen_tulovaikutus'] = ifelse( 
         -asuntolainan_korkovahennys * omistusasunto_matriisi[kk,'Korko'] * tulovero >= 0
         , -asuntolainan_korkovahennys * omistusasunto_matriisi[kk,'Korko'] * tulovero
         , 0)
      omistusasunto_matriisi[kk,'Lyhennys'] = omistusasunto_maksuera - omistusasunto_matriisi[kk,'Korko']
      #omistusasunto_matriisi$Kertynyt_korko[kk] = omistusasunto_matriisi$Kertynyt_korko[kk-1] + omistusasunto_matriisi$Korko[kk]
      omistusasunto_matriisi[kk,'Kertynyt_korko'] = sum( omistusasunto_matriisi[1:kk,'Korko'] )
      omistusasunto_matriisi[kk,'Kertynyt_kustannus'] = omistusasunto_matriisi[kk-1,'Kertynyt_kustannus'] + omistusasunto_matriisi[kk,'Sahko_vesi_vastike']
      omistusasunto_matriisi[kk,'Maksettu_yhteensa'] = omistusasunto_matriisi[kk,'Kertynyt_korko'] + omistusasunto_matriisi[kk,'Kertynyt_kustannus']
      omistusasunto_matriisi[kk,'Sijoitus'] = sum(omistusasunto_matriisi[kk,'Nettopalkka']
                                                  , omistusasunto_matriisi[kk,'Korkovahennyksen_tulovaikutus']
                                                  , omistusasunto_matriisi[kk,'Kustannukset_yht'])
      omistusasunto_matriisi[kk,'Sijoituksen_tuotto'] = omistusasunto_matriisi[kk-1,'Sijoituksen_arvo']*sijoituksen_oletustuotto/12
      omistusasunto_matriisi[kk,'Sijoituksen_arvo'] = sum(omistusasunto_matriisi[kk-1,'Sijoituksen_arvo']
                                                          , omistusasunto_matriisi[kk,'Sijoitus']
                                                          , omistusasunto_matriisi[kk,'Sijoituksen_tuotto'])
      
      #### vuokra ####
      
      omistusasunto_matriisi[kk,'Vuokra'] = ifelse( omistusasunto_matriisi[kk,'Vuosi'] == omistusasunto_matriisi[kk-1,'Vuosi']
                                                    , omistusasunto_matriisi[kk-1,'Vuokra']
                                                    , omistusasunto_matriisi[kk-1,'Vuokra'] * (1+vuokran_kehitys) )
      omistusasunto_matriisi[kk,'Vuokra_kustannukset'] = omistusasunto_matriisi[kk,'Vuokra'] - omistusasunto_matriisi[kk,'Muut_kulut']
      omistusasunto_matriisi[kk,'Vuokra_maksettu_yht'] = omistusasunto_matriisi[kk-1,'Vuokra_maksettu_yht'] + omistusasunto_matriisi[kk,'Vuokra']
      omistusasunto_matriisi[kk,'Vuokra_sijoitus'] = omistusasunto_matriisi[kk,'Nettopalkka'] + omistusasunto_matriisi[kk,'Vuokra_kustannukset']
      omistusasunto_matriisi[kk,'Vuokra_sijoituksen_tuotto'] = omistusasunto_matriisi[kk-1,'Vuokra_sijoituksen_arvo'] * sijoituksen_oletustuotto/12
      omistusasunto_matriisi[kk,'Vuokra_sijoituksen_arvo'] = omistusasunto_matriisi[kk-1,'Vuokra_sijoituksen_arvo'] + omistusasunto_matriisi[kk,'Vuokra_sijoitus'] + omistusasunto_matriisi[kk,'Vuokra_sijoituksen_tuotto']
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
                                          , tulovero = tulovero
                                          , vuokran_kehitys = vuokran_kehitys
                                          , vuokra_vuokra = vuokra_vuokra
                                          , vuokra_sahko = vuokra_sahko
                                          , vuokra_vesi = vuokra_vesi
                                          , vuokra_vastike = vuokra_vastike
                                          , alkupaaoma = alkupaaoma)

if(timing){
   paattymisaika = Sys.time()
   cat( paattymisaika - aloitusaika  )
}
if(viewing){
   View(omistusasunto_matriisi)
}
if(plotting){
   plot(omistusasunto_matriisi[,'Vuosi'],omistusasunto_matriisi[,'Sijoituksen_arvo'] , type='l', col='red')
   points(omistusasunto_matriisi[,'Vuosi'], omistusasunto_matriisi[,'Vuokra_sijoituksen_arvo'] , type='l', col='blue')
}


omistusasunto_matriisi = simuloi_asuminen(
   nettopalkka = nettopalkka 
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
   , tulovero = tulovero
   , vuokran_kehitys = vuokran_kehitys
   , vuokra_vuokra = vuokra_vuokra
   , vuokra_sahko = vuokra_sahko
   , vuokra_vesi = vuokra_vesi
   , vuokra_vastike = vuokra_vastike
   , alkupaaoma = alkupaaoma)

plot(omistusasunto_matriisi[,'Kuukausi'],omistusasunto_matriisi[,'Sijoituksen_arvo'] , type='l', col='red')
points(omistusasunto_matriisi[,'Kuukausi'], omistusasunto_matriisi[,'Vuokra_sijoituksen_arvo'] , type='l', col='blue')
legend( x='topleft' , c('Omakoti','Vuokra'), col=c('red','blue') ,lty=c(1,1) )
