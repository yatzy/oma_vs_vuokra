############ SIMULOINTI ############ 

# simuloi_asuminen = function( 
#    ### yleiset paramterit
#    nettopalkka = nettopalkka
#    , oma_paaoma = oma_paaoma
#    , muut_kulut = muut_kulut 
#    ### omistusasunto parametrit
#    , korkovahennyksen_tulovaikutus = korkovahennyksen_tulovaikutus 
#    , omistusasunto_sahko =omistusasunto_sahko
#    , omistusasunto_vesi =omistusasunto_vesi
#    , omistusasunto_vastike =omistusasunto_vastike
#    , omistusasunto_lainan_tasoera = omistusasunto_lainan_tasoera
#    , omistusasunto_kustannukset_yht = omistusasunto_kustannukset_yht
#    , omistusasunto_asunnon_omapaaoma = omistusasunto_asunnon_omapaaoma 
#    , lainan_maara = lainan_maara
#    , asunnon_hinta = asunnon_hinta
#    , omistusasunto_lyhennys = omistusasunto_lyhennys
#    , omistusasunto_korko = omistusasunto_korko 
#    , lainan_korko = lainan_korko
#    , laina_aika = laina_aika
#    , tarkasteluaika = tarkasteluaika
#    , palkan_kehitys = palkan_kehitys
#    , inflaatio = inflaatio
#    , asuntojen_hintakehitys = asuntojen_hintakehitys
#    , sijoituksen_oletustuotto = sijoituksen_oletustuotto
#    , asuntolainan_korkovahennys = asuntolainan_korkovahennys
#    , tulovero = tulovero 
#    ### vuokraparametrit
#    , vuokran_kehitys = vuokran_kehitys
#    , vuokra_vuokra = vuokra_vuokra
#    , vuokra_sahko = vuokra_sahko
#    , vuokra_vesi = vuokra_vesi
# ){

simuloi_asuminen = function( 
   ### yleiset paramterit
   nettopalkka 
   , oma_paaoma
   , muut_kulut 
   ### omistusasunto parametrit
   , omistusasunto_sahko
   , omistusasunto_vesi
   , omistusasunto_vastike
   , omistusasunto_kustannukset_yht
   , omistusasunto_asunnon_omapaaoma 
   , lainan_maara 
   , asunnon_hinta
   , omistusasunto_lyhennys
   , omistusasunto_korko 
   , lainan_korko 
   , laina_aika 
   , tarkasteluaika
   , palkan_kehitys
   , inflaatio 
   , asuntojen_hintakehitys
   , sijoituksen_oletustuotto
   , asuntolainan_korkovahennys
   #, tulovero  
   , asunto_osana_varallisuutta
   ### vuokraparametrit
   , vuokran_kehitys , vuokra_vuokra
   , vuokra_sahko , vuokra_vesi
){
   
   ############ LASKETUT ############ 
   
   alkupaaoma = asunnon_hinta * oma_paaoma
   lainan_maara = asunnon_hinta - alkupaaoma
   
   vuodet <- rep( seq(from=1,to=tarkasteluaika) , each=12 )
   kuukaudet = 1:length(vuodet)
   
   # omistusasunto
   
   omistusasunto_sahko = -abs(omistusasunto_sahko)
   omistusasunto_vesi = -abs(omistusasunto_vesi)
   omistusasunto_vastike = -abs(omistusasunto_vastike)
   
   omistusasunto_sahko_vesi_vastike = sum( omistusasunto_sahko
                                           , omistusasunto_vesi
                                           , omistusasunto_vastike)
   
   # vuokra
   
   vuokra_vuokra = -abs(vuokra_vuokra)
   vuokra_sahko = -abs(vuokra_sahko)
   vuokra_vesi = -abs(vuokra_vesi)
   alkupaaoma = oma_paaoma*asunnon_hinta
   
   ############ LAHTOTILANNE ############ 
   tulovero = laske_tulovero(nettopalkka)
   # http://www.math.jyu.fi/matyl/peruskurssi/talousmatematiikkaa/korkor3.8.htm  
   omistusasunto_lainan_tasoera_osoittaja = (1+lainan_korko)^laina_aika * lainan_korko * asunnon_hinta
   omistusasunto_lainan_tasoera_nimittaja = (1+lainan_korko)^laina_aika - 1
   omistusasunto_lainan_tasoera = omistusasunto_lainan_tasoera_osoittaja / omistusasunto_lainan_tasoera_nimittaja / 12
   omistusasunto_korko = -lainan_maara * lainan_korko/12
   
   korkovahennyksen_tulovaikutus = asuntolainan_korkovahennys * omistusasunto_korko * tulovero
   omistusasunto_korkokustannukset = -abs(lainan_korko / 12 * lainan_maara)
   
   omistusasunto_yhteensa = sum( omistusasunto_sahko
                                 , omistusasunto_vesi
                                 , omistusasunto_vastike
                                 , omistusasunto_korkokustannukset )
   
   omistusasunto_kertynyt_korko = omistusasunto_korko
   omistusasunto_kertynyt_kustannus = omistusasunto_sahko_vesi_vastike
   omistusasunto_maksettu_yhteensa = omistusasunto_kertynyt_korko + omistusasunto_kertynyt_kustannus
   omistusasunto_maksuera = -abs( omistusasunto_lainan_tasoera)
   
   
   omistusasunto_kustannukset_yht = sum( omistusasunto_sahko
                                         , omistusasunto_vesi
                                         , omistusasunto_vastike
                                         , -omistusasunto_lainan_tasoera
                                         , -muut_kulut )
   
   omistusasunto_lyhennys = -( omistusasunto_lainan_tasoera + omistusasunto_korko )
   omistusasunto_korkovahennyksen_tulovaikutus = - asuntolainan_korkovahennys * omistusasunto_korko * tulovero
   omistusasunto_sijoitus = sum( nettopalkka
                                 , korkovahennyksen_tulovaikutus
                                 , omistusasunto_kustannukset_yht )
   omistusasunto_sijoituksen_tuotto = 0                             
   omistusasunto_asunnon_omapaaoma = asunnon_hinta - lainan_maara
   
   
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
      tulovero = laske_tulovero( omistusasunto_matriisi[kk,'Nettopalkka'] )
      omistusasunto_matriisi[kk,'Muut_kulut'] = omistusasunto_matriisi[kk-1,'Muut_kulut']*( 1+inflaatio / 12 )
      
      #### omistusasunto ####
      
      omistusasunto_matriisi[kk,'Sahko_vesi_vastike'] = omistusasunto_matriisi[kk-1,'Sahko_vesi_vastike']*(1+inflaatio/12)
      omistusasunto_matriisi[kk,'Lainan_maara'] = ifelse(
         omistusasunto_matriisi[kk-1,'Lainan_maara'] + omistusasunto_matriisi[kk-1,'Lyhennys'] > 0 
         , omistusasunto_matriisi[kk-1,'Lainan_maara'] + omistusasunto_matriisi[kk-1,'Lyhennys']
         , 0)
      omistusasunto_matriisi[kk,'Korko'] = -omistusasunto_matriisi[kk,'Lainan_maara'] * lainan_korko / 12      
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
      omistusasunto_matriisi[kk,'Kertynyt_korko'] = sum( omistusasunto_matriisi[1:kk,'Korko'] )
      omistusasunto_matriisi[kk,'Kertynyt_kustannus'] = omistusasunto_matriisi[kk-1,'Kertynyt_kustannus'] + omistusasunto_matriisi[kk,'Sahko_vesi_vastike']
      omistusasunto_matriisi[kk,'Maksettu_yhteensa'] = omistusasunto_matriisi[kk,'Kertynyt_korko'] + omistusasunto_matriisi[kk,'Kertynyt_kustannus']
      omistusasunto_matriisi[kk,'Sijoitus'] = sum(omistusasunto_matriisi[kk,'Nettopalkka']
                                                  , omistusasunto_matriisi[kk,'Korkovahennyksen_tulovaikutus']
                                                  , omistusasunto_matriisi[kk,'Kustannukset_yht'])
      omistusasunto_matriisi[kk,'Sijoituksen_tuotto'] = ifelse(asunto_osana_varallisuutta
         , omistusasunto_matriisi[kk-1 , 'Sijoituksen_arvo'] * sijoituksen_oletustuotto/12 - omistusasunto_matriisi[kk,'Sijoituksen_tuotto']                                                      
         , omistusasunto_matriisi[kk-1 , 'Sijoituksen_arvo'] * sijoituksen_oletustuotto/12 )
      omistusasunto_matriisi[kk,'Sijoituksen_arvo'] = ifelse( asunto_osana_varallisuutta,
                                                              sum(omistusasunto_matriisi[kk-1,'Sijoituksen_arvo']
                                                                  , omistusasunto_matriisi[kk,'Sijoitus']
                                                                  , omistusasunto_matriisi[kk,'Sijoituksen_tuotto']
                                                                  , omistusasunto_matriisi[kk,'Asunnon_arvo'])
                                                              , sum(omistusasunto_matriisi[kk-1,'Sijoituksen_arvo']
                                                                    , omistusasunto_matriisi[kk,'Sijoitus']
                                                                    , omistusasunto_matriisi[kk,'Sijoituksen_tuotto']) )
      
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
   
   return(omistusasunto_matriisi)
}
