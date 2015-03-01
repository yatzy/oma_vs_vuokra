load('palkkaluokat.rda') # lataa palkkaluokat -data.framen

laske_tulovero = function( palkka ){
   for( i in 1:nrow(palkkaluokat) ){
      if(palkka < palkkaluokat$palkkaluokka[i]  ){
         return( palkkaluokat$veroprosentti[i-1] )
      }
   }
   return(56.3)
}

pmt = function( lainan_korko , laina_aika , asunnon_hinta ){
   osoittaja = (1+lainan_korko)^laina_aika * lainan_korko * asunnon_hinta
   nimittaja = (1+lainan_korko)^laina_aika - 1
   return(-osoittaja/nimittaja / 12)
}
