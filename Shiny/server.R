
# shinyapps::deployApp('/home/yatzy/Dropbox/Projektit/oma_vs_vuokra/Shiny')
# server.R
# setwd( '/home/yatzy/Dropbox/Projektit/oma_vs_vuokra/Shiny' )
source('apufunktiot.R')
source('simuloi_asuminen.R')

shinyServer(
   function(input, output) {
      
#                   input = list( nettopalkka=2700 
#                                , muut_kulut = 23 
#                                , oma_paaoma = 0.15 ###  
#                                # muut parametrit
#                                , omistusasunto_sahko = 23 ###
#                                , omistusasunto_vesi = 45 ###
#                                , omistusasunto_vastike = 164 ###         
#                                , asunnon_hinta = 300000 ### 
#                                , lainan_korko = 0.035 ###
#                                , laina_aika = 30 ###
#                                , tarkasteluaika = 50
#                                , palkan_kehitys = 0.02 ###
#                                , inflaatio = 0.02 ###
#                                , asuntojen_hintakehitys = 0.02 ###
#                                , sijoituksen_oletustuotto = 0.07 ###
#                                , asuntolainan_korkovahennys = 0.3 ###
#                                , tulovero = 0.4 ###
#                                , omistusasunto_korko = 0.03
#                                , vuokran_kehitys = 0.03 ###
#                                , vuokra_vuokra = 704  ###
#                                , vuokra_sahko = 0 ###
#                                , vuokra_vesi = 0 ###  
#                                 , asunto_osana_varallisuutta = F
#                                )
      
      cat('### ALOITETAAN SIMULOINTI ### \n')
      
      omistusasunto_matriisi = reactive({ 
         # omistusasunto_matriisi = 
         simuloi_asuminen(
            # omistusasunto parametrit
            nettopalkka = input$nettopalkka ###
            , muut_kulut = input$muut_kulut ###
            , oma_paaoma = input$oma_paaoma ###  
            # muut parametrit
            , omistusasunto_korko = input$omistusasunto_korko
            , omistusasunto_sahko = input$omistusasunto_sahko ###
            , omistusasunto_vesi = input$omistusasunto_vesi ###
            , omistusasunto_vastike = input$omistusasunto_vastike ###         
            , asunnon_hinta = input$asunnon_hinta ### 
            , lainan_korko = input$lainan_korko ###
            , laina_aika = input$laina_aika ###
            , palkan_kehitys = input$palkan_kehitys ###
            , inflaatio = input$inflaatio ###
            , asuntojen_hintakehitys = input$asuntojen_hintakehitys ###
            , sijoituksen_oletustuotto = input$sijoituksen_oletustuotto ###
            , asuntolainan_korkovahennys = input$asuntolainan_korkovahennys ###
            #, tulovero = input$tulovero ###
            , vuokran_kehitys = input$vuokran_kehitys ###
            , vuokra_vuokra = input$vuokra_vuokra  ###
            , vuokra_sahko = input$vuokra_sahko ###
            , vuokra_vesi = input$vuokra_vesi ###
            , tarkasteluaika = input$tarkasteluaika
            , asunto_osana_varallisuutta = input$asunto_osana_varallisuutta
         )
      })
      cat('### SIMULOINTI VALMIS ### \n')
      #cat( is(omistusasunto_matriisi), ' \n')
      
      output$text1 <- renderText({ 
         paste("Valittu nettopalkka: " , input$nettopalkka )
      })
      output$text2 <- renderText({ 
         paste("Nettopalkka matriisissa: " , as.character(omistusasunto_matriisi()[1 , 'Nettopalkka']) )
      })
      
      #observe({ cat( 'nettopalkalkka: ' , input$nettopalkka , '\n' ) })
      
      output$nettopalkka = renderText(input$nettopalkka)
      output$omistusasunto_matriisi = renderDataTable(omistusasunto_matriisi)
      
      
      output$tuottokuva <- renderPlot({
         plot(omistusasunto_matriisi()[,'Kuukausi']/12 , omistusasunto_matriisi()[,'Sijoituksen_arvo'] 
              , type='l', col='red',main='Sijoituksen tuotto')   
         points(omistusasunto_matriisi()[,'Kuukausi']/12 , omistusasunto_matriisi()[,'Vuokra_sijoituksen_arvo']
                , type='l', col='blue') # , 
         legend( x='topleft' , c('Omakoti','Vuokra'), col=c('red','blue') ,lty=c(1,1) )
         abline(v=input$laina_aika)
         
      })
   }
)
