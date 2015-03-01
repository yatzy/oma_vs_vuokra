shinyUI(
   fluidPage(
      #titlePanel("censusVis"),
      fluidRow(
         column( 2,
                 #helpText("Create demographic maps with information from the 2010 US Census."),
                 
                 
                 checkboxInput("asunto_osana_varallisuutta", label = "Asunto osana varallisuutta", value = F),
                 sliderInput("sijoituksen_oletustuotto", 
                             label = "sijoituksen_oletustuotto:",
                             min = 0, max = 0.15, value = 0.075 ),
                 sliderInput("inflaatio", 
                             label = "inflaatio:",
                             min = 0, max = 0.05, value = 0.025 ),
                 sliderInput("asuntojen_hintakehitys", 
                             label = "asuntojen_hintakehitys:",
                             min = -0.05, max = 0.05, value = 0.02 ),
                 sliderInput("lainan_korko", 
                             label = "lainan_korko:",
                             min = 0, max = 0.05, value = 0.03),
                 sliderInput("palkan_kehitys", 
                             label = "palkan_kehitys:",
                             min = 0, max = 0.05, value = 0.02),
                 sliderInput("vuokran_kehitys", 
                             label = "vuokran_kehitys:",
                             min = 0, max = 0.05, value = 0.025),
#                  sliderInput("tulovero", 
#                              label = "tulovero:",
#                              min = 0, max = 0.5, value = 0.3),
                 sliderInput("asunnon_hinta", 
                             label = "asunnon_hinta:",
                             min = 0, max = 1000000, value = 300000),
                 sliderInput("omistusasunto_korko", 
                             label = "omistusasunto_korko:",
                             min = 0, max = 0.1, value = 0.03) ,
                 sliderInput("asuntolainan_korkovahennys", 
                             label = "asuntolainan_korkovahennys:",
                             min = 0, max = 1, value = 0.7) 
         ),
         
         column(8,
                textOutput('text1'),
                textOutput('text2'),
                
                #dataTableOutput('omistusasunto_matriisi')
                plotOutput(outputId = "tuottokuva", width = "100%")
         ),
         
         #       vuokra_vuokra = -1200
         #       vuokra_sahko = 0
         #       vuokra_vesi = 0
         #       vuokra_vastike = 0
         
         #          omistusasunto_vuokra = 0
         #          omistusasunto_sahko = -70
         #          omistusasunto_vesi = -20
         #          omistusasunto_vastike = -130
         
         
         column(2,
                sliderInput("vuokra_vuokra", 
                            label = "vuokra_vuokra:",
                            min = 0, max = 2000, value = 700),         
                sliderInput("vuokra_sahko", 
                            label = "vuokra_sahko:",
                            min = 0, max = 50, value = 0),
                sliderInput("vuokra_vesi", 
                            label = "vuokra_vesi:",
                            min = 0, max = 20, value = 0),
                sliderInput("omistusasunto_sahko", 
                            label = "omistusasunto_sahko:",
                            min = 0, max = 100, value = 70),
                sliderInput("omistusasunto_vesi", 
                            label = "omistusasunto_vesi:",
                            min = 0, max = 50, value = 20),
                sliderInput("omistusasunto_vastike", 
                            label = "omistusasunto_vastike:",
                            min = 0, max = 500, value = 130),
                sliderInput("nettopalkka", 
                            label = "nettopalkka:",
                            min = 0, max = 10000, value = 2700),
                sliderInput("muut_kulut", 
                            label = "muut_kulut:",
                            min = 0, max = 1500, value = 500),
                sliderInput("oma_paaoma", 
                            label = "oma_paaoma:",
                            min = 0, max = 0.5, value = 0.1),
                sliderInput("laina_aika", 
                            label = "laina_aika:",
                            min = 0, max = 100, value = 40),
                sliderInput("tarkasteluaika", 
                            label = "tarkasteluaika:",
                            min = 0, max = 100, value = 40)
         )
      )
   )
)