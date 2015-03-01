
   
library(XML)
theurl <- 'http://www.veronmaksajat.fi/luvut/Laskelmat/Palkansaajan-veroprosentit/'
tables <- readHTMLTable(theurl)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
palkkaluokat = tables[[which.max(n.rows)]][ 2:n.rows , c(2,3) ]
colnames(palkkaluokat) = c('palkkaluokka' , 'veroprosentti')
palkkaluokat$palkkaluokka = as.numeric( gsub(' ' , '' , palkkaluokat$palkka) )
palkkaluokat$veroprosentti = as.numeric( gsub( ' %','', gsub(',','.',palkkaluokat$veroprosentti) ) )
palkkaluokat = rbind( c(0,7.1) , palkkaluokat )
save(palkkaluokat , file='palkkaluokat.rda')
