library(digest)

n = 10
m = 100
k = 4
l = rep(0, m)

URL_clicked=c('www.google.com','www.facebook.com','www.yahoo.com','www.gamespot.com','www.ign.com','www.stayfit.com','www.gamer.com','www.gmail.com','www.outlook.com','www.mailer.com')
URL_notclicked=c('www.insider.com','www.outsider.com','www.paytm.com','www.gpay.com','www.gameology.com','www.youpay.com','www.msrit.com','www.theforum.com','www.bms.com','www.pvr.com')


get_hash = function(item, seed) {
  hex_str = digest(object = item,
                   algo = "murmur32",
                   serialize = F,
                   seed = seed)
  hex = paste('0x', hex_str, sep = "")
  return(as.numeric(hex) %% m)
}

add = function(x) {
  for(i in 1:k) {
    hash = get_hash(x, i)
    l[hash] <<- 1
  }
}

check = function(x) {
  for(i  in 1:k) {
    hash = get_hash(x, i)
    if(l[hash] == 0) return(FALSE)
  }
  return(TRUE)
}

for(i in 1:n) add(URL_clicked[i])

test_set = sample(c(URL_clicked, URL_notclicked), n)

for(i in 1:n) {
  if(check(test_set[i])) cat(test_set[i], "is probably clicked.\n")
  else cat(test_set[i], "is definitely not clicked.\n")
}