install.packages("Rcpp")
library("Rcpp")
install.packages("installr")
library("installr")
install.rtools()
cppFunction('
Rcpp::String znak1(double x) {
  if(x > 0) {
    return "liczba jest dodatnia";
  } else if(x < 0) {
    return "liczba jest ujemna";
  } else {
    return "liczba jest równa zero";
  }
}
')
znak1(-5)
znak1(0)
znak1(5)
dir.exists("C:/rtools45")
cumsum(1:10)
library(Rcpp)
cppFunction('
NumericVector my_cumsum_cpp(NumericVector x) {
  int n = x.size();
  NumericVector wynik(n);
  
  double suma = 0;
  for(int i = 0; i < n; i++) {
    suma += x[i];
    wynik[i] = suma;
  }
  
  return wynik;
}
')
my_cumsum_cpp(3:4)

x = c(1,3,5,7)
my_cumsum_cpp(x)
cumsum(x)
all.equal(my_cumsum_cpp(x),cumsum(x))
install.packages("microbenchmark") 
library(microbenchmark)
x = runif(1e6)
microbenchmark(
  Rcpp = my_cumsum_cpp(x),
  R = cumsum(x),
  times = 10
)


sourceCpp("cpp.cpp")


matrix
matrix()

myMatrix <- function(data, nrow, ncol, byrow = FALSE) {
  data <- as.vector(data)
  if (length(data) < nrow * ncol) {
    data <- rep(data, length.out = nrow * ncol)
  }
  mat <- vector(mode = "numeric", length = nrow * ncol)
  
  
}
set.seed(444)
wektor = sample(1:20,20,replace = F)
wektor
literki =sample(letters,20,replace = F)
literki
wynik = literki[wektor>5 & wektor<15]
wynik
#18
litery = sample(LETTERS,26)
litery
litery = sort(litery)
numery = sample(1:(length(litery)),replace = F)
numery = sort(numery)
numery
litery_numery = paste(litery,numery,sep="_")
litery_numery
repl

set.seed(444)
wektor = sample(1:20,20,replace = F)
wektor
literki =sample(letters,20,replace = F)
literki
wynik = literki[wektor>5 & wektor<15]
wynik
#18
litery = sample(LETTERS,26)
litery
litery = sort(litery)
numery = sample(1:(length(litery)),replace = F)
numery = sort(numery)
numery
litery_numery = paste(litery,numery,sep="_")
litery_numery
repl


set.seed(666)
wektor =sample(1:10,10) 
lista = as.list(wektor)

set.seed(666)
cyfry = lapply(lista,function(x)runif(x))
cyfry

str(cyfry)

polaczony = unlist(cyfry)
polaczony

lista = list(1,2,3,4,5)
wektor = sample(1:10,5,replace = T)

set.seed(123)

podpunktc = lapply(lista,function(x) rep(x,x))
podpunktc  



podpunktc

wektorc = unlist(podpunktc)  
wektorc  





funkcja = function(x){
  
  Q = quantile(x,probs=c(0.25,0.75))
  Q1 = Q[1] 
  Q3 = Q[2]
  IQR <- Q3 - Q1
  dolna =Q1-1.5*(Q3-Q1)
  gorna =Q3+1.5*(Q3-Q1)
  
  odstające = x<dolna | x > gorna
  nieodstajace = !odstające
  wynik = x
  
  attr(wynik,"odstające wartości") = x[odstające]
  attr(wynik,"nieodstające wartości") = x[nieodstajace]
  attr(wynik,"indeksy_odstające") = which(odstające)
  attr(wynik,"indeksy_nieodstające")=which(nieodstajace)
  
  return(wynik)
  
}

set.seed(1)
x = rcauchy(10)
wynik = funkcja(x)
wynik

odstające = attr(wynik,"odstające wartości")
nieodstające = attr(wynik,"nieodstające wartości")
odstające
nieodstające



set.seed(444)
symulacja = sample(10:20,30,replace = T)
nowy_wektor = length(symulacja)

i = 1;
for(i in seq_along(symulacja)){
  nowy_wektor[i] = symulacja[i]
  i = i+1
  cat("wektor uzupełniany:", nowy_wektor,"\n")
  cat("typ wektora:",typeof(nowy_wektor),"\n")
}
#------------------zad2
wektor = 1:9

lista = vector("list",length(wektor))
i=1
for(wartosc in wektor){
  lista[[i]] = wektor[1:wartosc]
  i=i+1
}mu

lista


lista = list(c(111),c(222),c(333),list())
lista[[4]] = c(123)
lista[[4]]

lista2 = list(c("alabala"))

lista_wynikowa = c(lista,lista2)
lista_wynikowa

wektory = unlist(lista_wynikowa)
wektory

i=0
for(elem in wektory){
  i = i+1
}
i
length(lista_wynikowa)

lista_wynikowa[[3]] = NULL
lista_wynikowa[[3]]

lista_wynikowa[[5]] = c(123)
lista_wynikowa
elo = list(c(1,1,1,111),c(222),c(333),list())
elo[[1]] = elo[[1]]+10
dodawanie = lapply(elo[[1]], function(x) x = x + 10)
dodawanie

elo
#Zadanie 8: Napisz program, aby uzyskac dlugosc pierwszych dwoch wektorow z danej listy.

zadanie = list(c(1,1,1,111),c(222,2),c(333),list())

jeden = length(zadanie[[1]])#wektor1
dwa = length(zadanie[[2]])#wektor2

jeden
dwa
zadanie1 = list(c(1,1,1,111),c(222,2),c(333))

zadanie2 = list(c(1,1,1,111))

wektor1 = c(1)
wektor2 = c(1)
zmienna = 1
if(wektor1 == wektor2){
  zmienna = 10
}

zmienna

lista1 <- list(1, 2, 3, 4, 5)
lista2 <- list(3, 4, 6)
wynik <- setdiff(lista1, lista2)

wynik

wektor1 = c(8,2,5,4,5,5,5)
sort.default(wektor1, decreasing = F)

liczba = 5


i = sum(wektor1 == liczba)
i
wektor2 = c(1,1,1,NA,1,1)

suma = sum(wektor1,na.rm=TRUE)
suma

wektor_wynikowy = wektor1 + wektor2
wektor_wynikowy
wektor1 = c(400,628,800)
wektor2 = c(1,1,1,10,1,1)


wektor_wynikowy =wektor1 + wektor2
wektor_wynikowy

wspolne <- intersect(wektor1, wektor2)
wspolne


wektor_odwrocony = c()

for(i in length(wektor1):1){
  wektor_odwrocony = c(wektor_odwrocony,wektor1[i])
}
wektor_odwrocony

wektor1 = c(400,628,800)
wektor2 = c(1,1,1,10,1,1)


wektor_wynikowy = c(wektor1 , wektor2)
wektor_wynikowy



wektor = sample(-10:10,10,replace = T)
wektor

zamien_na_srednia <- function(x) {
  srednia = mean(x[is.finite(x)], na.rm = TRUE)
  x[is.na(wektor) | is.nan(wektor) | is.infinite(wektor)] = srednia
  return(x)
}

wektor <- c(1, 2, NA, 3, NaN, 4, Inf)

nowy_wektor <- zamien_na_srednia(wektor)

print(nowy_wektor)

wektor =c(1,2,3,4,5)
faktor = factor(wektor)
wektor
faktor

levels(faktor)

levels(faktor)[1] = "elo"

levels(faktor)



obiekt_6 <- read.table(file ="C:\\Users\\mateu\\OneDrive\\Pulpit\\zadanie5.txt",header=T,sep='&')
lista = as.list(obiekt_6 ) 
lista

lista$nowe = ifelse(lista$losowe<0.8,"malo","duzo")
lista

suma = lapply(lista,function(x){
  if(is.numeric(x))sum(x,na.rm=T)
  else NA
} )
suma


wektor = 1:9
wektor
lista = vector("list",length(wektor))
lista

i=1
for(i in seq_along(wektor)){
  lista[[i]] = wektor[1:i]
  i=i+1
}
lista
macierz = matrix()
lista1 = list(c(111), c(222))
lista2 = list(c(333), c(444))
lista3 = list(c(555), c(666))

macierz = do.call(rbind,lista1)
macierz = do.call(rbind,lista2)
macierz = do.call(rbind,lista3)
macierz



wektor1 = c(3,2,3,4,5,3,3)
szukana_wartosc = 3

ile_cyfr = sum(wektor1 == 3)
ile_cyfr
funkcja = function(x){#wektor
  suma = sum(x)
  n = length(x)
  i=1
  
  names(x) = paste0("x_",seq_along(x))  
  
  
  return(x,suma)
}
x= c(1,2,3,4,5)
b = seq_along(x)
print(b)
wektor = funkcja(x)
wektor


wektor = 1:9
lista1 = vector("list",length(wektor))
lista2 = vector("list",length(wektor))
lista1

nazwy = paste0("l",wektor)
names(lista1) = nazwy
names(lista2) = nazwy
lista1
lista2
names(lista1)
i =1
while(i<=length(wektor)){
  lista1[[i]] = wektor[1:i]
  lista2[[1]] = NULL
  i=i+1
}
lista1
lista2

funkcja = function(x){#wektor
  
  if(is.data.frame(x) || is.matrix(x)){
    colnames(x) = paste0("x_",seq_len(ncol(x)))
    
    suma = colSums(x,na.rm=T)
  }
  
  suma = sum(x,na.rm=T)
  n = length(x)
  i=1
  
  names(x) = paste0("x_",seq_along(x))  
  
  
  return(x,suma)
}
x= c(2,2,2)
b = seq_along(x)
b
wektor = funkcja(x)
wektor


wektor = 1:9
lista1 = vector("list",length(wektor))
lista2 = vector("list",length(wektor))
lista1

nazwy = paste0("l",wektor)
names(lista1) = nazwy
names(lista2) = nazwy
lista1
lista2
names(lista1)
i =1
while(i<=length(wektor)){
  lista1[[i]] = wektor[1:i]
  lista2[[1]] = NULL
  i=i+1
}
lista1
lista2



faktor = factor(c("niski","średni","aysoki"))
faktor[1] = "bardzo_niski"

faktor

levels(faktor)=c(levels(faktor),"bardzo_niski")
faktor

levels(faktor)[levels(faktor)=="niski"] = "bardzo_niski"
faktor
levels(faktor)
ordered(faktor)


wektor = 1:10

lista = vector("list",length(wektor))

lista

i=1
for(elem in wektor){
  lista[[i]] = elem
  i=i+1
}

lista
macierz = matrix(0,nrow = 30,ncol=5)
macierz

set.seed(222)
macierz[,1] = sample(1:30,30,replace =F)
macierz[,5]=sample(1:3,30,replace=T)

macierz[,2:4] = runif(3*30)

wektor = 1:30

macierz = cbind(macierz,wektor)
macierz    

lista = list(c(2:31),c(3:32))

macierz = cbind(macierz,do.call(cbind,lista))
macierz

macierz = rbind(macierz,c(1:5))
macierz = rbind(macierz,c(1:8))
macierz


tablica_jedno = as.vector(macierz)
tablica_jedno
tablica_jednowymiarowa <- as.vector(t(macierz))
tablica_jednowymiarowa


kwantyle = function(x){
  Q1 = quantile(x,0.25)
  Q3 = quantile(x, 0.75)
  
  dolna_granica <- Q1 - 1.5 * (Q3 - Q1)
  gorna_granica <- Q3 + 1.5 * (Q3 - Q1)
  
  odstajace = x[x<dolna_granica | x > gorna_granica]
  nieodstajace = !odstajace
}
setdiff


parzyste = seq(from=50,by=2,length.out=15)
parzyste


k=50
wektor = c()
for(i in 1:30){
  if(i%%2 ==0){
    wektor = c(wektor,k+i)
  }
}
wektor



tablica = matrix(parzyste, nrow=5,ncol=3)
tablica
objekt = read.table(file="C:\\Users\\mateu\\
                    OneDrive\\Pulpit\\lol.txt",header=F,sep=':')

typeof(objekt)

objekt[2,3]

str(objekt)


wektor1 = as.character(unlist(objekt[1,]))
wektor2 = as.character(unlist(objekt[2,]))

wektor = c(letters)
wektor
objekt
colnames(objekt) = wektor[1:3]

objekt

suma1 = sum(wektor1 != "")

suma1

x=c("malo","malo","elo","elo","malo","malo","elo","elo","elo")
macierz = matrix(x,nrow=3,ncol=3)
macierz

suma_elo = sum(macierz =="elo")
suma_elo

suma_malo = sum(macierz == "malo")
suma_malo
#wykresy
szereg_1 = c(1,3,9,4,7)
szereg_2 = c(2,5,12,8,9)

plot(szereg_1,type="l")

title(main = "Wykres1", col.main = "red")

dev.off()

par(mar = c(2,2,1,0), mfrow=c(2,2) )
rozstep = range(szereg_1,szereg_2)
plot(szereg_1,main="wykres2",type="o",ylim=rozstep)

lines(szereg_2,col="yellow",lty=2,lwd=4)
#points()

set.seed(123)
r_norm = rnorm( 1000)

hist(x = r_norm,col="blue",main="Rozkład normalny",freq=F,xlab="Kwantyle",ylab="Gestosc")
lines(density(r_norm),col="red",lwd=4 ,lty=9)

#Bisekcja
funkcja = function(x){
  x^3 - 2*x - 5
}

dziedzina = seq(-3,3,by=0.1)
wartosci_y = funkcja(dziedzina)
plot(x = dziedzina , y = wartosci_y,col="blue",lwd=2,type="l")

abline(h=0)

Bisekcja = function(f,a,b,maxiter=100,eps=1e-16){
  stopifnot(f(a)* f(b) < 0) # twierdzenie darboux
  for(i in 1:maxiter){
    cc = (a+b)/2
    if(abs(f(cc) < eps))break
    if(f(cc) * f(a) > 0){a = cc}else{b = cc}
    
  }
  if(i== maxiter){
    warning("osiągnięto max liczbe iteracji")
  }
  return(list(m_zerowe = cc,wartosc = f(cc) ))
  
  
  
  
}
data = Sys.Date()


typeof(data)
str(data)
class(data)

as.double(data)

as.Date("1970-01-01")

#strptime użyj do %d %A


format(data,"%B %d %Y")
format(data,"Dzis mamy %d (%A) %b roku %Y)")

as.Date("1970--01::07")

czas = Sys.time()
czas
x = seq(-3,3,length.out=100)

y = x
par(mar=c(5,6,5,1))
plot(y,x,type="l",xlim=c(-3,3),ylim=c(-3,3),xlab="x",ylab="y",main="Wykres funkcji")
abline(h=0,v=0)

for(i in seq(-3,3,by=0.5)){
  abline(h=i,v=i,col = "red",lty=2)
}
wektor = rnorm(1000)
wektor

par(mar=c(4,5,4,1))
hist(wektor,breaks=30,col="blue",freq=F,main="Histogram")

elo2 = Sys.Date()
czas = Sys.time()
godzina = as.numeric(format(czas,"%H"))
godzina
if (godzina < 12) {
  cat("Jest przed południem\n")
} else {
  cat("Jest po południu\n")
}

czy_przestepny = function(rok){
  
  (rok%%4 == 0 & rok%% 100 != 0) | (rok%%400 == 0)
  
}
lata = c(2018,2016)
sapply(lata,czy_przestepny)



dir.create("zadanie_3")

elo = function(x){
  y = toupper(x)
  
  liczby = gsub(pattern="[[^:digit]]","",x)
  rozmiar_wektora = nchar(liczby)
  
  y = gsub(pattern="[[:space:]]","_",y)
  
  attr(y, "liczba_cyfr") <- rozmiar_wektora
  return (y)
  
}
wektor= c("poprawa","e l o","123sie mA")

elo(wektor)

czy_takie_samo=c(1,2,3,4,5)
czy_takie_samo
aczy_takie=c(1:5)
aczy_takie

install.packages("Rcpp")
library("Rcpp")

cppFunction(
  '
  NumericVector indexC(NumericVector x, IntegerVector ind) {
  int n = ind.size();
  NumericVector wynik(n);
  
  for (int i = 0; i < n; i++) {
    int idx = ind[i] - 1; // w R indeksy zaczynają się od 1, w C++ od 0
    if (idx < 0 || idx >= x.size()) {
      stop("Index out of bounds");
    }
    wynik[i] = x[idx];
  }
  
  return wynik;
}

  
  '
)
x <- 14:20
indexC(x, c(4,6,7))
dir.create("elo")
setwd("elo")
for(i in 1:20){
  pliki = paste0("plik_",i,".txt")
  data = Sys.Date() +(i-1)
  writeLines(as.character(data),pliki)
}
list.files()

pliki <- list.files(pattern="^.*.txt$")
pliki_sorted <- pliki[order(as.numeric(gsub("^.*.txt$", "", pliki)))]
pliki_sorted

katalog = dir.create("zadanie")
setwd("zadanie")
for(i in 1:20){
  nazwa_pliku=paste0("plik_",i,".txt")
  data=Sys.Date()+(i-1)
  writeLines(as.character(data),nazwa_pliku)
}

list.files()

readLines("plik_1.txt")


sortowane_pliki = sort(list.files(pattern="^.*.txt$"))
sortowane_pliki

plik_wszystko =file("plik_wszystko")

install.packages("Rcpp")
library("Rcpp")
cppFunction(
  '
  NumericVector numi(NumericVector x) {
  int n = x.size();
  NumericVector ciag(n);
  double suma = 0.0;
  for(int i =0;i<n;i++){
    suma +=x[i];
    ciag[i] = suma;
  }
  return ciag;
}
  
  
  '
)
numi(1:4)


set.seed(123)
macierz_1 = matrix(runif(100),nrow=10,ncol=10)

srodowisko_1 = new.env()
assign("macierz_1",macierz_1,envir=srodowisko_1)
srodowisko_1$macierz


funkcja = function(e){
  
  m1 = get("macierz_1",envir=e)
  assign("macierz_2",m1,envir=e)
  
  assign("macierz_3",m1,envir=.GlobalEnv)
  return (NULL)
}
lines(density(wektor),col="red",lty=2,lwd=3)

data = Sys.Date()
data

sekwencja = seq(from=data,length.out=10,by="1 month")

write.table(sekwencja,file="zadanie_4",row.names=F,col.names=F)





macierz_1 = matrix(runif(100),10,10)
srodowisko_1 = new.env()

assign("macierz_1",macierz_1,envir=srodowisko_1)

Funkcja = function(x){
  x$macierz_2 = srodowisko_1$macierz_1
  assign("macierz_3",srodowisko_1$macierz_1,envir = globalenv())
}
Funkcja(srodowisko_1)





for(i in 1:20){
  nazwa_pliku=paste0("plik_",i,".txt")
  data=Sys.Date()+(i-1)
  writeLines(as.character(data),nazwa_pliku)
}

install.packages("Rcpp")
library("Rcpp")
cppFunction(
  '
 NumericVector my_cumsum(int x) {

  NumericVector ciag(10);
  
  double suma = x;
  int k =0;
  for (int i = 0; i < 30; i=i+3) {
    suma += i;
    ciag[k] = suma;
    k=k+1;
  }
  
  return ciag;
}
  '
)
my_cumsum(10)


Pierwsze = new.env()
assign("x",10,envir = Pierwsze)
assign("Drugie",new.env(),envir = Pierwsze)

assign("y",20,envir=Pierwsze$Drugie)
ls(Pierwsze$Drugie)
zmiana = function(e){
  temp = e$x
  e$x = e$Drugie$y
  e$Drugie$y = temp
}
zmiana(Pierwsze)
Pierwsze$x



czas_char <- "2019-sty-13 20:10:10"
czas_num <- as.numeric(as.POSIXct(czas_char, format = "%Y-%b-%d %H:%M:%S"))
sekwencja <- seq(from = Sys.Date(), by = "year", length.out = 10)
write.table(sekwencja, file = "zadanie_4.txt", col.names = F, row.names = F)


Pierwsze <- new.env()

assign("x",10, envir = Pierwsze)
assign("Drugie", new.env(), envir = Pierwsze)
assign("y",20, envir = Pierwsze$Drugie)

zmiana <- function(srd){
  srd$x <- srd[[ls(srd)[[1]]]]$y
}

zmiana(Pierwsze)

x <- seq(from = -3,to = 3,length.out = 100)
y <- x
#b
par(mar= c(5,6,5,1))
#c
plot(x,y,type = "l",main = "Uklad wspolrzednych",xlab = "X",ylab = "Y",col = "blue")
#d
i<--3
while(i<=3){
  abline(h = i,v = i,col = "gray")
  i<-i+0.5
}





















































