dodaj <- function(a, b)
{
  a + b
}

srednia <- function(wektor)
{
  suma <- 0
  licznik <- 0
  for(x in wektor)
  {
    suma <- suma + x
    licznik <- licznik + 1
  }
  if(licznik == 0) return(0)
  return(suma/licznik)
}

bmi <- function(masa, wzrost)
{
  return(masa/wzrost^2)
}

czy_pierwsza <- function(n)
{
  if (n <= 1) return(FALSE)
  if (n == 2) return(TRUE)
  if (n %% 2 == 0) return(FALSE)
  
  pierwiastek = sqrt(n)
  dzielnik <- 3
  while(dzielnik <= pierwiastek)
  {
    if(n %% dzielnik == 0) return(FALSE)
    dzielnik <- dzielnik + 2
  }
  return(TRUE)
}

pierwiastek_babilon <- function(S, iteracje = 10)
{
  if (S < 0) return(NaN)
  if (S == 0) return(0)
  
  x <- S / 2
  for (i in 1:iteracje)
  {
    x <- 0.5 * (x + S /x)
  }
  return(x)
}

czy_rowne_wektory <- function(w1, w2)
{
  if (length(w1) != length(w2)) return(FALSE)
  
  for (i in 1:length(w1))
  {
    if (w1[i] != w2[i]) return(FALSE)
  }
  return(TRUE)
}

czy_palindrom <- function(w)
{
  len <- length(w)
  
  for (i in 1:(len %% 2))
  {
    if (w[i] != w[len - i + 1]) return(FALSE)
  }
  return(TRUE)
}

min_max <- function(w)
{
  min_val = w[1]
  max_val <- w[1]
  
  for (x in w[-1])
  {
    if (x < min_val) min_val <- x
    if (x > max_val) max_val <- x
  }
  return(list(min = min_val, max = max_val))
}

normalizacja_min_max <- function(w)
{
  zakres <- min_max(w)
  min_v <- zakres$min
  max_v <- zakres$max
  
  if (min_v == max_v) return(rep(0, length(w)))
  
  return((w - min_v) / (max_v - min_v))
}

unikalne <- function(w)
{
  unikalne <- c()
  
  for (x in w) 
  {
    jest_w_liscie <- FALSE
    for (u in unikalne)
    {
      if (x == u)
      {
        jest_w_liscie <- TRUE
        break
      }
    }
    if(!jest_w_liscie)
    {
      unikalne <- c(unikalne, x)
    }
  }
  return(unikalne)
}
  
wartosci_wspolne <- function(w1, w2)
{
  wspolne <- c()
  
  w1_uni <- unikalne(w1)
  
  for (x in w1_uni)
  {
    for (y in w2)
    {
      if(x == y)
      {
        wspolne <- c(wspolne, x)
        break
      }
    }
  }
  return(wspolne)
}

sma <- function(wektor, okres)
{
  n <- length(wektor)
  if(okres > n) return(NULL)
  
  wynik <- numeric(n - okres + 1)
  
  for (i in 1:(n - okres + 1))
  {
    okno <- wektor[i:(i + okres - 1)]
    wynik[i] = mean(okno)
  }
  return(wynik)
}

dzielenie_zdania <- function(zdanie)
{
  slowa <- c()
  temp <- ""
  
  for(i in 1:nchar(zdanie))
  {
    znak <- substr(zdanie, i, i)
    if (znak == " ")
    {
      if (nchar(temp) > 0)
      {
        slowa <- c(slowa, temp)
        temp <- ""
      }
    }
    else
    {
      temp <- paste0(temp, znak)
    }
  }
  
  if (nchar(temp) > 0)
  {
    slowa <- c(slowa, temp)
  }
  return(slowa)
}

entropia_s <- function(wektor) {

  wartosci <- unikalne(wektor)
  licznosci <- numeric(length(wartosci))
  n <- length(wektor)
  
  for (i in 1:length(wartosci)) {
    count <- 0
    for (x in wektor) {
      if (x == wartosci[i]) count <- count + 1
    }
    licznosci[i] <- count
  }
  
  p <- licznosci / n
  
  entropia <- 0
  for (prob in p) {
    if (prob > 0) {
      entropia <- entropia - (prob * log2(prob))
    }
  }
  return(entropia)
}

silnia <- function(n)
{
  if (n < 0) return(NaN)
  if (n == 0 || n == 1) return(1)
  return(n * silnia(n - 1))
}

newton <- function(n, k)
{
  if (k < 0 || k > n) return(0)
  
  licznik <- silnia(n)
  mianownik <- silnia(k) * silnia(n-k)
  return(licznik/mianownik)
}

newton_rekurencyjnie <- function(n, k)
{
  if (k < 0 || k > n) return(0)
  if (k == 0 || k == n) return(1)
  
  return(newton_rekurencyjnie(n-1, k-1) + newton_rekurencyjnie(n-1, k))
}
