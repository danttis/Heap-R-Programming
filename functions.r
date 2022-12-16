# Os algoritmos foram feitos com base no livro Estruturas de Dados e Seus Algoritmos, e implementados na linguagem R.

# Algoritmo 6.1 Subir
subir = function(x, i){
  j = i%/%2
  if(j >= 1){
    if(x[i] > x[j]){
      x[c(i,j)] = x[c(j,i)]
      x = subir(x, j)
    }
  }
  return(x)
}

# Algoritmo 6.2 Descer
descer = function(x,n,i){
  j = 2*i
  if(j <= n){
    if(j < n){
      if(x[j+1] > x[j]){
        j = j+1
      }
    }
    if(x[i] < x[j]){
      x[c(i,j)] = x[c(j,i)]
      x = descer(x,n,j)
    }
  }
  return(x)
}

# Algoritmo 6.3 Inserção
inserir = function(x, e){
  if(length(x) < 15){ #Limite definido para a função visto que todos os objetos no R são dinâmicos.
    x = append(x, e)
    n = length(x)
    if(length(x) > 1){
      x = subir(x,n)
    }
    return(x)
  }
  else{
    cat("overflow \n")
  }
}

# Algoritmo 6.4 Remoção
remover = function(x){
  n = length(x)
  if(n > 0){
    x = x[-1]
    x = descer(x,n-1,1)
    return(x)
  }
  else{
    cat("underflow")
  }
}

# Algoritmo 6.5 Construção
criar_heap = function(x){
  n = length(x)
  for(i in 2:n){
    x = subir(x,i)
  }
  return(x)
}

# Algoritmo 6.6 Arranjar
arranjar = function(x, n){
  i = n%/%2
  for(j in 1:i){
    x = descer(x,n,j)
  }
  return(x)
}

# Algoritmo 7.6 Heapsort
heapsort = function(x){
  n = length(x)
  x = arranjar(x,n)
  m = n
  while(m > 0){
    x[c(1,m)] = x[c(m,1)]
    m = m - 1
    x = descer(x,m,1)
  }
  return(x)
}

