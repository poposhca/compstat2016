#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int sample(NumericVector v)
{
  //Nos aseguramos de que las probabilidades sumen 1 en caso de que el vector
  //este chaca
  v = v / sum(v);
  NumericVector acum = cumsum(v);
  //Runif en C regresa un vector, por lo que queremos el primer indice
  double u = runif(1)[0];
  int i = 0;
  while(acum[i] < u)
  {
    i++;
  }
  //Los vectores de R inician en i
  return i + 1;
}

// [[Rcpp::export]]
int mc_transition(int state, NumericMatrix trans)
{
  //Trns es la matriz de transicion, es decir, el grafo
  //NumericMatrix hace que puedas casi manejar la matrices como en C
  //_ es parte de Rcpp (Rcpp::_) y es como el * en expresion regular
  //Al parecer en R esto se escribe como R[y,]
  NumericVector prob = trans(state-1,_);
  int newState = sample(prob);
  return newState;
}

// [[Rcpp::export]]
NumericVector mc_trayectoria(int state, int nobs, NumericMatrix trans)
{
  NumericVector trayectoria(nobs + 1);
  trayectoria[0] = state;
  for(int i = 0; i < nobs; i++)
  {
    trayectoria[i+1] = mc_transition(trayectoria[i], trans);
  }
  return trayectoria;
}