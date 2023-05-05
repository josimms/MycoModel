#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

struct symphony_parameters{
  double A;
  double s;
  double r;
  double m_p;
  double e_p;
  double r_p;
  double k;
  double alpha;
  double beta;
  double i;
  double l;
  double y;
  double u;
  double e;
  double Ca;
  double psi_i;
};

struct soil_balence{
  std::vector<double> C_p;
  std::vector<double> C_f;
  std::vector<double> C_df;
  std::vector<double> C_ds;
  std::vector<double> N;
  std::vector<double> C_s;
};

// Vector to symphony_parameters
symphony_parameters vector_to_symphony(std::vector<double> input) {
  symphony_parameters params;
  
  params.A = input[0];
  params.s = input[1];
  params.r = input[2];
  params.m_p = input[3];
  params.e_p = input[4];
  params.r_p = input[5];
  params.k = input[6];
  params.alpha = input[7];
  params.beta = input[8];
  params.i = input[9];
  params.l = input[10];
  params.y = input[11];
  params.u = input[12];
  params.e = input[13];
  params.Ca = input[14];
  params.psi_i = input[15];
  
  return(params);
}


// [[Rcpp::export]]
Rcpp::List symphony(std::vector<double> params, std::vector<double> Photosynthesis) {
  // Parameters into symphony_parameters format
  symphony_parameters symphony_params = vector_to_symphony(params);
  
  // Make vectors
  std::vector<double> psi_ph;
  std::vector<double> psi_up;
  std::vector<double> psi_ims;
  std::vector<double> psi_imf;
  std::vector<double> psi_d;
  std::vector<double> psi_f;
  std::vector<double> C_p;
  std::vector<double> C_ds;
  std::vector<double> C_df;
  std::vector<double> C_f;
  std::vector<double> N;
  std::vector<double> C_s;
  
  double psi_ph_min;
  double psi_d_min;
  double psi_f_min;
  
  // Initialisation with the observed values of the paper
  C_p.push_back(525);
  C_f.push_back(635);
  N.push_back(2.09);
  // PRELES ultermate input
  psi_ph = Photosynthesis;
  // Initialized with 1 to test the code
  psi_up.push_back(1);
  psi_ims.push_back(1);
  psi_imf.push_back(1);
  psi_f.push_back(1);
  psi_d.push_back(1);
  C_ds.push_back(1);
  C_df.push_back(1);
  C_s.push_back(1);
  
  // alpha and beta should maybe be reset each year, so I need to add them!
  
  // TODO: add the initial conditions and decide which process should happen first
  
  for (int day = 0; day <= 364; day++) { // Indexes start from the 2nd day of the year
    // N in
    psi_up.push_back(symphony_params.beta * (psi_ph[day] - symphony_params.r_p*C_p[day]));
    // N immobilisation, mineralisation
    psi_ims.push_back(symphony_params.alpha * symphony_params.r * C_ds[day] + (symphony_params.beta - symphony_params.alpha)*psi_d[day]);
    // N immobilisation,
    psi_imf.push_back(symphony_params.alpha * symphony_params.r * C_df[day] + (symphony_params.beta - symphony_params.alpha)*psi_f[day]);
    // N decomposition flux
    psi_d_min = std::min(symphony_params.y*C_f[day], (symphony_params.i*N[day] + symphony_params.alpha * symphony_params.r * C_ds[day])/(symphony_params.alpha - symphony_params.beta));
    psi_d.push_back(psi_d_min);
    psi_f_min = std::min(symphony_params.u*C_f[day], (symphony_params.i*N[day] + symphony_params.alpha * symphony_params.r * C_df[day])/(symphony_params.alpha - symphony_params.beta));
    psi_f.push_back(psi_d_min);
    
    C_p.push_back(C_p[day] + psi_ph[day] - C_p[day]*(symphony_params.r_p + symphony_params.m_p + symphony_params.e_p));
    C_f.push_back(C_f[day] + symphony_params.m_p * C_p[day] - psi_d[day] - psi_f[day]);
    C_df.push_back(C_df[day] + psi_f[day] - (symphony_params.s + symphony_params.r) * C_df[day]);
    C_ds.push_back(C_ds[day] + (symphony_params.A - symphony_params.s - symphony_params.r)*C_ds[day] + psi_d[day]);
    N.push_back(N[day] + symphony_params.psi_i - symphony_params.l*N[day] - psi_up[day] + psi_ims[day] + psi_imf[day]);
    C_s.push_back(C_s[day] + (symphony_params.s - symphony_params.A) * C_ds[day]);
  }
  
  // Output
  return Rcpp::List::create(Rcpp::_["C_df"] = C_df,
                            Rcpp::_["C_ds"] = C_ds,
                            Rcpp::_["C_p"] = C_p,
                            Rcpp::_["C_f"] = C_f,
                            Rcpp::_["N"] = N,
                            Rcpp::_["C_s"] = C_s);
}


/*** R
symphony_parameters = c(0.0317917, # A
                        0.016906, # s
                        0.0368857, # r
                        0.00505757, # m_p
                        3.98799e-4, # e_p
                        0.00369772, # r_p
                        0.0121216, # k
                        0.0909091, # alpha
                        0.0142857, # beta
                        0.0110068, # i
                        0.00262647, # l
                        4.22868e-4, # y
                        0.00929094, # u
                        0.0289652, # e
                        400, # Ca
                        0.0627704) # psi i

Photosynthesis = sin(seq(0, pi, by = pi/365))

symphony(symphony_parameters, Photosynthesis)


*/
