% copy and paste to run this mod:
% dynare simulation_us.mod

% This mod simulates estimated US model.

%--------------------------------------------------
% define variables and parameters
%--------------------------------------------------

var R H L C X W infl ii iit Y N zeta
    gamma chi bb lambda g;

varexo eps_a eps_chi eps_bb eps_lambda eps_g eps_ii;

parameters
    betta varphi nu sigma phi_p phi_pi phi_y H_ss L_ss infl_ss
    rho_a rho_chi rho_bb rho_lambda rho_g rho_ii
    gamma_bar lambda_bar g_bar
    ;

% basic structual parameters
betta           = 0.99;
varphi          = 4.6841;
nu              = 17.7031;
sigma           = 3.9798;

phi_p           = 56.6938;
phi_pi          = 1.8992;
phi_y           = 0.4549;

% 6 shock AR(1) parameters
rho_a           = 0.5;
rho_chi         = 0.5;
rho_bb          = 0.5;
rho_lambda      = 0.5;
rho_g           = 0.5;
rho_ii          = 0.8040;

% initial ss values (from data or pre-set)
H_ss            = 1-0.05693602694;
L_ss            = 0.6518350168;
infl_ss         = 1+0.00458718283;
%infl_ss         =1.02;

gamma_bar       = 1.0035;
g_bar           = 1/0.85;
lambda_bar      = 0.1669;

%kaochuan
%phi_p * (exp(infl)/exp(steady_state(infl))-1)^2/2

%kaochuan'
%phi_p * (exp(infl) / exp(steady_state(infl)) -1) / exp(steady_state(infl))



%--------------------------------------------------
% model equations
%--------------------------------------------------
model;

% define parameter dependencies
#delta = ( 1 / ( 
            varphi * H_ss * ( L_ss*H_ss/g_bar )^(-nu) * (1-lambda_bar) / ( (1+varphi) * (1-L_ss)^(-nu) ) +1    
            )
          );
#chi_bar = ( delta * ( L_ss*H_ss/g_bar )^(1-nu) + (1-delta) * (1-L_ss)^(1-nu) )^((nu-sigma)/(1-nu)) *delta *( L_ss*H_ss/g_bar )^(-nu) * (1-lambda_bar) / ( H_ss^varphi);

% 1. Euler equation
    1 = betta * exp(bb(+1)) * exp(X(+1))^(nu-sigma) * exp(C(+1))^(-nu) * exp(R) / ( exp(bb) * exp(X)^(nu-sigma) * exp(C)^(-nu) * exp(gamma(+1)) );

% 2. opt. labor participation
    exp(chi) * exp(H)^(1+varphi) / (1+varphi) = delta * exp(X)^(nu-sigma) * exp(C)^(-nu) * exp(W) * exp(H) - (1-delta) * exp(X)^(nu-sigma) * (1-exp(L))^(-nu);

% 3. opt employment threshold
    delta * exp(X)^(nu-sigma) * exp(C)^(-nu) * exp(W) = exp(chi) * exp(H)^varphi;

% 4. Define consumption bundle X
    exp(X) = ( delta * exp(C)^(1-nu) + (1-delta) * (1-exp(L))^(1-nu) )^(1/(1-nu));

% 5. production function
    exp(Y) = exp(N);

% 6. NKPC
    (1-1/exp(lambda)) * ( 1 - (phi_p * (exp(infl)/exp(steady_state(infl))-1)^2)/2 ) - exp(infl) * phi_p * (exp(infl) / exp(steady_state(infl)) -1) / exp(steady_state(infl)) + exp(W)/exp(lambda)
    + betta * ( phi_p * (exp(infl(+1)) / exp(steady_state(infl)) -1) / exp(steady_state(infl)) )
    * exp(bb(+1)) * exp(X(+1))^(nu-sigma) * exp(C(+1))^(-nu) * exp(infl(+1)) * exp(Y(+1)) / ( exp(bb) * exp(X)^(nu-sigma) * exp(C)^(-nu)  * exp(Y) );

% 7. labor equilibrium
    exp(N) = exp(L) * exp(H);

% 8. Aggregate resource:
    exp(C) + exp(zeta) * exp(Y) = ( 1 - (phi_p * (exp(infl)/exp(steady_state(infl))-1)^2)/2 ) * exp(Y);

% 9. MP feedback
    exp(ii) = exp(iit)^(1-rho_ii) * exp(ii(-1))^rho_ii *exp(eps_ii);

% 10. Taylor rule
    exp(iit) = exp(steady_state(ii)) * ( exp(infl) / exp(steady_state(infl)) )^phi_pi * ( exp(gamma) * exp(Y) / ( exp(steady_state(gamma)) * exp(Y(-1)) ) )^phi_y;

% 11. link nominal and real rates
    exp(ii) = exp(R) * exp(infl(+1));

% 12. Define zeta
    exp(zeta) = 1 - 1/exp(g);

%-------------------------------------------------------------------
% shocks------------------------------------------------------------
%-------------------------------------------------------------------
% 1. productivity AR(1) (gamma)
    gamma = (1-rho_a)*log(gamma_bar) + rho_a*gamma(-1) + eps_a;

% 2. Define labor supply shock (chi)
    chi = (1-rho_chi) * log(chi_bar) + rho_chi * chi(-1) + eps_chi;

% 3. Define preference shock (db)
    bb =  rho_bb * bb(-1) + eps_bb;

% 4. Define price markup shock (epsilon)
    lambda = (1-rho_lambda) * log(lambda_bar) + rho_lambda * lambda(-1) + eps_lambda;

% 5. Define gov spending shock (g)
    g = (1-rho_g) * log(g_bar) + rho_g * g(-1) + eps_g;


end;

steady_state_model;
% from data average:
H = log(H_ss);
L = log(L_ss);
infl = log(infl_ss);

gamma = log(gamma_bar);
R = log( exp(gamma)/betta );
ii = log( exp(R) * exp(infl) );
iit = ii;
N = log( exp(H)*exp(L) );
Y = N;
zeta = log ( 1 - 1/g_bar );
C = log( (1-exp(zeta))*exp(Y));
W = log ( 1-lambda_bar );
X = log (( 1 / ( varphi * H_ss * ( 0.85*L_ss*H_ss )^(-nu) * (1-lambda_bar) / ( (1+varphi) * (1-L_ss)^(-nu) ) +1    ) * exp(C)^(1-nu) + (1-1 / ( varphi * H_ss * ( 0.85*L_ss*H_ss )^(-nu) * (1-lambda_bar) / ( (1+varphi) * (1-L_ss)^(-nu) ) +1    )) * (1-exp(L))^(1-nu) )^(1/(1-nu)));

chi = log( (( 1 / ( varphi * H_ss * ( 0.85*L_ss*H_ss )^(-nu) * (1-lambda_bar) / ( (1+varphi) * (1-L_ss)^(-nu) ) +1    )) * ( 0.85*L_ss*H_ss )^(1-nu) + (1-( 1 / ( varphi * H_ss * ( 0.85*L_ss*H_ss )^(-nu) * (1-lambda_bar) / ( (1+varphi) * (1-L_ss)^(-nu) ) +1    ))) * (1-L_ss)^(1-nu) )^((nu-sigma)/(1-nu)) *( 1 / ( varphi * H_ss * ( 0.85*L_ss*H_ss )^(-nu) * (1-lambda_bar) / ( (1+varphi) * (1-L_ss)^(-nu) ) +1    )) *( 0.85*L_ss*H_ss )^(-nu) * (1-lambda_bar) / ( H_ss^varphi) );
bb  = 0;
lambda = log(lambda_bar);
g = log(g_bar);
end;

steady;
resid;

shocks;
var eps_ii;
stderr 0.5;
var eps_a;
stderr 0;
var eps_chi;
stderr 0;
var eps_bb;
stderr 0;
var eps_lambda;
stderr 0;
var eps_g;
stderr 0;

end;

stoch_simul(periods=2000,irf=16,order=1) L H Y W R infl ii N;











