% copy and paste to run this mod:
% dynare estimation_rotembergversion_japan.mod
 
% this is a detrended, rotemberg setting model

%--------------------------------------------------
% define variables and parameters
%--------------------------------------------------

var R H L C X W infl ii iit Y N zeta
    gamma chi bb lambda g
    Y_obs C_obs infl_obs ii_obs L_obs H_obs;

varexo eps_a eps_chi eps_bb eps_lambda eps_g eps_ii;

parameters
    betta varphi nu sigma phi_p phi_pi phi_y H_ss L_ss infl_ss
    rho_a rho_chi rho_bb rho_lambda rho_g rho_ii
    gamma_bar lambda_bar g_bar
    ;

% basic structual parameters
betta           = 0.99;
varphi          = 5;
nu              = 20;
sigma           = 2;

phi_p           = 30;
phi_pi          = 1.5;
phi_y           = 0.2;

% 6 shock AR(1) parameters
rho_a           = 0.5;
rho_chi         = 0.5;
rho_bb          = 0.5;
rho_lambda      = 0.5;
rho_g           = 0.5;
rho_ii          = 0.5;

% initial ss values (from data or pre-set)
L_ss            =   0.610906147;        % steady state labor participation
H_ss            =   1-0.04035;    % this is not a solution but a guess
infl_ss         =   1-0.001177538; % from data
%infl_ss         =1.02;

gamma_bar       = 1;
g_bar           = 1/0.85;
lambda_bar      = 0.1;

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

%-------------------------------------------------------------------
% Measurement equations---------------------------------------------
%-------------------------------------------------------------------
% 1. gdp （all logged)
Y_obs = Y - Y(-1) + gamma;

% 2. real consumption （all logged)
C_obs = C - C(-1) + gamma;

% 3. inflation
infl_obs = infl;

% 4. interest rate
ii_obs = ii;

% 5. LFPR
L_obs = L;

% 6. employment
H_obs = H;

end;

%-------------------------------------------------------------------
%-------------------------------------------------------------------
%-------------------------------------------------------------------
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

Y_obs = gamma;
C_obs = gamma;
infl_obs = infl;
ii_obs = ii;
L_obs = L;
H_obs = H;

end;

steady;
resid;

shocks;
var eps_ii;
stderr 0.005;
var eps_a;
stderr 0.005;
var eps_chi;
stderr 0.005;
var eps_bb;
stderr 0.005;
var eps_lambda;
stderr 0.005;
var eps_g;
stderr 0.005;

end;

estimated_params;
% para_name, initval, LB, UP, prior shape, para1, para2;
% prior shape: BETA_PDF, INV_GAMMA_PDF, GAMMA_PDF

%sigma_a, 0.005, , , INV_GAMMA_PDF, 0.005, 0.02;
%sigma_ii, 0.005, , , INV_GAMMA_PDF, 0.005, 0.02;
%sigma_chi, 0.005, , , INV_GAMMA_PDF, 0.005, 0.02;
%sigma_bb, 0.005, , , INV_GAMMA_PDF, 0.005, 0.02;
%sigma_epsilon, 0.005, , , INV_GAMMA_PDF, 0.005, 0.02;
%sigma_g, 0.005, , , INV_GAMMA_PDF, 0.005, 0.02;

stderr eps_ii, 0.005, 0.0001, 5, INV_GAMMA_PDF, 0.005, 2;
stderr eps_a, 0.005, 0.0001, 5, INV_GAMMA_PDF, 0.005, 2;
stderr eps_chi, 0.005, 0.0001, 5, INV_GAMMA_PDF, 0.005, 2;
stderr eps_bb, 0.005, 0.0001, 5, INV_GAMMA_PDF, 0.005, 2;
stderr eps_lambda, 0.005, 0.0001, 5, INV_GAMMA_PDF, 0.005, 2;
stderr eps_g, 0.005, 0.0001, 5, INV_GAMMA_PDF, 0.005, 2;

rho_a, 0.5, 0.0001, 0.9999, BETA_PDF, 0.5, 0.20;
rho_ii, 0.5, 0.0001, 0.9999, BETA_PDF, 0.5, 0.20;
rho_chi, 0.5, 0.0001, 0.9999, BETA_PDF, 0.5, 0.20;
rho_bb, 0.5, 0.0001, 0.9999, BETA_PDF, 0.5, 0.20;
rho_lambda, 0.5, 0.0001, 0.9999, BETA_PDF, 0.5, 0.20;
rho_g, 0.5, 0.0001, 0.9999, BETA_PDF, 0.5, 0.20;

varphi, 5, 0.0001, 100, GAMMA_PDF, 5, 2.5;
nu, 20, 0.0001, 100, GAMMA_PDF, 20, 2.5;
sigma, 2, 0.0001, 100, GAMMA_PDF, 2, 1;

phi_p, 30, 0.001, 200, GAMMA_PDF, 30, 15;
phi_pi, 1.5, 0.0001, 10, GAMMA_PDF, 1.5, 0.25;
phi_y, 0.2, 0.0001, 3, GAMMA_PDF, 0.2, 0.1;

gamma_bar, 1, , , GAMMA_PDF, 1, 0.5;
lambda_bar, 0.1, , , BETA_PDF, 0.1, 0.05;

end;


varobs Y_obs C_obs infl_obs ii_obs L_obs H_obs; % 

estimation(datafile='japan_obs_file_2.csv',            % Specify the data file
           mode_compute=6,              % Specify the optimizer (e.g., 6 for CMA-ES, or 4 for Chris Sims' optimizer)
           mh_replic=40000,             % Number of MH draws (set to 0 if you don’t want to run MH yet)
           mh_nblocks=2,                % Number of MH blocks
           mh_drop=0.2,                 % Fraction of MH draws to discard
           mh_jscale=0.2,               % Scaling factor for MH step size
           first_obs=1,                 % First observation to use in estimation
           presample=4,                 % Number of presample periods to use (e.g., for initialization)
           lik_init=2,                  % Initialization method for the likelihood function (1: exact initial conditions, 2: diffuse prior)
           prefilter=0,                 % Set to 1 if you want to detrend your data before estimation
           bayesian_irf,                % Generate Bayesian impulse response functions
           irf = 12,
           irf_shocks=(eps_ii),
           graph_format=(pdf),
           %nograph,                     % Suppress Dynare's graphs
           nodiagnostic,                % Suppress diagnostics during estimation
           tex)
           Y C N H L W R ii infl;                        % Generate LaTeX output (e.g., tables for posterior distributions)








