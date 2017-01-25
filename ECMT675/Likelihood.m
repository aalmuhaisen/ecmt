function likelihood = Likelihood(theta, X, y)
%%% The likelihood function %%%
likelihood = -sum(log(normcdf((2*y-1).*(X*theta))));
end