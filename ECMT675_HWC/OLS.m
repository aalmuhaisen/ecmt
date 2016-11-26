function C = OLS(theta, X, y)
%%% The OLS function %%%
C = sum(((X*theta)-y).^2)/(2*length(y));
end
