%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                  Econometrics I - Homework 2                       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Clear everything
clear; clc

%%% Set global seed %%%
s = RandStream('mt19937ar','Seed',1991);

%%% Import Data %%%
[mydata, myheader] = xlsread('MROZ.xls');

%%% Assign each column to the appropriate variable %%%
for i = 1:length(myheader)
      commandExec = [myheader{i}, ' = ', 'mydata(:,', num2str(i) , ');'];
      evalin('base', commandExec );
end

%%% Create the variables lists %%%
z0 = [nwifeinc exper expersq age kidslt6 kidsge6];
z1 = [nwifeinc educ exper expersq age kidslt6 kidsge6];
z2 = [motheduc fatheduc huseduc];
const = ones(size(z1,1), 1);

%%% Create initial parameters %%%
init_delta21 = zeros(size(z1, 2), 1); 
init_delta22 = zeros(size(z2, 2), 1);
init_delta11 = zeros(size(z1, 2), 1);
init_delta12 = zeros(size(z0, 2), 1);
init_alpha11 = zeros(size(1, 2), 1);
init_beta11 = zeros(size(1, 2), 1);

%%%%%  Question 1: Estimate the model using the simple probit model  %%%%%
%%% Prepare data %%%
y = inlf;
X = [const z1 z2];
Varb = {'const'; 'nwifeinc'; 'educ'; 'exper'; 'expersq'; 'age';...
    'kidslt6'; 'kidsge6'; 'motheduc'; 'fatheduc'; 'huseduc'};
init_theta = [init_beta11' init_delta21' init_delta22']';
y = (y > mean(y));

%%% Set optimization options then optimize %%%
options = optimset('MaxIter', 10000,'MaxFunEvals', 10000,'Display', 'off');
options.LargeScale = 'off';
[estimator, likelihood]=fminunc(@(theta)(Likelihood(theta,X,y)),init_theta...
    , options);

%%% Calculate ME at the mean %%%
X_bar = mean(X,1);
ME1 = pdf('normal',(X_bar*estimator),0,1) * estimator;
ME1(1) = NaN;

%%% Standard Error using bootstrap %%%
nsamples = 300;
bootlist = zeros(11,nsamples);
for i=1:nsamples
    index = randsample(s, 1:length(X), size(X,1), true);
    X_boot = X(index,:);
    y_boot = y(index,:);
    options = optimset('MaxIter', 10000,'MaxFunEvals', 10000,'Display',...
        'off');
    options.LargeScale = 'off';
    [boot_estimator, boot_likelihood]=fminunc(@(theta)(Likelihood(theta,X_boot,...
        y_boot)),init_theta, options);
    bootlist(:,i) = boot_estimator;
end;
std_boot = std(bootlist.').';

%%% Display data in a table %%%
coef = round(estimator, 4);
std_bootstrap = round(std_boot, 4);
t_statistics = coef./std_bootstrap;
ME_Mean = round(ME1, 4);
Regressor = Varb;
Regression_Restults_Q1 = table(Regressor, coef, std_bootstrap,...
    t_statistics, ME_Mean);
disp('Answers for Q1: ')
disp('Results for a simple Probit: ')
disp(' ')
disp(Regression_Restults_Q1)
disp(['Log likelihood: ',num2str(-likelihood)])
disp(' ')
y112 = X*estimator; y12 = y112; y12(y12>0)=1; y12(y12<=0)=0; %Prediction
disp(['Percent Correctly Predicted: ',num2str(round(100*mean(y==y12),2))])
disp(' ')

%%%%%                 Q2: Control Function Approach                 %%%%%%
%%%% Regress all variables on educ and save the residuals %%%%
%%% Prepare the data %%%
X1 = [const z0 z2];
Varb1 = {'const'; 'nwifeinc'; 'exper'; 'expersq'; 'age'; 'kidslt6';...
    'kidsge6'; 'motheduc'; 'fatheduc'; 'huseduc'};
init_theta1 = [init_beta11' init_delta12' init_delta22']';

%%% Set optimization options then optimize and save the residuals %%%
options = optimset('MaxIter', 20000,'MaxFunEvals', 20000,'Display', 'off');
options.LargeScale = 'off';
[estimator11]=fminunc(@(theta)(OLS(theta,X1,educ)),init_theta1, options);
Res2 = educ - (X1*estimator11);

%%% Standard Error using bootstrap %%%
nsamples = 300;
bootlist1 = zeros(10,nsamples);
for i=1:nsamples
    index1 = randsample(s, 1:length(X1), size(X1,1), true);
    X_boot1 = X1(index1,:);
    educ_boot = educ(index1,:);
    options = optimset('MaxIter', 10000,'MaxFunEvals', 10000,'Display',...
        'off');
    options.LargeScale = 'off';
    [boot_estimator21]=fminunc(@(theta)(OLS(theta,X_boot1,educ_boot)),...
        init_theta1, options);
    bootlist1(:,i) = boot_estimator21;
end;
std_boot21 = std(bootlist1.').';

%%% Display data in a table %%%
coef = round(estimator11, 4);
std_bootstrap = round(std_boot21,4);
t_statistics = coef./std_bootstrap;
Regressors = Varb1;
OLS_Results = table(Regressors, coef, std_bootstrap, t_statistics);
disp(' ')
disp('Answers for Q2: ')
disp('Results of Regressing all variables on educ: ')
disp(' ')
disp(OLS_Results)

%%%% Use Probit to regress all data plus the OLS residual on Y %%%
%%% Prepare the data %%%
X2 = [const z1 Res2];
Varb2 = {'const'; 'nwifeinc'; 'educ'; 'exper'; 'expersq'; 'age';...
    'kidslt6'; 'kidsge6'; 'OLS'};
init_theta2 = [init_beta11' init_delta11' init_alpha11]';
Res2 = (Res2 > mean(Res2));

%%% Set optiomization options then optimize %%%
options = optimset('MaxIter', 20000,'MaxFunEvals', 20000,'Display', 'off');
options.LargeScale = 'off';
[estimator2, likelihood2]=fminunc(@(theta)(Likelihood(theta,X2,y)),...
    init_theta2, options);

%%% Calculate ME at the mean %%%
X_bar2 = mean(X2,1);
ME2 = pdf('normal',(X_bar2*estimator2),0,1) * estimator2;
ME2(1) = NaN;

%%% Standard Error Using bootstrap %%%
nsamples = 300;
bootlist22 = zeros(9,nsamples);
for i=1:nsamples
    index2 = randsample(s, 1:length(X2), size(X2,1), true);
    X_boot2 = X2(index2,:);
    y_boot2 = y(index2,:);
    options = optimset('MaxIter', 10000,'MaxFunEvals', 10000,'Display',...
        'off');
    options.LargeScale = 'off';
    [boot_estimator22, boot_likelihood22]=fminunc(@(theta)(Likelihood(theta,...
        X_boot2, y_boot2)),init_theta2, options);
    bootlist22(:,i) = boot_estimator22;
end;
std_boot22 = std(bootlist22.').';

%%% Display data in table %%%
coef = round(estimator2, 4);
std_bootstrap = round(std_boot22, 4);
t_statistics = coef./std_bootstrap;
ME_Mean = round(ME2, 4);
Regressor = Varb2;
Regression_Restults_Q2 = table(Regressor, coef, std_bootstrap,...
    t_statistics, ME_Mean);
disp('Results for regressing all variables plus the residuals on Y: ')
disp(' ')
disp(Regression_Restults_Q2)
disp(['Log likelihood: ',num2str(-likelihood2)])
disp(' ')
y11 = X2*estimator2;
y1 = y11;
y1(y1>0)=1; y1(y1<=0)=0;
disp(['Percent Correctly Predicted: ',num2str(round(100*mean(y==y1),2))])
disp(' ')
disp('The t-statistic for the residual of the regression of all variables')
disp('on educ is lower than the significance level. Therefore, we reject')
disp('the hypothesis that educ is endogenous.')
disp(' ')


%%%%%           IV Approach  (OLS with adjusted coefficients)         %%%%%
%%% Prepare the data %%%
X2 = [const z1 Res2]; %First stage already done in Q2
Varb2 = {'const'; 'nwifeinc'; 'educ'; 'exper'; 'expersq'; 'age';...
    'kidslt6'; 'kidsge6'; 'OLS'};
init_theta2 = [init_beta11' init_delta11' init_alpha11]';


%%% Set optimization options then optimize %%%
options = optimset('MaxIter', 20000,'MaxFunEvals', 20000,'Display', 'off');
options.LargeScale = 'off';
[estimator3]=fminunc(@(theta)(OLS(theta,X2,y)),init_theta2, options);

%%% Standard Error using bootstrap %%%
nsamples = 300;
bootlist3 = zeros(9,nsamples);
for i=1:nsamples
    index3 = randsample(s, 1:length(X2), size(X2,1), true);
    X_boot3 = X2(index3,:);
    y_boot3 = y(index3,:);
    options = optimset('MaxIter', 10000,'MaxFunEvals', 10000,'Display',...
        'off');
    options.LargeScale = 'off';
    [boot_estimator3]=fminunc(@(theta)(OLS(theta,X_boot3,y_boot3)),...
        init_theta2, options);
    %Adjust the estimators by multipliying by 2.5
    bootlist3(:,i) = boot_estimator3*2.5;
end;
std_boot3 = std(bootlist3.').';

%%% Display data in a table after adjusting the coef%%%
coef = round(estimator3, 4)*2.5;
std_bootstrap = round(std_boot3,4);
t_statistics = coef./std_bootstrap;
Regressors = Varb2;
OLS_Results = table(Regressors, coef, std_bootstrap, t_statistics);
disp('Answers for Q3: ')
disp('Results of regressing all variables plus the OLS residuals on Y: ')
disp(' ')
disp(OLS_Results)
disp(' ')
disp('Similar to the case of CF approach, the t-statistics for the')
disp('residual of the regression of all variables on educ is')
disp('lower than the significance level. Therefore, we reject')
disp('the hypothesis that educ is endogenous.')