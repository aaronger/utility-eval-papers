# Allocation scoring rules as a generalization of quantile scoring rules

A quantile scoring function elicits the optimal point forecast $Q$ (= order/provision/stocking choice) for a scalar quantity $y$ representing a demand under incremental cost for exceeding $y$ and loss for failing to meet $y$:

$$
s(Q,y) = C(g(Q) - g(y))_+ + L(g(y)-g(Q))_+
$$

That is, if a forecaster believes that $y \sim F$ with density $f$, then their optimal point forecast is given by minimizing 

$$
Z = E_F[s(Q,y)]= C\int_{0}^Q(g(Q) - g(y))f(y)dy + L\int_Q^{\infty}(g(y) - g(Q))f(y)dy \\
$$

which is attained at the quantile $Q = F^{-1}\left(\frac{L}{C+L}\right)$ as a solution to the first order equation

$$
\frac{dZ}{dQ} = g'(Q)\left(CF(Q) - L(1-F(Q)) \right)  = 0
$$

(For $Q$ to be a minimum we also need $g'(Q)>0$, so that $\frac{d^2Z}{dQ^2}>0$... see Saerens, 2000.)

An allocation scoring function elicits a forecast for a vector quantity where each outcome $y_i$ has a cost and loss and there is a constraint $K\geq \sum w_i Q_i$ on the total provision available. The objective function is now

$$
Z= \sum Z_i = \sum\left\{C_i\int_{0}^{Q_i}(g_i(Q_i) - g_i(y))f_i(y)dy + 
L_i\int_{Q_i}^{\infty}(g_i(y) - g_i(Q_i))f_i(y)dy \right\}.
$$

Scaling each $g_i$ by $L_i^{-1}$ (both here and in the scoring rule below) we can assume $L_i = 1$ during optimization and that the $C_i$ are cost-loss ratios.

If the decoupled $Z_i$ optimizers $Q_i = F_i^{-1}((1+C_i)^{-1})$ satisfy the constraint then they also give a solution to the constrained $Z$ optimization problem.  If they do not we then use a Lagrange multiplier $\lambda$ to get first order equations

$$
\frac{dZ}{dQ_i}=C_iF_i(Q_i) - (1-F_i(Q_i)) + \lambda w_i = 0  
$$

with solutions

$$
Q_i = F_i^{-1}\left( \frac{1 - w_i \lambda}{1+C_i}\right).
$$

We then have to solve (in general numerically) for $\lambda$ :

$$
\sum w_i F_i^{-1}\left( \frac{1 - w_i \lambda}{1+C_i}\right) - K = 0
$$

The resulting $Q_i= Q_i(K, C_i)$ form the Bayes act for the the forecast $\{f_i\}$ and the realized losses and costs defines the scoring rule evaluated on $\{f_i\}$:

$$
\begin{aligned}
S(\mathbf{f},\mathbf{y}) = 
s(\mathbf{Q}, \mathbf{y})&=\sum C_i(g_i(Q_i) - g_i(y_i))_+ + (g_i(y_i)-g_i(Q_i))_+
\\
&= \sum(1-(1+C)\mathbf{1}\{Q_i>y_i\})(g_i(y_i) -g_i(Q_i)
\end{aligned}
$$

Remarks:

- Quantile rules allow you to vary cost ratios while allocation rules add capacity as a parameter.

- While quantile rules are undefined for zero-overage costs, allocation rules are defined when a sufficiently tight capacity constraint is imposed.

#### Special Case: $F_i$ from same location-scale family and $w_i = 1$, $C_i = C$.

Also assume the constraint is active, i.e., 

$$
K < \sum F_i^{-1}((1+C)^{-1})= \sum \mu_i + \sigma_i\Phi^{-1}((1+C)^{-1})
$$

where $\Phi$ is the standard CDF for the family. Then

$$
\frac{1 - \lambda}{1+C} = 
\Phi\left(\frac{K - \sum \mu_j}{\sum \sigma_j} \right):= F(K)
$$

and the Bayes act when the constraint is set to $K$ is 

$$
\begin{aligned} 
 Q_i(K) = F_i^{-1}(F(K)) &= \mu_i +\sigma_i\Phi^{-1}(F(K)) \\
&= \mu_i + \sigma_i\left(\frac{K - \sum \mu_j}{\sum \sigma_j} \right) \\
 &= \mu_i + \tilde{\sigma}_i(K - \sum \mu_j)
 \end{aligned}

$$

where $\tilde{\sigma_i}$ is the proportion of $F$'s scale (e.g. SD) "due" to $F_i$. 

Remarks:

- $Q_i(K)$ here does not depend on $C$ unlike when the constraint is inactive or the $C_i$ differ.

- An "excess or shortage in mean" $K - \sum \mu_j$ is divided among the components in proportion to their scale factors as adjustmments up or down from their locations $\mu_i$.  This suggests an interpretation of under-dispersion of a forecast in one component as leading to that component not receiving as much additional recources as it should when there is an excess or other components not recieving as much of scarce resources as they should when there is a shortage.

The score for the forecast $\{F_i\}$ is

$$
\begin{aligned}
s_K(\mathbf{Q}, \mathbf{y})&=
 \sum(1-(1+C)\mathbf{1}
\left\{\frac{K - \sum \mu_i}{\sum \sigma_i}> 
\frac{y_i - \mu_i}{\sigma_i}\right\})
(g_i(y_i) -g_i(Q_i(K))) 
\end{aligned}
$$

In the motivating case that $C=0$, this becomes

$$
s_K(\mathbf{Q}, \mathbf{y})=
 \sum \mathbf{1}
\left\{\frac{K - \sum \mu_i}{\sum \sigma_i}\leq 
\frac{y_i - \mu_i}{\sigma_i}\right\}
(g_i(y_i) -g_i(Q_i(K))) 
$$

That is, a $Q_i$ is only penalized when the standardized observed excess demand in component $i$ exceeds the standardized excess in resources or the standardized observed shortfall in demand is not as large as the standardized shortage of resources.
