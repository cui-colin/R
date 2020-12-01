library(animation)

Nsim = 100
current_q = 0
L = 5
epsilon = 0.3
U = function(q)
{
	return(q^2/2)
}

samples <- matrix(nrow=Nsim, ncol=2)
oopt = ani.options(interval = 0.01, nmax = 1000)


saveGIF({plot(-4:4, -4:4, type = "n")
		for (i in 1:Nsim) {
		p = rnorm(length(q),0,1) #independent standard normal variates
		q = current_q
		current_p = p
		# Make a half step for momentum at the beginning
		steps = matrix(nrow=L, ncol=2)
		for (m in 1:L)
		{
			p = p - epsilon / 2 * (q) 
			q = q + epsilon *p		
			p = p - epsilon / 2 *(q)
		}
		# Negate momentum at the end of trajectory to make the proposal symmetric
		p = -p
		# Evaluate potential and kinetic energies at start and end of trajectory
		current_U = U(current_q)
		current_K = sum(current_p^2) / 2
		proposed_U = U(q)
		proposed_K = sum(p^2) / 2
		# Accept or reject the state at the end of trajectory, return either
		# the position at the end of the trajectory or the initial position
		if (runif(1) < exp(current_U-proposed_U+current_K-proposed_K))
		{
			q  #accept
			current_q <- q
		}else{
			current_q  # reject
		}		
	samples[i,] <- c(current_q, p)
	points(samples[i,1], samples[i,2], pch=20)
		if(i!=1)
		{
			lines(samples[(i-1):i,])
		}
	ani.pause() ## pause for a while (’interval’)
	}
}, movie.name = "HMC_Sim.gif",  interval = 10, ani.width = 600, ani.height = 600
)
#end running code

